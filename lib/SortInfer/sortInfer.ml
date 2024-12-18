open Misc
open Vars
open Types
open Prelude
open Preprocess_ast
open Format


type sort_check_env = {
  prelude : prelude;
  varsorts : USortVar.t Var.Env.t;
  covarsorts : USortVar.t CoVar.Env.t;
  tyvarsorts : usort TyVar.Env.t;
  unifier : usort USortVar.Env.t;
  }

let initial_sort_check_env prelude = {
  prelude;
  varsorts = Var.Env.empty;
  covarsorts = CoVar.Env.empty;
  tyvarsorts = TyVar.Env.empty;
  unifier = USortVar.Env.empty
}

exception Sort_mismatch of string * string * position * position

let fail_polarity_mismatch upol1 upol2 pos1 pos2 =
  raise (Sort_mismatch (string_of_usort upol1, string_of_usort upol2, pos1, pos2))

exception Ambiguous_polarity of position

let fail_ambiguous_sort loc = raise (Ambiguous_polarity loc)


let pos_uso = Litt (Base Positive)

let neg_uso = Litt (Base Negative)

let idx_uso i = Litt (Index i)

let get env uso =
  USortVar.Env.find uso env.unifier

let get_opt env uso =
  USortVar.Env.find_opt uso env.unifier

let rec get_loc env polvar =
  try match get env polvar with
    | Loc (loc, _) -> loc
    | Redirect u -> get_loc env u
    | _ -> dummy_pos
  with
  | Not_found -> dummy_pos

let set uso env sovar =
    let uso =
      try match get env sovar with
        | Loc (loc, _) -> Loc (loc, uso)
        | _ -> uso
      with Not_found -> uso in
    {env with unifier = USortVar.Env.add sovar uso env.unifier}

let unify_uso env uso1 uso2 =

  let acc = ref [] in
  let add sovar = acc := sovar :: !acc in
  let finalize p = List.fold_left (set p) env !acc in

  let rec collect loc = function
    | Litt p -> loc, Some p
    | Loc (loc, v) -> collect loc v
    | Redirect v -> begin
        add v;
        match get_opt env v with
        | None -> loc, None
        | Some upol -> collect loc upol
    end in

  let loc1, so1 = collect dummy_pos uso1 in
  let loc2, so2 = collect dummy_pos uso2 in
  let loc = if loc1 <> dummy_pos then loc1 else loc2 in

  let p = match so1, so2 with
  | Some p, None
  | None, Some p -> (Litt p)
  | None, None -> (Redirect (USortVar.fresh ()))
  | Some p1, Some p2 ->
    if p1 = p2 then (Litt p1) else fail_polarity_mismatch (Litt p1) (Litt p2) loc1 loc2 in

  finalize (Loc (loc, p))


let unify_prog env prog =

  let debug = None in
  let prelude = env.prelude in
  let env = ref env in

  let rec pp_upol fmt = function
    | Litt so -> pp_sort fmt so
    | Loc (loc,u) -> fprintf fmt "loc(%s)%a" (string_of_position loc) pp_upol u
    | Redirect v -> pp_print_string fmt (USortVar.to_string v) in

  let rec unify uso1 uso2 =
    env := unify_uso !env uso1 uso2

  and unify_tyvar_sort sort tvar =
    match TyVar.Env.find_opt tvar !env.tyvarsorts with
    | Some u -> unify u sort
    | None -> env := {!env with tyvarsorts = TyVar.Env.add tvar sort !env.tyvarsorts}

  and unify_typ upol1 typ = match typ with
    | TCons {node;_} -> unify_typecons upol1 node
    | TApp {tfun;_} -> unify_typ upol1 tfun
    | TVar {node=var; _}| TInternal var ->
      try unify upol1 (TyVar.Env.find var !env.tyvarsorts)
      with
      | Not_found ->
        let pol = Redirect (USortVar.fresh ()) in
        env := {!env with tyvarsorts = TyVar.Env.add var pol !env.tyvarsorts};
        unify upol1 pol

  and unify_typecons upol1 (tcons : 'a Types.type_cons) =
    match tcons with
    | Unit | Zero | Closure | Box _ | Prod _ | Sum _ | Autopack -> unify upol1 pos_uso
    | Top | Bottom | Thunk | Fix | Choice _ | Fun _ | Autospec -> unify upol1 neg_uso
    | Cons cons ->
      let consdef = TyConsVar.Env.find cons !prelude.tycons in
      let _, ret_so = unmk_arrow consdef.sort in
      unify upol1 (Litt ret_so)

  and unify_newvar upol var loc =
    let polvar =
      try Var.Env.find var !env.varsorts
      with Not_found ->
        let polvar = USortVar.fresh () in
        env := {!env with varsorts = Var.Env.add var polvar !env.varsorts};
        polvar in
    unify upol (Loc (loc, Redirect polvar))

  and unify_bind upol (var, typ) loc =
    unify_newvar upol var loc;
    unify_typ upol typ

  and unify_newcovar upol covar loc =
    let polvar =
      try CoVar.Env.find covar !env.covarsorts
      with Not_found ->
        let polvar = USortVar.fresh () in
        env := {!env with
                covarsorts = CoVar.Env.add covar polvar !env.covarsorts
               };
        polvar in
    unify upol (Loc (loc, Redirect polvar))

  and unify_cobind upol (covar, typ) loc =
    unify_newcovar upol covar loc;
    unify_typ upol typ

  and unify_val upol valu loc =
    match valu with
    | Var v ->
      let polvar =
        try Var.Env.find v !env.varsorts
        with Not_found ->
          Misc.fail_invariant_break ~loc ("this variable is unbound" ^ Var.to_string v) in
      unify upol (Loc (loc, Redirect polvar))
    | CoTop -> unify upol (Loc (loc, neg_uso))
    | Bindcc {bind; pol; cmd} ->
      unify_cobind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd cmd
    | Box {bind; cmd; _} ->
      unify upol (Loc (loc, pos_uso));
      unify_cobind neg_uso bind loc;
      unify_cmd cmd
    | Cons cons ->
      unify_cons loc upol cons
    | Fix {stk; bind} ->
      unify_cobind neg_uso bind loc;
      unify_meta_stk pos_uso stk;
      unify upol (Loc (loc, pos_uso))
    | Destr {default; cases; for_type} ->
      unify_typecons upol for_type;
      List.iter (unify_copatt loc upol) cases;
      begin match default with
        | None -> ()
        | Some (a, cmd) -> unify_cobind upol a loc; unify_cmd cmd
      end

  and unify_stk upol loc stk =
    match stk with
    | Ret a ->
      let polvar =
        try CoVar.Env.find a !env.covarsorts
        with Not_found ->
          Misc.fail_invariant_break ~loc ("this variable is unbound" ^ CoVar.to_string a) in
      unify upol (Loc (loc, Redirect polvar))
    | CoZero -> unify upol (Loc (loc, pos_uso))
    | CoBind {bind; pol; cmd} ->
      unify_bind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd cmd
    | CoBox {stk;_} ->
      unify upol pos_uso;
      unify_meta_stk neg_uso stk
    | CoDestr destr ->
      unify_destr loc upol destr
    | CoCons {default; cases; for_type} ->
      unify_typecons upol for_type;
      List.iter (unify_patt loc upol) cases;
      begin match default with
        | None -> ()
        | Some (x, cmd) -> unify_bind pos_uso x loc; unify_cmd cmd
      end
    | CoFix stk ->
      unify upol pos_uso;
      unify_meta_stk neg_uso stk

  and unify_cons loc upol (Raw_Cons cons) =
    let so = match cons.tag with Closure  -> sort_negtype | _ -> sort_postype in
    unify upol (Loc (loc, pos_uso));
    let Consdef { constructor = Raw_Cons def; _ } = def_of_cons prelude cons.tag in
    List.iter2 (fun t (_, so) -> unify_typ (Litt so) t) cons.idxs def.idxs;
    List.iter (unify_meta_val (Loc (loc, Litt so))) cons.args

  and unify_destr loc upol (Raw_Destr destr) =
    let so = match destr.tag with Thunk -> sort_postype | _ -> sort_negtype in
    unify upol (Loc (loc, neg_uso));
    let Destrdef {destructor = Raw_Destr def; _} = def_of_destr prelude destr.tag in
    List.iter2 (fun t (_, so) -> unify_typ (Litt so) t) destr.idxs def.idxs;
    unify_meta_stk (Litt so) destr.cont;
    List.iter (unify_meta_val pos_uso) destr.args

  and unify_patt loc upol (Raw_Cons patt, cmd) =
    let so = match patt.tag with Closure -> sort_negtype | _ -> sort_postype in
    let Consdef { constructor = Raw_Cons def; _} = def_of_cons prelude patt.tag in
    let def_idxs = List.map (fun (t,so) -> (t, Litt so)) def.idxs in
    unify upol pos_uso;
    List.iter (fun bind -> unify_bind (Litt so) bind loc) patt.args;
    List.iter2 (fun (x,so) (_,so') ->
        unify_tyvar_sort so x;
        unify_tyvar_sort so' x)
      def_idxs patt.idxs;
    unify_cmd cmd

  and unify_copatt loc upol (Raw_Destr copatt, cmd) =
    let so = match copatt.tag with
      | Thunk -> pos_uso
      | _ -> neg_uso
    in
    unify upol neg_uso;
    let Destrdef {destructor = Raw_Destr def; _} = def_of_destr prelude copatt.tag in
    let def_idxs = List.map (fun (t,so) -> (t, Litt so)) def.idxs in
    List.iter (fun bind -> unify_bind pos_uso bind loc) copatt.args;
    unify_cobind so copatt.cont loc;
    List.iter2 (fun (x, so) (_, so')->
        unify_tyvar_sort so x;
        unify_tyvar_sort so' x)
      def_idxs copatt.idxs;
    (* If we don't unify the commands after the pattern, the bound variables
       won't be in scope *)
    unify_cmd cmd;

  and unify_meta_val pol (MetaVal {node; val_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "value with(%a,%a) %a@."
          pp_upol pol
          pp_typ val_typ
          pp_pre_value node;
        Format.pp_print_flush fmt ();
      | None -> ()
    end;

    unify_val pol node loc;
    unify_typ pol val_typ;

  and unify_meta_stk cont_pol (MetaStack {node; cont_typ;  loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "stack with(%a:%a) %a@."
          pp_typ cont_typ
          pp_upol cont_pol
          pp_pre_stack node;
        pp_print_flush fmt ()
      | None -> ()
    end ;
    unify_typ cont_pol cont_typ;
    unify_stk cont_pol loc node;


  and unify_cmd (Command cmd) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "command %a@." pp_cmd (Command cmd);
        (* dump_env std_formatter !env; *)
        pp_print_flush fmt ();
      | None -> ()
    end ;
    match cmd.node with
    | Interact {valu; stk; mid_typ} ->
      unify_meta_val cmd.pol valu;
      unify_meta_stk cmd.pol stk;
      unify_typ cmd.pol mid_typ
    | Trace {dump; cmd=cmd'; _} ->
      Option.iter (fun v -> unify_meta_val pos_uso v) dump;
      unify pos_uso cmd.pol;
      unify_cmd cmd'
    | Struct {valu; binds; cmd=cmd'} ->
      unify_meta_val pos_uso valu;
      unify pos_uso cmd.pol;
      List.iter (fun b -> unify_bind pos_uso b cmd.loc) binds;
      unify_cmd cmd'
    | Pack { stk; name; cmd=cmd' } ->
      unify_meta_stk pos_uso stk;
      unify_newcovar pos_uso name cmd.loc;
      unify_cmd cmd'

    | Spec { valu; name; cmd=cmd' } ->
      unify_meta_val neg_uso valu;
      unify_newvar neg_uso name cmd.loc;
      unify_cmd cmd'

  and unify_declaration item = begin match item with
    | Value_declaration item ->
      let (_, typ) = item.bind in
      unify_typ item.pol typ
    | Value_definition item ->
      let (_, typ) = item.bind in
      unify_typ item.pol typ;
      unify_meta_val item.pol item.content
  end;
    begin match item with
      | Value_declaration {bind = (name, _); pol; loc; _}
      | Value_definition {bind = (name, _); pol; loc; _} ->
        let polvar = USortVar.fresh () in
        env := {!env with varsorts = Var.Env.add name polvar !env.varsorts};
        unify pol (Loc (loc, Redirect polvar));
    end;

  and unify_execution exec = match exec with
    | Command_execution item ->
      let upol = Redirect (USortVar.fresh ()) in
      unify_cobind upol (item.cont, item.conttyp) item.loc;
      unify upol item.pol;
      unify_cmd item.content

  and unify_goal (Goal {polynomial; _}) =
    unify_typecons (Litt (sort_idx (Primitives.sort_nat))) (Cons polynomial);
  in

  List.iter unify_declaration prog.declarations;
  Option.iter unify_execution prog.command;
  Option.iter unify_goal prog.goal;
  !env
