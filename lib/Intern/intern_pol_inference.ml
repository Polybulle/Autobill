open Util
open Vars
open Types
open Intern_common
open Intern_prettyPrinter
open Format
open Ast
open InternAst


let get env polvar =
  PolVarEnv.find polvar env.unifier

let get_opt env polvar =
  PolVarEnv.find_opt polvar env.unifier

let get_loc env polvar =
  try match get env polvar with
    | Loc (loc, _) -> loc
    | _ -> dummy_pos
  with
  | Not_found -> dummy_pos

let set upol env polvar =
    let upol =
      try match get env polvar with
        | Loc (loc, _) -> Loc (loc, upol)
        | _ -> upol
      with Not_found -> upol in
    {env with unifier = PolVarEnv.add polvar upol env.unifier}

let unify_upol env upol1 upol2 =

  let acc = ref [] in
  let add polvar = acc := polvar :: !acc in
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

  let loc1, pol1 = collect dummy_pos upol1 in
  let loc2, pol2 = collect dummy_pos upol2 in

  match pol1, pol2 with
  | Some p, None
  | None, Some p -> finalize (Litt p)
  | None, None -> finalize (Redirect (PolVar.fresh ()))
  | Some p1, Some p2 when p1 = p2 -> finalize (Litt p1)
  | _ -> fail_polarity_mismatch loc1 loc2


let unify_def ?debug prelude env (Definition item) =

  let env = ref env in

  let rec pp_upol fmt = function
    | Litt p -> pp_pol fmt p
    | Loc (loc,u) -> fprintf fmt "loc(%s)%a" (string_of_position loc) pp_upol u
    | Redirect v -> pp_print_string fmt (PolVar.to_string v) in

  let rec unify upol1 upol2 =
    env := unify_upol !env upol1 upol2

  and unify_typ upol1 typ =
  let upol2 = match typ with
    | TBox _ | TPos _ ->  Litt positive
    | TNeg _ -> Litt negative
    | TCons {node;_} ->
      begin match node with
        | Unit | Zero | ShiftPos _ | Prod _ | Sum _ -> Litt positive
        | Top | Bottom | ShiftNeg _ | Fun _ | Choice _ -> Litt negative
        | Cons (cons, _) ->
          let consdef = TyConsEnv.find cons prelude.tycons in
          match consdef.ret_sort with
          | Base p -> Litt p
          | _ -> raise (Failure "FATAL type constructors has non-base type")
      end
    | TVar {node=var; _}| TInternal var ->
      try TyVarEnv.find var !env.tyvarpols
      with
      | Not_found ->
        let pol = Redirect (PolVar.fresh ()) in
        env := {!env with tyvarpols = TyVarEnv.add var pol !env.tyvarpols};
        pol
  in unify upol1 upol2

  and unify_bind upol (var, typ) loc =
    let polvar =
        try VarEnv.find var !env.varpols
        with Not_found -> fail_undefined_var (Var.to_string var) loc in
    unify upol (Loc (loc, Redirect polvar));
    unify_typ upol typ

  and unify_val upol valu loc =
    match valu with
    | Var v ->
      let polvar =
        try VarEnv.find v !env.varpols
        with Not_found -> fail_undefined_var (Var.to_string v) loc in
      unify upol (Loc (loc, Redirect polvar))
    | Bindcc {bind = (pol1, typ); pol = pol2; cmd} ->
      unify_typ upol typ;
      unify upol (Loc (loc, pol1));
      unify upol (Loc (loc, pol2));
      unify_cmd upol cmd
    | Box {bind=(pol,typ); cmd; _} ->
      unify upol (Loc (loc, Litt positive));
      unify_typ pol typ;
      unify pol (Litt positive);
      unify_cmd pol cmd
    | Cons cons ->
      unify upol (Loc (loc, Litt positive));
      unify_cons cons
    | Destr copatts ->
      unify upol (Loc (loc, Litt negative));
      let go (copatt, cmd) =
        unify_copatt copatt loc;
        unify_cmd upol cmd in
      List.iter go copatts

  and unify_stk upol final_upol loc stk =
    match stk with
    | Ret -> unify (Loc (loc, upol)) final_upol
    | CoBind {bind=(var,typ); pol; cmd} ->
      let polvar = PolVar.fresh () in
      env := {!env with varpols = VarEnv.add var polvar !env.varpols};
      unify upol (Loc (loc, pol));
      unify_typ upol typ;
      unify upol (Redirect polvar);
      unify_cmd final_upol cmd
    | CoBox {stk;_} ->
      unify upol (Litt positive);
      unify_meta_stk (Litt positive) final_upol stk
    | CoDestr destr ->
      unify upol (Loc (loc, Litt negative));
      unify_destr final_upol destr
    | CoCons patts ->
      unify upol (Loc (loc, Litt positive));
      let go (patt, cmd) =
        unify_patt patt loc;
        unify_cmd final_upol cmd in
      List.iter go patts

  and unify_cons cons =
    let args, upol = match cons with
      | Unit -> [], Litt positive
      | Left x | Right x -> [x], Litt positive
      | ShiftPos x -> [x], Litt negative
      | Pair (a,b) -> [a;b], Litt positive
      | PosCons (_, args) -> args, Litt positive in
    List.iter (unify_meta_val upol) args

  and unify_destr final_upol destr =
    let args, cont, cont_pol = match destr with
      | Call (x,a) -> [x], a, Litt negative
      | Yes a | No a -> [], a, Litt negative
      | ShiftNeg a -> [], a, Litt positive
      | NegCons (_, args, cont) -> args, cont, Litt positive in
    List.iter (unify_meta_val (Litt positive)) args;
    unify_meta_stk cont_pol final_upol cont

  and unify_patt patt loc = match patt with
    | Unit -> ()
    | Left bind | Right bind -> unify_bind (Litt positive) bind loc
    | ShiftPos bind -> unify_bind (Litt positive) bind loc
    | Pair (binda, bindb) ->
      unify_bind (Litt positive) binda loc;
      unify_bind (Litt negative) bindb loc
    | PosCons (_, args) ->
      List.iter (fun bind -> unify_bind (Litt positive) bind loc) args

  and unify_copatt copatt loc = match copatt with
    | Call (bindx, binda) ->
      unify_bind (Litt positive) bindx loc;
      unify_typ (Litt negative) binda
    | Yes binda | No binda | ShiftNeg binda -> unify_typ (Litt negative) binda
    | NegCons (_, args, cont) ->
      List.iter (fun bind -> unify_bind (Litt positive) bind loc) args;
      unify_typ (Litt negative) cont

  and unify_meta_val pol (MetaVal {node; val_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "value with(%a) %a"
          pp_upol pol
          pp_pre_value node;
        dump_env std_formatter !env
      | None -> ()
    end;
    Format.pp_print_flush Format.std_formatter ();

    unify_val pol node loc;
    unify_typ pol val_typ

  and unify_meta_stk cont_pol final_pol (MetaStack {node; cont_typ; final_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "stack with(%a) final(%a) %a"
          pp_upol cont_pol
          pp_upol final_pol
          pp_pre_stack node;
        dump_env std_formatter !env;
        Format.pp_print_flush Format.std_formatter ()
      | None -> ()
    end ;
    unify_typ cont_pol cont_typ;
    unify_typ final_pol final_typ;
    unify_stk cont_pol final_pol loc node

 and unify_cmd final_pol (Command cmd) =
   begin match debug with
     | Some fmt ->
       fprintf fmt "command final(%a) %a"
         pp_upol final_pol
         pp_cmd (Command cmd);
       dump_env std_formatter !env
     | None -> ()
   end ;
    (* unify_typ cmd.pol cmd.loc cmd.mid_typ; *)
    unify_meta_val cmd.pol cmd.valu;
    unify_meta_stk cmd.pol final_pol cmd.stk

  in

  begin match item.content with
  | Value_definition valu -> unify_meta_val item.pol valu
  | Stack_definition stk -> unify_meta_stk item.pol (Litt negative) stk
  | Command_definition cmd -> unify_cmd (Litt negative) cmd
  end;

  !env
