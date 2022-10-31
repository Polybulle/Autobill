open Util
open Vars
open Types
open Intern_common
open Intern_prettyPrinter
open Format
open Ast
open InternAst


let get env polvar =
  PolVar.Env.find polvar env.unifier

let get_opt env polvar =
  PolVar.Env.find_opt polvar env.unifier

let rec get_loc env polvar =
  try match get env polvar with
    | Loc (loc, _) -> loc
    | Redirect u -> get_loc env u
    | _ -> dummy_pos
  with
  | Not_found -> dummy_pos

let set upol env polvar =
    let upol =
      try match get env polvar with
        | Loc (loc, _) -> Loc (loc, upol)
        | _ -> upol
      with Not_found -> upol in
    {env with unifier = PolVar.Env.add polvar upol env.unifier}

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
  let loc = if loc1 <> dummy_pos then loc1 else loc2 in

  let p = match pol1, pol2 with
  | Some p, None
  | None, Some p -> (Litt p)
  | None, None -> (Redirect (PolVar.fresh ()))
  | Some p1, Some p2 when p1 = p2 -> (Litt p1)
  | Some _, Some _ ->
    fail_polarity_mismatch upol1 upol2 loc1 loc2 in

  finalize (Loc (loc, p))


let unify_def ?debug env item =

  let prelude = env.prelude in
  let env = ref env in

  let rec pp_upol fmt = function
    | Litt p -> pp_pol fmt p
    | Loc (loc,u) -> fprintf fmt "loc(%s)%a" (string_of_position loc) pp_upol u
    | Redirect v -> pp_print_string fmt (PolVar.to_string v) in

  let rec unify upol1 upol2 =
    env := unify_upol !env upol1 upol2

  and unify_typ_sort sort typ = match sort with
    | Base p -> unify_typ (Litt p) typ

  and unify_tyvar_sort sort tvar = match sort with
    | Base p ->
      match TyVar.Env.find_opt tvar !env.tyvarpols with
      | Some u -> unify u (Litt p)
      | None ->
        env := {!env with tyvarpols = TyVar.Env.add tvar (Litt p) !env.tyvarpols};

  and unify_typ upol1 typ = match typ with
    | TBox {node=t;_} ->
      unify upol1 (Litt positive);
      unify_typ (Litt negative) t
    | TPos t ->
      unify upol1 (Litt positive);
      unify_typ (Litt positive) t
    | TNeg t ->
      unify upol1 (Litt negative);
      unify_typ (Litt positive) t
    | TCons {node;_} -> unify_typecons upol1 node
    | TFix t ->
      unify (Litt negative) upol1;
      unify_typ (Litt negative) t
    | TVar {node=var; _}| TInternal var ->
      try unify upol1 (TyVar.Env.find var !env.tyvarpols)
      with
      | Not_found ->
        let pol = Redirect (PolVar.fresh ()) in
        env := {!env with tyvarpols = TyVar.Env.add var pol !env.tyvarpols};
        unify upol1 pol

  and unify_typecons upol1 (tcons : ('a,'b) Constructors.type_cons) =
    match tcons with
    | Unit | Zero -> unify upol1 (Litt Positive)
    | Top | Bottom -> unify upol1 (Litt Negative)
    | Thunk t -> unify upol1 (Litt Negative); unify_typ (Litt Positive) t
    | Closure t -> unify upol1 (Litt Positive); unify_typ (Litt Negative) t
    | Prod ts | Sum ts ->
      unify upol1 (Litt Positive);
      List.iter (unify_typ (Litt Positive)) ts
    | Choice ts ->
      unify upol1 (Litt Negative);
      List.iter (unify_typ (Litt Negative)) ts
    | Fun (ts, t) ->
      unify upol1 (Litt Negative);
      List.iter (unify_typ (Litt Positive)) ts;
      unify_typ (Litt Negative) t
    | Cons (cons, ts) ->
      let get_base_pol (Base p) = Litt p in
      let consdef = TyConsVar.Env.find cons prelude.tycons in
      unify upol1 (get_base_pol consdef.ret_sort);
      List.iter2 (fun (_, so) t -> unify_typ (get_base_pol so) t) consdef.args ts


  and unify_bind upol (var, typ) loc =
    let polvar =
        try Var.Env.find var !env.varpols
        with Not_found ->
          let polvar = PolVar.fresh () in
          env := {!env with varpols = Var.Env.add var polvar !env.varpols};
          polvar in
    unify upol (Loc (loc, Redirect polvar));
    unify_typ upol typ

  and unify_cobind upol (covar, typ) loc =
    let polvar =
        try CoVar.Env.find covar !env.covarpols
        with Not_found ->
          let polvar = PolVar.fresh () in
          env := {!env with
                  covarpols = CoVar.Env.add covar polvar !env.covarpols
                 };
          polvar in
    unify upol (Loc (loc, Redirect polvar));
    unify_typ upol typ

  and unify_val upol valu loc =
    match valu with
    | Var v ->
      let polvar =
        try Var.Env.find v !env.varpols
        with Not_found -> fail_undefined_var (Var.to_string v) loc in
      unify upol (Loc (loc, Redirect polvar))
    | CoTop -> unify upol (Loc (loc, Litt negative))
    | Bindcc {bind; pol; cmd} ->
      unify_cobind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd upol cmd
    | Box {bind; cmd; _} ->
      unify upol (Loc (loc, Litt positive));
      unify_cobind upol bind loc;
      unify upol (Litt negative);
      unify_cmd upol cmd
    | Cons cons ->
      unify_cons loc upol cons
    | Fix {self=(x,t); cmd; cont=bind} ->
      unify_bind (Litt positive) (x, boxed exp t) loc;
      unify_cobind (Litt negative) bind loc;
      unify_cmd (Litt negative) cmd;
      unify upol (Loc (loc, Litt negative))
    | Destr copatts ->
      let go (copatt, cmd) = unify_copatt copatt cmd upol loc in
      List.iter go copatts
    | Pack (cons, typs, v) ->
      unify upol (Litt Positive);
      let Consdef { private_typs; _ } = def_of_cons prelude cons in
      List.iter2 (fun t (_, so) -> unify_typ_sort so t) typs private_typs;
      unify_meta_val (Litt Positive) v
    | Spec { spec_vars; cmd; destr; bind} ->
      let Destrdef {private_typs; _} = def_of_destr prelude destr in
      unify upol (Litt Negative);
      unify_cobind upol bind loc;
      List.iter2 (fun (x, so) (_, so')->
          unify_tyvar_sort so x;
          unify_tyvar_sort so' x)
        spec_vars private_typs;
      unify_cmd (Litt Negative) cmd

  and unify_stk upol final_upol loc stk =
    match stk with
    | Ret a ->
      unify (Loc (loc, upol)) final_upol;
      let polvar =
        try CoVar.Env.find a !env.covarpols
        with Not_found -> fail_undefined_var (CoVar.to_string a) loc in
      unify upol (Loc (loc, Redirect polvar))
    | CoZero -> unify upol (Loc (loc, Litt positive))
    | CoBind {bind; pol; cmd} ->
      unify_bind upol bind loc;
      unify upol (Loc (loc, pol));
      unify_cmd final_upol cmd
    | CoBox {stk;_} ->
      unify upol (Litt positive);
      unify_meta_stk (Litt negative) final_upol stk
    | CoDestr destr ->
      unify_destr loc upol final_upol destr
    | CoCons patts ->
      let go (patt, cmd) =
        unify_patt patt upol loc;
        unify_cmd final_upol cmd in
      List.iter go patts
    | CoFix stk ->
      unify upol (Litt negative);
      unify_meta_stk (Litt negative) final_upol stk
    | CoSpec (destr, typs, stk) ->
      unify upol (Litt Negative);
      let Destrdef { private_typs; _ } = def_of_destr prelude destr in
      List.iter2 (fun t (_, so) -> unify_typ_sort so t) typs private_typs;
      unify_meta_stk (Litt Negative) final_upol stk
    | CoPack { pack_vars; cmd; bind; cons } ->
      let Consdef { private_typs; _} = def_of_cons prelude cons in
      unify upol (Litt Positive);
      unify_bind (Litt Positive) bind loc;
      List.iter2 (fun (x,so) (_,so') ->
          unify_tyvar_sort so x;
          unify_tyvar_sort so' x)
        pack_vars private_typs;
      unify_cmd final_upol cmd

  and unify_cons loc upol cons =
    let args, upol', args_upol = match cons with
      | Unit -> [], Litt positive, Litt positive
      | Inj(_, _, x) -> [x], Litt positive, Litt positive
      | Thunk x -> [x], Litt negative, Litt positive
      | Tupple xs -> xs, Litt positive, Litt positive
      | PosCons (_, args) -> args, Litt positive, Litt positive in
    unify upol (Loc (loc, upol'));
    List.iter (unify_meta_val args_upol) args

  and unify_destr loc upol final_upol destr =
    let args, cont, upol', cont_pol = match destr with
      | Call (x,a) -> x, a, Litt negative, Litt negative
      | Proj (_, _, a)-> [], a, Litt negative, Litt negative
      | Closure a -> [], a, Litt positive, Litt negative
      | NegCons (_, args, cont) -> args, cont, Litt positive, Litt negative in
    List.iter (unify_meta_val (Litt positive)) args;
    unify upol (Loc (loc, upol'));
    unify_meta_stk cont_pol final_upol cont

  and unify_patt patt upol loc =
    match patt with
    | Thunk _ -> unify upol (Loc (loc, Litt negative))
    | _ ->  unify upol (Loc (loc, Litt positive));
    match patt with
    | Unit -> ()
    | Inj (_, _, bind) -> unify_bind (Litt positive) bind loc
    | Thunk bind -> unify_bind (Litt positive) bind loc
    | Tupple xs ->
      List.iter (fun x -> unify_bind (Litt positive) x loc) xs
    | PosCons (_, args) ->
      List.iter (fun bind -> unify_bind (Litt positive) bind loc) args

  and unify_copatt copatt cmd upol loc =
    match copatt with
    | Closure _ -> unify upol (Loc (loc, Litt positive))
    | _ ->  unify upol (Loc (loc, Litt negative));
    match copatt with
    | Call (bindxs, binda) ->
      List.iter (fun x -> unify_bind (Litt positive) x loc) bindxs;
      unify_cobind (Litt negative) binda loc;
      unify_cmd (Litt negative) cmd
    | Proj (_, _, binda) ->
      unify_cobind (Litt negative) binda loc;
      unify_cmd (Litt negative) cmd
    | Closure binda ->
      unify_cobind (Litt negative) binda loc;
      unify_cmd (Litt negative) cmd;
    | NegCons (_, args, cont) ->
      List.iter (fun bind -> unify_bind (Litt positive) bind loc) args;
      unify_cobind (Litt negative) cont loc;
      unify_cmd (Litt negative) cmd;

  and unify_meta_val pol (MetaVal {node; val_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "value with(%a,%a) %a"
          pp_upol pol
          pp_typ val_typ
          pp_pre_value node;
        dump_env std_formatter !env
      | None -> ()
    end;
    Format.pp_print_flush Format.std_formatter ();

    unify_val pol node loc;
    unify_typ pol val_typ;

  and unify_meta_stk cont_pol final_pol (MetaStack {node; cont_typ; final_typ; loc}) =
    begin match debug with
      | Some fmt ->
        fprintf fmt "stack with(%a:%a) final(%a:%a) %a"
          pp_typ cont_typ
          pp_upol cont_pol
          pp_typ final_typ
          pp_upol final_pol
          pp_pre_stack node;
        dump_env std_formatter !env;
        Format.pp_print_flush Format.std_formatter ()
      | None -> ()
    end ;
    unify_typ cont_pol cont_typ;
    unify_typ final_pol final_typ;
    unify_stk cont_pol final_pol loc node;


 and unify_cmd final_pol (Command cmd) =
   begin match debug with
     | Some fmt ->
       fprintf fmt "command final(%a) %a"
         pp_upol final_pol
         pp_cmd (Command cmd);
       dump_env std_formatter !env
     | None -> ()
   end ;
   unify_meta_val cmd.pol cmd.valu;
   unify_meta_stk cmd.pol final_pol cmd.stk;
   unify_typ cmd.pol cmd.mid_typ;
   unify_typ final_pol cmd.final_typ

  in

  begin match item with
    | Value_declaration item -> unify_typ item.pol item.typ
    | Value_definition item -> unify_meta_val item.pol item.content
    | Command_execution item ->
      let upol = Redirect (PolVar.fresh ()) in
      unify_cobind upol (item.cont, item.conttyp) item.loc;
      unify_cmd upol item.content;
      unify upol item.pol
  end;

  begin match item with
    | Value_declaration {name; pol; loc; _} | Value_definition {name; pol; loc; _} ->
       let polvar = PolVar.fresh () in
      env := {!env with varpols = Var.Env.add name polvar !env.varpols};
      unify pol (Loc (loc, Redirect polvar));
    | _ -> ()
  end;

  !env
