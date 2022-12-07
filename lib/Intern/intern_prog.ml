open Vars
open Types
open Constructors
open Intern_common

open InternAst


let intern_type_annot env scope typ = match typ with
  | Some typ -> intern_type !env scope typ
  | None -> TInternal (TyVar.fresh ())

let rec visit_many_vars vars k = function
    | [] -> vars, []
    | h::t ->
      let vars, h = k vars h in
      let vars, t = visit_many_vars vars k t in
      vars, h :: t

let visit_cons vars env loc k = function
    | Unit -> vars, Unit
    | Thunk a ->
      let vars, a = k vars a in
      (vars, Thunk a)
    | Tupple xs ->
      let vars, xs = visit_many_vars vars k xs in
      vars, Tupple xs
    | Inj (i,n,a) -> let vars, a = k vars a in vars, Inj (i,n,a)
    | PosCons (cons, args) ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      let vars, args_rev =
        List.fold_left
          (fun (vars, args_rev) arg -> let vars, arg = k vars arg in (vars, arg :: args_rev))
          (vars, []) args in
      vars, PosCons (cons, List.rev args_rev)

let visit_destr vars env loc kx ka = function
  | Call (xs,a) ->
    let vars, xs = visit_many_vars vars kx xs in
    let vars, a = ka vars a in
    vars, a, Call (xs, a)
  | Proj (i,n,a) -> let vars, a = ka vars a in vars, a, Proj (i,n,a)
  | Closure a -> let vars, a = ka vars a in vars, a, Closure a
  | NegCons (destr, args, cont) ->
    let destr =
        try StringEnv.find destr !env.destrs
        with Not_found -> fail_undefined_cons destr loc in
    let vars, args_rev =
        List.fold_left
          (fun (vars, args_rev) arg -> let vars, arg = kx vars arg in (vars, arg :: args_rev))
          (vars, []) args in
    let vars, cont = ka vars cont in
      vars, cont, NegCons (destr, List.rev args_rev, cont)


let intern_definition env declared_vars def =

  let env = ref env in

  let intern_pol = function
    | Some p -> Litt (Base p)
    | None -> Redirect (USortVar.fresh ()) in

  let rec intern_val scope = function

    | Cst.Var {node; loc} ->
      let var =
        try get_var scope node
        with Not_found ->
        try StringEnv.find node declared_vars
        with Not_found -> fail_undefined_var node loc in
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Var var; loc; val_typ}

    | Cst.CoTop {loc} -> MetaVal {node = CoTop; loc; val_typ = top}

    | Cst.Bindcc {bind=(a,typ); pol; cmd; loc} ->
      let pol = intern_pol pol in
      let val_typ = intern_type_annot env scope typ in
      let scope = add_covar scope a in
      let a = get_covar scope a in
      let cmd = intern_cmd scope cmd in
      MetaVal {node = Bindcc {bind = (a, val_typ); pol; cmd}; loc; val_typ}

    | Cst.Box {bind=(a,typ); cmd; loc; kind} ->
      let typ = intern_type_annot env scope typ in
      let scope = add_covar scope a in
      let a = get_covar scope a in
      let cmd = intern_cmd scope cmd in
      MetaVal {node = Box {bind = (a, typ); cmd; kind}; loc; val_typ = boxed exp typ}

    | Cst.Macro_box {kind; valu; loc} ->
      let a_str = CoVar.to_string (CoVar.fresh ()) in
      let scope = add_covar scope a_str in
      intern_val scope (Cst.V.box ~loc kind a_str None Cst.(valu |+| S.ret a_str))

    | Cst.Macro_fun {args; valu; loc} ->
      let a_str = CoVar.to_string (CoVar.fresh ()) in
      let scope = add_covar scope a_str in
      let func = Cst.(V.case ~loc:loc [
          call args (a_str, None) |=> (valu |~| S.ret a_str)
        ]) in
      intern_val scope func

    | Cst.Cons {node;loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Cons (intern_cons scope loc node); loc; val_typ}

    | Cst.Destr {node; loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      let go_one (destr, cmd) =
        let scope, _, destr = intern_copatt scope loc destr in
        let cmd = intern_cmd scope cmd in
        (destr, cmd) in
      let destr = List.map go_one node in
      MetaVal {loc; val_typ; node = Destr destr}

    | Fix {self=(x,tx); cont=(a,ta); cmd; loc} ->
      let scope = add_var (add_covar scope a) x in
      let x_typ = intern_type_annot env scope tx in
      let a_typ = intern_type_annot env scope ta in
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {loc; val_typ; node = Fix {
          self = (get_var scope x, x_typ);
          cont = (get_covar scope a, a_typ);
          cmd = intern_cmd scope cmd;
        }}

    | Pack {cons; typs; content; loc} ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      let typs = List.map (intern_type !env scope) typs in
      let content = intern_val scope content in
      MetaVal {loc; val_typ = TInternal (TyVar.fresh ()); node =
                                                            Pack (cons, typs, content)}

    | Spec {destr; spec_vars; bind=(a,typ); cmd; loc} ->
      let destr =
        try StringEnv.find destr !env.destrs
        with Not_found ->
          fail_undefined_destr destr loc in
      let go scope (tvar, so) =
        let so = intern_sort !env so in
        let scope = add_tyvar scope tvar in
        let new_var = get_tyvar scope tvar in
        env := {!env with
                prelude_typevar_sort = TyVar.Env.add new_var so !env.prelude_typevar_sort};
        (scope, (new_var, so)) in
      let scope, spec_vars = List.fold_left_map go scope spec_vars in
      let scope = add_covar scope a in
      let typ = match typ with
        | Some typ -> intern_type !env scope typ
        | None -> TInternal (TyVar.fresh ()) in
      let cmd = intern_cmd scope cmd in
      MetaVal {
        loc;
        val_typ = TInternal (TyVar.fresh ());
        node = Spec {destr; spec_vars; bind=(get_covar scope a, typ); cmd}
      }

  and intern_cmd scope cmd = match cmd with

    | Cst.Macro_term {name; pol; typ; valu; cmd; loc} ->
      let stk = Cst.S.(bind ~loc ?pol name typ cmd) in
      intern_cmd scope (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_env {pol; name; typ; stk; cmd; loc} ->
      let valu = Cst.V.(bindcc ~loc ?pol name typ cmd) in
      intern_cmd scope (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_match_val {patt; pol; valu; cmd; loc} ->
      let stk = Cst.S.case [patt, cmd] in
      intern_cmd scope (Command {loc; pol; valu; stk; typ = None})

    | Cst.Macro_match_stk {copatt; pol; stk; cmd; loc} ->
      let valu = Cst.V.case [copatt, cmd] in
      intern_cmd scope (Command {loc; pol; valu; stk; typ = None})

    | Cst.Command {pol; valu; stk; typ; loc} ->
      let mid_typ = intern_type_annot env scope typ in
      let final_typ = intern_type_annot env scope None in
      let valu = intern_val scope valu in
      let stk = intern_stk scope stk in
      let pol = intern_pol pol in
      Command {mid_typ; final_typ; loc; valu; stk; pol}


  and intern_stk scope stk =
    let final_typ = tvar (TyVar.fresh ()) in

    match stk with

    | Cst.Ret {var; loc} ->
      MetaStack {loc; cont_typ = final_typ; final_typ;
                 node = Ret (get_covar scope var)}

    | Cst.CoZero {loc} ->
      MetaStack {loc; cont_typ = zero; final_typ; node = CoZero}

    | Cst.CoBind {loc; bind=(name,typ); pol; cmd} ->
      let cont_typ = intern_type_annot env scope typ in
      let scope = add_var scope name in
      let name = get_var scope name in
      MetaStack {loc; cont_typ; final_typ; node = CoBind {
          bind = (name, cont_typ);
          pol = intern_pol pol;
          cmd = intern_cmd scope cmd
        }}

    | CoBox {kind; stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let node = CoBox {kind; stk = intern_stk scope stk} in
      MetaStack {loc; cont_typ; final_typ; node}

    | CoCons {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let go_one (cons, cmd) =
        let scope, cons = intern_patt scope loc cons in
        let cmd = intern_cmd scope cmd in
        (cons, cmd) in
      MetaStack {loc; cont_typ; final_typ; node = CoCons (List.map go_one node)}

    | CoDestr {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      MetaStack {loc; cont_typ; final_typ;
                 node = CoDestr (intern_destr scope loc node)}

    | CoFix {stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let stk = intern_stk scope stk in
      MetaStack {loc; cont_typ; final_typ; node = CoFix stk}

    | CoSpec {destr; typs; content; loc} ->
      let destr =
        try StringEnv.find destr !env.destrs
        with Not_found -> fail_undefined_destr destr loc in
      let typs = List.map (intern_type !env scope) typs in
      let content = intern_stk scope content in
      MetaStack {loc; cont_typ = TInternal (TyVar.fresh ());
                 final_typ = TInternal (TyVar.fresh ());
                 node = CoSpec (destr, typs, content)}

    | CoPack {cons; pack_vars; bind = (x,t); cmd; loc} ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      let go scope (tvar, so) =
        let so = intern_sort !env so in
        let new_var = get_tyvar scope tvar in
        env := {!env with
                prelude_typevar_sort = TyVar.Env.add new_var so !env.prelude_typevar_sort};
        (scope, (new_var, so)) in
      let scope, pack_vars = List.fold_left_map go scope pack_vars in
      let scope = add_var scope x in
      let x' = get_var scope x in
      let t = match t with
        | Some t -> intern_type !env scope t
        | None -> TInternal (TyVar.fresh ()) in
      let cmd = intern_cmd scope cmd in
      MetaStack {loc; cont_typ = TInternal (TyVar.fresh ());
                 final_typ = TInternal (TyVar.fresh ());
                 node = CoPack {cons; pack_vars; bind = (x',t); cmd}}


  and intern_cons vars loc cons =
    snd @@ visit_cons vars env loc (fun vars valu -> (vars, intern_val vars valu)) cons

  and intern_patt scope loc patt =
    let k scope (name, typ) =
      let typ = intern_type_annot env scope typ in
      let scope = add_var scope name in
      let var = get_var scope name in
      scope, (var, typ) in
    visit_cons scope env loc k patt


  and intern_destr scope loc destr =
    let _,_,destr =  visit_destr scope env loc
        (fun vars valu -> (vars, intern_val vars valu))
        (fun vars stk -> (vars, intern_stk vars stk))
        destr in
    destr

  and intern_copatt scope loc copatt =
    let kx scope (name, typ) =
      let scope = add_var scope name in
      let var = get_var scope name in
      let typ = intern_type_annot env scope typ in
      scope, (var, typ) in
    let ka scope (name, typ) =
      let scope = add_covar scope name in
      let var = get_covar scope name in
      let typ = intern_type_annot env scope typ in
      scope, (var, typ) in
    visit_destr scope env loc kx ka copatt


  in

  let scope = empty_scope in

  let def' = match def with

    | Cst.Term_declaration {name; typ; loc} ->
      let var = Var.of_string name in
      Value_declaration {
        name = var;
        loc;
        typ = intern_type_annot env scope (Some typ);
        pol = Redirect (USortVar.fresh ())}

    | Cst.Term_definition {name; typ; content; loc} ->
      let var = Var.of_string name in
      Value_definition {
        name = var;
        typ = intern_type_annot env scope typ;
        content = intern_val scope content;
        loc;
        pol = Redirect (USortVar.fresh ())}

    | Cst.Cmd_execution {name; typ; content; loc; cont} ->
      let final_type = intern_type_annot env scope typ in
      let scope = add_covar scope cont in
      let var = match name with
        | Some name -> Var.of_string name
        | None -> Var.of_string "anon" in
      Command_execution {
        name = var;
        conttyp = final_type;
        cont = get_covar scope cont;
        content = intern_cmd scope content;
        loc;
        pol = Redirect (USortVar.fresh ())}

    | _ -> raise (Failure "FATAL Invariant break: in internalizer, \
                           a prelude definition has found its way \
                           in the term internalizer") in

  let declared_vars = match def, def' with
    | Cst.Term_declaration {name = old_name;_}, Value_declaration {name = new_name; _}
    | Cst.Term_definition {name = old_name; _}, Value_definition {name = new_name; _}->
      StringEnv.add old_name new_name declared_vars
    | _ -> declared_vars in

  (declared_vars, def', !env)
