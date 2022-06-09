open Vars
open Types
open Constructors
open Intern_common

open InternAst


let intern_type_annot env typ = match typ with
  | Some typ -> intern_type !env typ
  | None -> TInternal (TyVar.fresh ())

let aux_cons vars env loc k = function
    | Unit -> vars, Unit
    | ShiftPos a ->
      let vars, a = k vars a in
      (vars, ShiftPos a)
    | Pair (a,b) ->
      let vars, a = k vars a in
      let vars, b = k vars b in
      (vars, Pair (a,b))
    | Left a -> let vars, a = k vars a in vars, Left a
    | Right b -> let vars, b = k vars b in vars, Right b
    | PosCons (cons, args) ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      let vars, args_rev =
        List.fold_left
          (fun (vars, args_rev) arg -> let vars, arg = k vars arg in (vars, arg :: args_rev))
          (vars, []) args in
      vars, PosCons (cons, List.rev args_rev)

let aux_destr vars env loc kx ka = function
  | Call (x,a) ->
    let vars, x = kx vars x in
    let vars, a = ka vars a in
    vars, Call (x, a)
  | Yes a -> let vars, a = ka vars a in vars, Yes a
  | No a -> let vars, a = ka vars a in vars, No a
  | ShiftNeg a -> let vars, a = ka vars a in vars, ShiftNeg a
  | NegCons (destr, args, cont) ->
    let destr =
        try StringEnv.find destr !env.destrs
        with Not_found -> fail_undefined_cons destr loc in
    let vars, args_rev =
        List.fold_left
          (fun (vars, args_rev) arg -> let vars, arg = kx vars arg in (vars, arg :: args_rev))
          (vars, []) args in
    let vars, cont = ka vars cont in
      vars, NegCons (destr, List.rev args_rev, cont)

let intern_definition env def =

  let env = ref env in

  let intern_pol = function
    | Some p -> Litt p
    | None -> Redirect (PolVar.fresh ()) in

  let rec intern_val vars = function

    | Cst.Var {node; loc} ->
      begin
        try
          let var = StringEnv.find node vars in
          let val_typ = TInternal (TyVar.fresh ()) in
          MetaVal {node = Var var; loc; val_typ}
        with
        | Not_found -> fail_undefined_var node loc
      end

    | Cst.Bindcc {typ; pol; cmd; loc} ->
      let pol = intern_pol pol in
      let val_typ = intern_type_annot env typ in
      let cmd = intern_cmd vars val_typ cmd in
      MetaVal {node = Bindcc {bind = (pol, val_typ); pol; cmd}; loc; val_typ}

    | Cst.Box {typ; cmd; loc; kind} ->
      let val_typ = intern_type_annot env typ in
      let cmd = intern_cmd vars val_typ cmd in
      MetaVal {node = Box {bind = (Litt positive, val_typ); cmd; kind}; loc; val_typ}

    | Cst.Macro_box {kind; valu; loc} ->
      intern_val vars (Cst.V.box ~loc kind None Cst.(valu |+| S.ret ()))

    | Cst.Macro_fun {arg; typ; valu; loc} ->
      let func = Cst.(V.case ~loc:loc [
          call (arg, typ) None |=> (valu |~| S.ret ())
        ]) in
      intern_val vars func

    | Cst.Cons {node;loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Cons (intern_cons vars loc node); loc; val_typ}

    | Cst.Destr {node; loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      let go_one (destr, cmd) =
        let vars, destr = intern_copatt vars loc destr in
        let cmd = intern_cmd vars val_typ cmd in
        (destr, cmd) in
      MetaVal {loc; val_typ; node = Destr (List.map go_one node)}


  and intern_cmd vars conttyp cmd = match cmd with

    | Cst.Macro_term {name; pol; typ; valu; cmd; loc} ->
      let stk = Cst.S.(bind ~loc ?pol typ name cmd) in
      intern_cmd vars conttyp (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_env {pol; typ; stk; cmd; loc} ->
      let valu = Cst.V.(bindcc ~loc ?pol typ cmd) in
      intern_cmd vars conttyp (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_match_val {patt; pol; valu; cmd; loc} ->
      let stk = Cst.S.case [patt, cmd] in
      intern_cmd vars conttyp (Command {loc; pol; valu; stk; typ = None})

    | Cst.Macro_match_stk {copatt; pol; cmd; loc} ->
      let valu = Cst.V.case [copatt, cmd] in
      intern_cmd vars conttyp (Command {loc; pol; valu; stk = Cst.S.ret (); typ = None})

    | Cst.Command {pol; valu; stk; typ; loc} ->
      let mid_typ = intern_type_annot env typ in
      let valu = intern_val vars valu in
      let stk = intern_stk vars conttyp stk in
      let pol = intern_pol pol in
      Command {mid_typ; loc; valu; stk; pol}


  and intern_stk vars final_typ stk = match stk with

    | Cst.Ret {loc} ->
      MetaStack {loc; cont_typ = final_typ; final_typ; node = Ret}

    | Cst.CoBind {loc; name; typ; pol; cmd} ->
      let var = Var.of_string name in
      let cont_typ = intern_type_annot env typ in
      let vars = StringEnv.add name var vars in
      MetaStack {loc; cont_typ; final_typ; node = CoBind {
          bind = (var, cont_typ);
          pol = intern_pol pol;
          cmd = intern_cmd vars final_typ cmd
        }}

    | CoBox {kind; stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let node = CoBox {kind; stk = intern_stk vars final_typ stk} in
      MetaStack {loc; cont_typ; final_typ; node}

    | CoCons {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let go_one (cons, cmd) =
        let vars, cons = intern_patt vars loc cons in
        let cmd = intern_cmd vars final_typ cmd in
        (cons, cmd) in
      MetaStack {loc; cont_typ; final_typ; node = CoCons (List.map go_one node)}

    | CoDestr {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      MetaStack {loc; cont_typ; final_typ; node = CoDestr (intern_destr vars loc final_typ node)}

  and intern_cons vars loc cons =
    snd @@ aux_cons vars env loc (fun vars valu -> (vars, intern_val vars valu)) cons

  and intern_patt vars loc patt =
    let k vars (name, typ) =
      let var = Var.of_string name in
      let typ = intern_type_annot env typ in
      let vars = StringEnv.add name var vars in
      vars, (var, typ) in
    aux_cons vars env loc k patt

  and intern_destr vars loc final_typ destr =
    snd @@ aux_destr vars env loc
      (fun vars valu -> (vars, intern_val vars valu))
      (fun vars stk -> (vars, intern_stk vars final_typ stk))
       destr

  and intern_copatt vars loc copatt =
     let kx vars (name, typ) =
      let var = Var.of_string name in
      let typ = intern_type_annot env typ in
      let vars = StringEnv.add name var vars in
      vars, (var, typ) in
     let ka vars typ = vars, intern_type_annot env typ in
     aux_destr vars env loc kx ka copatt


  in

  let vars = StringEnv.empty in

  let def = match def with
    | Cst.Term_definition {name; typ; content; loc} ->
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = cons unit_t; (* Value definition get a dummy continuation type *)
        content = Value_definition (intern_val vars content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | Cst.Env_definition {name; typ; content; loc} ->
      let final_type = TInternal (TyVar.fresh ()) in
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = final_type; (* Value definition get a dummy continuation type *)
        content = Stack_definition (intern_stk vars final_type content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | Cst.Cmd_definition {name; typ; content; loc} ->
      let final_type = TInternal (TyVar.fresh ()) in
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = final_type; (* Value definition get a dummy continuation type *)
        content = Command_definition (intern_cmd vars final_type content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | _ -> raise (Failure "FATAL Invariant break: in internalizer, \
                           a prelude definition has found its way \
                           in the term internalizer") in

  (def, !env)

