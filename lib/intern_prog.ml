open Util
open Vars
open Types
open Constructors
open Intern_prelude

open InternAst

exception Ambiguous_polarity of position

exception Undefined_var of string * position

exception Undefined_constructor of string * position

exception Undefined_destructor of string * position

let fail_ambiguous_polarity loc = raise (Ambiguous_polarity loc)

let fail_undefined_var var loc = raise (Undefined_var (var, loc))

let fail_undefined_cons cons loc = raise (Undefined_constructor (cons, loc))

let fail_undefined_destr destr loc = raise (Undefined_destructor (destr, loc))


let intern_type_annot env typ = match typ with
  | Some typ -> intern_type !env typ
  | None -> TInternal (TyVar.fresh ())

let aux_cons env loc k = function
    | Unit -> Unit
    | ShiftPos a -> ShiftPos (k a)
    | Pair (a,b) -> Pair (k a, k b)
    | Left a -> Left (k a)
    | Right b -> Right (k b)
    | PosCons (cons, args) ->
      let cons =
        try StringEnv.find cons !env.conses
        with Not_found -> fail_undefined_cons cons loc in
      PosCons (cons, List.map k args)

let aux_destr env loc kx ka = function
  | Call (x,a) -> Call (kx x, ka a)
  | Yes a -> Yes (ka a)
  | No b -> No (ka b)
  | ShiftNeg a -> ShiftNeg (ka a)
  | NegCons (destr, args, cont) ->
    let destr =
        try StringEnv.find destr !env.destrs
        with Not_found -> fail_undefined_cons destr loc in
      NegCons (destr, List.map kx args, ka cont)

let intern_definition env def =

  let env = ref env in

  let intern_pol = function
    | Some Positive -> UPos
    | Some Negative -> UNeg
    | None -> Redirect (PolVar.fresh ()) in

  let rec intern_val = function

    | Cst.Var {node; loc} ->
      begin
        try
          let var = StringEnv.find node !env.values in
          let val_typ = TInternal (TyVar.fresh ()) in
          MetaVal {node = Var var; loc; val_typ}
        with
        | Not_found -> fail_undefined_var node loc
      end

    | Cst.Bindcc {typ; pol; cmd; loc} ->
      let pol = intern_pol pol in
      let val_typ = intern_type_annot env typ in
      let cmd = intern_cmd val_typ cmd in
      MetaVal {node = Bindcc {bind = val_typ; pol; cmd}; loc; val_typ}

    | Cst.Box {typ; cmd; loc; kind} ->
      let val_typ = intern_type_annot env typ in
      let cmd = intern_cmd val_typ cmd in
      MetaVal {node = Box {bind = val_typ; cmd; kind}; loc; val_typ}

    | Cst.Macro_box {kind; valu; loc} ->
      intern_val (Cst.V.box ~loc kind None Cst.(valu |+| S.ret ()))

    | Cst.Macro_fun {arg; typ; valu; loc} ->
      let func = Cst.(V.case ~loc:loc [
          call (arg, typ) None |=> (valu |~| S.ret ())
        ]) in
      intern_val func

    | Cst.Cons {node;loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      MetaVal {node = Cons (intern_cons loc node); loc; val_typ}

    | Cst.Destr {node; loc} ->
      let val_typ = TInternal (TyVar.fresh ()) in
      let go_one (destr, cmd) = (intern_copatt loc destr, intern_cmd val_typ cmd) in
      MetaVal {loc; val_typ; node = Destr (List.map go_one node)}


  and intern_cmd conttyp cmd = match cmd with

    | Cst.Macro_term {name; pol; typ; valu; cmd; loc} ->
      let stk = Cst.S.(bind ~loc ?pol typ name cmd) in
      intern_cmd conttyp (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_env {pol; typ; stk; cmd; loc} ->
      let valu = Cst.V.(bindcc ~loc ?pol typ cmd) in
      intern_cmd conttyp (Command {loc; pol; valu; stk; typ})

    | Cst.Macro_match_val {patt; pol; valu; cmd; loc} ->
      let stk = Cst.S.case [patt, cmd] in
      intern_cmd conttyp (Command {loc; pol; valu; stk; typ = None})

    | Cst.Macro_match_stk {copatt; pol; cmd; loc} ->
      let valu = Cst.V.case [copatt, cmd] in
      intern_cmd conttyp (Command {loc; pol; valu; stk = Cst.S.ret (); typ = None})

    | Cst.Command {pol; valu; stk; typ; loc} ->
      let mid_typ = intern_type_annot env typ in
      let valu = intern_val valu in
      let stk = intern_stk conttyp stk in
      let pol = intern_pol pol in
      Command {mid_typ; loc; valu; stk; pol}


  and intern_stk final_typ stk = match stk with

    | Cst.Ret {loc} ->
      MetaStack {loc; cont_typ = final_typ; final_typ; node = Ret}

    | Cst.CoBind {loc; name; typ; pol; cmd} ->
      let var = Var.of_string name in
      let cont_typ = intern_type_annot env typ in
      env := {!env with values = StringEnv.add name var !env.values};
      MetaStack {loc; cont_typ; final_typ; node = CoBind {
          bind = (var, cont_typ);
          pol = intern_pol pol;
          cmd = intern_cmd final_typ cmd
        }}

    | CoBox {kind; stk; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let node = CoBox {kind; stk = intern_stk final_typ stk} in
      MetaStack {loc; cont_typ; final_typ; node}

    | CoCons {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      let go_one (cons, cmd) = (intern_patt loc cons, intern_cmd final_typ cmd) in
      MetaStack {loc; cont_typ; final_typ; node = CoCons (List.map go_one node)}

    | CoDestr {node; loc} ->
      let cont_typ = TInternal (TyVar.fresh ()) in
      MetaStack {loc; cont_typ; final_typ; node = CoDestr (intern_destr loc final_typ node)}

  and intern_cons loc cons = aux_cons env loc intern_val cons

  and intern_patt loc patt =
    let k (name, typ) =
      let var = Var.of_string name in
      let typ = intern_type_annot env typ in
      env := {!env with values = StringEnv.add name var !env.values};
      (var, typ) in
    aux_cons env loc k patt

  and intern_destr loc final_typ destr =
    aux_destr env loc intern_val (intern_stk final_typ) destr

  and intern_copatt loc copatt =
     let kx (name, typ) =
      let var = Var.of_string name in
      let typ = intern_type_annot env typ in
      env := {!env with values = StringEnv.add name var !env.values};
      (var, typ) in
     let ka typ = intern_type_annot env typ in
     aux_destr env loc kx ka copatt


  in

  let def = match def with
    | Cst.Term_definition {name; typ; content; loc} ->
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = cons unit_t; (* Value definition get a dummy continuation type *)
        content = Value_definition (intern_val content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | Cst.Env_definition {name; typ; content; loc} ->
      let final_type = TInternal (TyVar.fresh ()) in
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = final_type; (* Value definition get a dummy continuation type *)
        content = Stack_definition (intern_stk final_type content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | Cst.Cmd_definition {name; typ; content; loc} ->
      let final_type = TInternal (TyVar.fresh ()) in
      let var = DefVar.of_string name in
      Definition {
        name = var;
        typ = intern_type_annot env typ;
        cont = final_type; (* Value definition get a dummy continuation type *)
        content = Command_definition (intern_cmd final_type content);
        loc;
        pol = Redirect (PolVar.fresh ())}

    | _ -> raise (Failure "FATAL Invariant break: in internalizer, \
                           a prelude definition has found its way \
                           in the term internalizer") in

  (def, !env)


let intern_prog env prog =
  let go (prog, env) item =
    let item, env = intern_definition env item in
    (item :: prog, env) in
  let prog, env = List.fold_left go ([],env) prog in
  List.rev prog, env

let internalize prog =
  let prog, prelude, env = Intern_prelude.internalize_prelude prog in
  let prog, env = intern_prog env prog in
  prelude, prog, env
