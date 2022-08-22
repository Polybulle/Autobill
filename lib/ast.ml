open Types
open Vars
open Constructors
open Util

type tycons_definition = {
  ret_sort : sort;
  full_sort : sort;
  loc : position;
  args : (TyVar.t * sort) list;
  content : tycons_def_content
}
and tycons_def_content =
  | Declared
  | Defined of typ
  | Data of (ConsVar.t, typ) constructor list
  | Codata of (DestrVar.t, typ, typ) destructor list

type cons_definition = Consdef of {
  typ_args : (TyVar.t * sort) list;
  val_args : typ list;
  resulting_type : typ
}

type destr_definition = Destrdef of {
  typ_args : (TyVar.t * sort) list;
  val_args : typ list;
  ret_arg : typ;
  resulting_type : typ
}

type prelude = {
  tycons : tycons_definition TyConsVar.Env.t;
  cons : cons_definition ConsVar.Env.t;
  destr : destr_definition DestrVar.Env.t;
  vars : typ Var.Env.t;
  sorts : sort TyVar.Env.t;
}

let get_env var env =
  match TyVar.Env.find_opt var !env with
  | Some v -> v
  | None ->
    (let w = TyVar.fresh () in
    env := TyVar.Env.add var w !env;
    w)

let rec refresh_typ env typ = match typ with
| TBox b -> TBox {b with node = refresh_typ env b.node}
| TVar {node; loc} -> TVar {node = get_env node env; loc}
| TPos typ -> TPos (refresh_typ env typ)
| TNeg typ -> TNeg (refresh_typ env typ)
| TInternal var -> TInternal (get_env var env)
| TCons {node; loc} ->
  TCons {loc;
         node = match node with
           Unit | Zero | Top | Bottom -> node
           | ShiftNeg t -> ShiftNeg (refresh_typ env t)
           | ShiftPos t -> ShiftPos (refresh_typ env t)
           | Prod ts -> Prod (List.map (refresh_typ env) ts)
           | Fun (ts,t) -> Fun (List.map (refresh_typ env) ts, refresh_typ env t)
           | Sum ts -> Sum (List.map (refresh_typ env) ts)
           | Choice ts -> Choice (List.map (refresh_typ env) ts)
           | Cons (cons, ts) -> Cons (cons, List.map (refresh_typ env) ts)
        }

and refresh_cons env = function
  | PosCons (cons, ts) -> PosCons (cons, List.map (refresh_typ env) ts)
  | _ -> raise (Failure "Internalisation invariant")

and refresh_destr env = function
  | NegCons (cons, ts, t) ->
    NegCons (cons, List.map (refresh_typ env) ts, refresh_typ env t)
  | _ -> raise (Failure "Internalisation invariant")

and refresh_tycons_def env def =
  {def with
   args = List.map (fun (x,so) -> (get_env x env, so)) def.args;
   content = match def.content with
     | Declared -> Declared
     | Defined typ -> Defined (refresh_typ env typ)
     | Data conses -> Data (List.map (refresh_cons env) conses)
     | Codata destrs -> Codata (List.map (refresh_destr env) destrs)
  }

and refresh_cons_def env (Consdef { typ_args; val_args; resulting_type }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  Consdef {typ_args; val_args; resulting_type}

and refresh_destr_def env (Destrdef { typ_args; val_args; ret_arg; resulting_type }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  let ret_arg = refresh_typ env ret_arg in
  Destrdef {typ_args; val_args; resulting_type; ret_arg}

let def_of_cons prelude cons =
    refresh_cons_def (ref TyVar.Env.empty) (ConsVar.Env.find cons prelude.cons)

let def_of_destr prelude destr =
    refresh_destr_def (ref TyVar.Env.empty) (DestrVar.Env.find destr prelude.destr)

let def_of_tycons prelude t =
    refresh_tycons_def (ref TyVar.Env.empty) (TyConsVar.Env.find t prelude.tycons)

module type AstParams = sig
  type val_bind
  type cont_bind
  type polarity
end

module FullAstParams = struct
  type val_bind = Var.t * typ
  type cont_bind = typ
  type polarity = Types.polarity
end

module Ast (Params : AstParams) = struct

  include Params

  type typ = (TyConsVar.t, TyVar.t) pre_typ
  type pattern = (ConsVar.t, val_bind) constructor
  type copattern = (DestrVar.t, val_bind, typ) destructor

  type meta_value =
      MetaVal of {
        node : pre_value;
        val_typ : typ;
        loc : position;
      }
  and pre_value =
    | Var of Var.t
    | CoTop
    | Bindcc of {
        bind : cont_bind;
        pol : polarity;
        cmd : command;
      }
    | Box of {
        kind : box_kind;
        bind : cont_bind;
        cmd : command;
      }
    | Cons of (ConsVar.t, meta_value) constructor
    | Destr of (copattern * command) list

  and meta_stack =
      MetaStack of {
        node : pre_stack;
        cont_typ : typ;
        final_typ : typ;
        loc : position;
      }
  and pre_stack =
    | Ret
    | CoZero
    | CoBind of {
        bind : val_bind;
        pol : polarity;
        cmd : command;
      }
    | CoBox of {
        kind : box_kind;
        stk : meta_stack;
      }
    | CoDestr of (DestrVar.t, meta_value, meta_stack) destructor
    | CoCons of (pattern * command) list
  and command =
      Command of {
        pol : polarity;
        valu : meta_value;
        stk : meta_stack;
        mid_typ : typ;
        final_typ : typ;
        loc : position;
      }

  type prog_item =
    | Value_declaration of {
      name : Var.t;
      typ : typ;
      pol : polarity;
      loc : position
    }
    | Value_definition  of {
      name : Var.t;
      typ : typ;
      pol : polarity;
      loc : position;
      content : meta_value
    }
    | Command_execution of {
      name : Var.t;
      pol : polarity;
      cont : typ;
      loc : position;
      content : command;
    }

  type program = prelude * prog_item list

  let dummy_val_meta v = MetaVal {
      node = v;
      loc = Util.dummy_pos;
      val_typ = tvar (TyVar.fresh ())
    }

  let dummy_stack_meta s = MetaStack {
      node = s;
      loc = Util.dummy_pos;
      cont_typ = tvar (TyVar.fresh ());
      final_typ = tvar (TyVar.fresh ());
    }

  module V = struct
    type t = meta_value
    let cotop = dummy_val_meta CoTop
    let var x = dummy_val_meta (Var x)
    let bindcc pol bind cmd = dummy_val_meta (Bindcc {pol;cmd;bind})
    let box kind bind cmd = dummy_val_meta (Box {kind; bind; cmd})
    let cons c = dummy_val_meta (Cons c)
    let case l = dummy_val_meta (Destr l)
  end

  module S = struct
    type t = meta_stack
    let cozero = dummy_stack_meta CoZero
    let ret = dummy_stack_meta (Ret)
    let bind pol bind cmd = dummy_stack_meta (CoBind {pol; bind; cmd})
    let box kind stk = dummy_stack_meta (CoBox {kind; stk})
    let destr c = dummy_stack_meta (CoDestr c)
    let case l = dummy_stack_meta (CoCons l)
  end

  let empty_prelude = {
    tycons = TyConsVar.Env.empty;
    cons = ConsVar.Env.empty;
    destr = DestrVar.Env.empty;
    vars = Var.Env.empty;
    sorts = TyVar.Env.empty;
  }

end

module FullAst = Ast (FullAstParams)
