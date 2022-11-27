open Types
open Vars
open Constructors
open Misc

type sort = SortVar.t Types.sort

type tycons_definition = {
  sort : sort;
  loc : position;
  args : (TyVar.t * sort) list;
  content : tycons_def_content
}
and tycons_def_content =
  | Declared
  | Defined of typ
  | Data of (ConsVar.t, typ) constructor list
  | Codata of (DestrVar.t, typ, typ) destructor list
  | Pack of ConsVar.t * cons_definition
  | Spec of DestrVar.t * destr_definition

and cons_definition = Consdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  val_args : typ list;
  resulting_type : typ
}

and destr_definition = Destrdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  val_args : typ list;
  ret_arg : typ;
  resulting_type : typ
}

type var_multiplicity =
  | MulZero
  | MulOne
  | MulMany

let mult a b = match a,b with
  | MulZero, x | x, MulZero -> x
  | MulMany, _ | _, MulMany -> MulMany
  | MulOne, MulOne -> MulMany

let update a b = match b with
  | None -> Some a
  | Some b -> Some (mult a b)

type prelude = {
  sort_defs : unit SortVar.Env.t;
  tycons : tycons_definition TyConsVar.Env.t;
  cons : cons_definition ConsVar.Env.t;
  destr : destr_definition DestrVar.Env.t;
  vars : typ Var.Env.t;
  covars : typ CoVar.Env.t;
  sorts : sort TyVar.Env.t;
  var_multiplicities : var_multiplicity Var.Env.t;
  covar_multiplicities : var_multiplicity CoVar.Env.t
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
| TFix t -> TFix (refresh_typ env t)
| TCons _ -> typ
| TApp {loc; tfun; args} ->
  TApp {loc;
       tfun = refresh_typ env tfun;
       args = List.map (refresh_typ env) args}

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
     | Pack (cons, def) -> Pack (cons, refresh_cons_def env def)
     | Spec (destr, def) -> Spec (destr, refresh_destr_def env def)
  }

and refresh_cons_def env
    (Consdef { typ_args; val_args; private_typs; resulting_type }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  Consdef {typ_args; val_args; resulting_type; private_typs}

and refresh_destr_def env
    (Destrdef { typ_args; val_args; ret_arg; resulting_type; private_typs }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  let ret_arg = refresh_typ env ret_arg in
  Destrdef {typ_args; val_args; resulting_type; ret_arg; private_typs}

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
  type cont_bind = CoVar.t * typ
  type polarity = Types.polarity
end

module Ast (Params : AstParams) = struct

  include Params

  type typ = (TyConsVar.t, TyVar.t) pre_typ
  type pattern = (ConsVar.t, val_bind) constructor
  type copattern = (DestrVar.t, val_bind, cont_bind) destructor

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
    | Fix of {
        self : val_bind;
        cont : cont_bind;
        cmd : command
      }
    | Cons of (ConsVar.t, meta_value) constructor
    | Pack of ConsVar.t * typ list * meta_value
    | Spec of {
        destr : DestrVar.t;
        bind : cont_bind;
        spec_vars : (TyVar.t * sort) list;
        cmd : command;
      }
    | Destr of (copattern * command) list

  and meta_stack =
      MetaStack of {
        node : pre_stack;
        cont_typ : typ;
        final_typ : typ;
        loc : position;
      }
  and pre_stack =
    | Ret of CoVar.t
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
    | CoFix of meta_stack
    | CoPack of {
        cons : ConsVar.t;
        bind : val_bind;
        pack_vars : (TyVar.t * sort) list;
        cmd : command
      }
    | CoSpec of DestrVar.t * typ list * meta_stack
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
      conttyp : typ;
      cont : CoVar.t;
      loc : position;
      content : command;
    }

  type program = prelude * prog_item list

  let dummy_val_meta v = MetaVal {
      node = v;
      loc = dummy_pos;
      val_typ = tvar (TyVar.fresh ())
    }

  let dummy_stack_meta s = MetaStack {
      node = s;
      loc = dummy_pos;
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
    let ret a = dummy_stack_meta (Ret a)
    let bind pol bind cmd = dummy_stack_meta (CoBind {pol; bind; cmd})
    let box kind stk = dummy_stack_meta (CoBox {kind; stk})
    let destr c = dummy_stack_meta (CoDestr c)
    let case l = dummy_stack_meta (CoCons l)
  end

  let empty_prelude = {
    sort_defs = SortVar.Env.empty;
    tycons = TyConsVar.Env.empty;
    cons = ConsVar.Env.empty;
    destr = DestrVar.Env.empty;
    vars = Var.Env.empty;
    covars = CoVar.Env.empty;
    sorts = TyVar.Env.empty;
    var_multiplicities = Var.Env.empty;
    covar_multiplicities = CoVar.Env.empty
  }

end

module FullAst = Ast (FullAstParams)
