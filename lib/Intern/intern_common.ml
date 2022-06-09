open Util
open Types
open Constructors
open Vars
open Ast

exception Double_definition of string

exception Bad_sort of {
    loc : position;
    actual : sort;
    expected : sort;
  }

exception Undefined_type of {
    name : string;
    loc : position;
  }

exception Bad_type_cons_arity of {
    cons : string;
    loc : position;
  }

exception Bad_constructor_name of {
    loc : position
  }

exception Higher_order_type_argument of {
    loc : position;
    name : string
  }

exception Ambiguous_polarity of position

exception Undefined_var of string * position

exception Undefined_constructor of string * position

exception Undefined_destructor of string * position

exception Polarity_mismatch of position * position

let fail_double_def mess loc =
  raise (Double_definition
           (Printf.sprintf "%s: FATAL the %s is already defined"
              (Util.string_of_position loc)
              mess))

let fail_bad_sort loc expected actual =
  raise (Bad_sort {loc; actual; expected})

let fail_undefined_type name loc =
  raise (Undefined_type {name; loc})

let fail_bad_arity cons loc =
  raise (Bad_type_cons_arity {cons; loc})

let fail_bad_constructor loc =
  raise (Bad_constructor_name {loc})

let fail_higher_order_arg name loc =
  raise (Higher_order_type_argument {name; loc})

let fail_ambiguous_polarity loc = raise (Ambiguous_polarity loc)

let fail_undefined_var var loc = raise (Undefined_var (var, loc))

let fail_undefined_cons cons loc = raise (Undefined_constructor (cons, loc))

let fail_undefined_destr destr loc = raise (Undefined_destructor (destr, loc))

let fail_polarity_mismatch pos1 pos2 = raise (Polarity_mismatch (pos1, pos2))


type upol =
  | Loc of position * upol
  | Litt of Types.polarity
  | Redirect of PolVar.t

module InternAstParams = struct
  include FullAstParams
  type polarity = upol
  type cont_bind = upol * typ
end

module InternAst = Ast (InternAstParams)

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

module TyVarEnv = Map.Make (struct
    type t = TyVar.t
    let compare = compare
  end)

module PolVarEnv = Map.Make (struct
    type t = PolVar.t
    let compare = compare
end)

module VarEnv = Map.Make (struct
    type t = Var.t
    let compare = compare
  end)

type sort_check_env = {
  prelude : prelude;

  tycons_vars : TyConsVar.t StringEnv.t;
  type_vars : TyVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : sort TyConsEnv.t;
  prelude_typevar_sort : sort TyVarEnv.t;

  varpols : PolVar.t VarEnv.t;
  tyvarpols : upol TyVarEnv.t;
  unifier : upol PolVarEnv.t;
  }

let upol_pos = PolVar.fresh ()
let upol_neg = PolVar.fresh ()

let empty_sortcheck = {
  prelude = InternAst.empty_prelude;

  tycons_vars = StringEnv.empty;
  type_vars = StringEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
  definitions = StringEnv.empty;

  tycons_sort = TyConsEnv.empty;
  prelude_typevar_sort = TyVarEnv.empty;

  varpols = VarEnv.empty;
  tyvarpols = TyVarEnv.empty;
(* INVARIANT: if a value in 'unifier' as a Loc node, then it is at the root.
   This implies it must be the only one appearing in the value *)
  unifier = PolVarEnv.empty
}


let rec intern_type env = function

  | TVar {node; loc} ->
    begin
      try TCons {node = Cons (StringEnv.find node env.tycons_vars, []); loc}
      with
        Not_found ->
        try TVar {node = StringEnv.find node env.type_vars; loc}
        with
          Not_found -> fail_undefined_type node loc
    end

  | TCons {node; loc} ->
      let aux output = TCons {loc; node = output} in
      begin match node with
        | Unit -> aux unit_t
        | Zero -> aux zero
        | Top -> aux top
        | Bottom -> aux bottom
        | ShiftPos a -> aux (shift_pos_t (intern_type env a))
        | ShiftNeg a -> aux (shift_neg_t (intern_type env a))
        | Prod (a,b) -> aux (prod (intern_type env a) (intern_type env b))
        | Sum (a,b) -> aux (sum (intern_type env a) (intern_type env b))
        | Fun (a,b) -> aux (func (intern_type env a) (intern_type env b))
        | Choice (a,b) -> aux (choice (intern_type env a) (intern_type env b))
        | Cons (cons, args) ->
          let name =
            try StringEnv.find cons env.tycons_vars
            with _ -> fail_undefined_type cons loc in
          aux (typecons name (List.map (intern_type env) args))
      end

  | TInternal var -> intern_type env (TVar {node = var; loc = dummy_pos})

  | TPos t -> intern_type env t

  | TNeg t -> intern_type env t

  | TBox {node; _} -> intern_type env node
