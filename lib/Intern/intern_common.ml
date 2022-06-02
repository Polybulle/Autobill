open Util
open Types
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


type upol =
  | Redirect of PolVar.t
  | UPos
  | UNeg

module InternAstParams = struct
  include FullAstParams
  type polarity = upol
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
  tycons_vars : TyConsVar.t StringEnv.t;
  type_vars : TyVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : sort TyConsEnv.t;
  prelude_typevar_sort : sort TyVarEnv.t;
  }

let upol_pos = PolVar.fresh ()
let upol_neg = PolVar.fresh ()

let empty_sortcheck = {
    tycons_vars = StringEnv.empty;
    type_vars = StringEnv.empty;
    conses = StringEnv.empty;
    destrs = StringEnv.empty;
    definitions = StringEnv.empty;

    tycons_sort = TyConsEnv.empty;
    prelude_typevar_sort = TyVarEnv.empty;
  }
