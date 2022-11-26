open Misc
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

exception Undefined_sort of {
    name : string;
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

exception Sort_mismatch of string * string * position * position

let fail_double_def mess loc =
  raise (Double_definition
           (Printf.sprintf "%s: FATAL the %s is already defined"
              (string_of_position loc)
              mess))

let fail_bad_sort loc expected actual =
  raise (Bad_sort {loc; actual; expected})

let fail_undefined_type name loc =
  raise (Undefined_type {name; loc})

let fail_undefined_sort name =
  raise (Undefined_sort {name})

let fail_bad_arity cons loc =
  raise (Bad_type_cons_arity {cons; loc})

let fail_bad_constructor loc =
  raise (Bad_constructor_name {loc})

let fail_higher_order_arg name loc =
  raise (Higher_order_type_argument {name; loc})

let fail_ambiguous_sort loc = raise (Ambiguous_polarity loc)

let fail_undefined_var var loc = raise (Undefined_var (var, loc))

let fail_undefined_cons cons loc = raise (Undefined_constructor (cons, loc))

let fail_undefined_destr destr loc = raise (Undefined_destructor (destr, loc))



module USortVar = LocalVar (struct
    let default_name = "pol"
  end)

type usort =
  | Loc of position * usort
  | Litt of SortVar.t Types.sort
  | Redirect of USortVar.t

let rec string_of_usort = function
  | Loc (pos, upol) -> Printf.sprintf "%s@\"%s\"" (string_of_usort upol)  (string_of_position pos)
  | Litt so -> string_of_sort SortVar.to_string so
  | Redirect var -> USortVar.to_string var

let fail_polarity_mismatch upol1 upol2 pos1 pos2 =
  raise (Sort_mismatch (string_of_usort upol1, string_of_usort upol2, pos1, pos2))


module InternAstParams = struct
  include FullAstParams
  type polarity = usort
end

module InternAst = Ast (InternAstParams)

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

type sort_check_env = {
  prelude : prelude;

  sort_vars : SortVar.t StringEnv.t;
  tycons_vars : TyConsVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : sort TyConsVar.Env.t;
  prelude_typevar_sort : sort TyVar.Env.t;

  varsorts : USortVar.t Var.Env.t;
  covarsorts : USortVar.t CoVar.Env.t;
  tyvarsorts : usort TyVar.Env.t;
  unifier : usort USortVar.Env.t;
  }


let empty_sortcheck = {
  prelude = InternAst.empty_prelude;

  sort_vars = StringEnv.empty;
  tycons_vars = StringEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
  definitions = StringEnv.empty;

  tycons_sort = TyConsVar.Env.empty;
  prelude_typevar_sort = TyVar.Env.empty;

  varsorts = Var.Env.empty;
  covarsorts = CoVar.Env.empty;
  tyvarsorts = TyVar.Env.empty;
(* INVARIANT: if a value in 'unifier' as a Loc node, then it is at the root.
   This implies it must be the only one appearing in the value *)
  unifier = USortVar.Env.empty
}

type scope = {
  vars : Var.t StringEnv.t;
  covars : CoVar.t StringEnv.t;
  tyvars : TyVar.t StringEnv.t;
}

let add_var scope v =
  {scope with vars = StringEnv.add v (Var.of_string v) scope.vars}

let add_covar scope a =
  {scope with covars = StringEnv.add a (CoVar.of_string a) scope.covars}

let add_tyvar scope t =
  {scope with tyvars = StringEnv.add t (TyVar.of_string t) scope.tyvars}

let add_sort scope t =
  {scope with tyvars = StringEnv.add t (TyVar.of_string t) scope.tyvars}

let get_var scope v = StringEnv.find v scope.vars

let get_covar scope a = StringEnv.find a scope.covars

let get_tyvar scope t = StringEnv.find t scope.tyvars


let empty_scope = {
  vars = StringEnv.empty;
  covars = StringEnv.empty;
  tyvars = StringEnv.empty;
}


let rec intern_sort env = function
  | Base p -> Base p
  | Arrow (s,t) -> Arrow (intern_sort env s, intern_sort env t)
  | Index i ->
    try Index (StringEnv.find i env.sort_vars) with
    | Not_found -> fail_undefined_sort i

let rec intern_type env scope = function

  | TVar {node; loc} ->
    begin
      try TCons {node = Cons (StringEnv.find node env.tycons_vars, []); loc}
      with
        Not_found ->
        try TVar {node = get_tyvar scope node; loc}
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
        | Thunk a -> aux (thunk_t (intern_type env scope a))
        | Closure a -> aux (closure_t (intern_type env scope a))
        | Prod ts -> aux (Prod (List.map (intern_type env scope) ts))
        | Sum ts -> aux (Sum (List.map (intern_type env scope) ts))
        | Fun (a,b) -> aux (Fun (List.map (intern_type env scope) a,
                                 intern_type env scope b))
        | Choice ts -> aux (Choice (List.map (intern_type env scope) ts))
        | Cons (cons, args) ->
          let name =
            try StringEnv.find cons env.tycons_vars
            with _ -> fail_undefined_type cons loc in
          aux (typecons name (List.map (intern_type env scope) args))
      end

  | TInternal var -> intern_type env scope (TVar {node = var; loc = dummy_pos})

  | TPos t -> intern_type env scope t

  | TNeg t -> intern_type env scope t

  | TBox box -> TBox {box with node=intern_type env scope box.node}

  | TFix t -> TFix (intern_type env scope t)
