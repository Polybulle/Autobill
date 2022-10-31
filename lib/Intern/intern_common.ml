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

exception Polarity_mismatch of string * string * position * position

let fail_double_def mess loc =
  raise (Double_definition
           (Printf.sprintf "%s: FATAL the %s is already defined"
              (Util.string_of_position loc)
              mess))

type upol =
  | Loc of position * upol
  | Litt of Types.polarity
  | Redirect of PolVar.t

let rec string_of_upol = function
  | Loc (pos, upol) -> Printf.sprintf "%s@\"%s\"" (string_of_upol upol)  (string_of_position pos)
  | Litt pol -> string_of_polarity pol
  | Redirect var -> PolVar.to_string var

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

let fail_polarity_mismatch upol1 upol2 pos1 pos2 =
  raise (Polarity_mismatch (string_of_upol upol1, string_of_upol upol2, pos1, pos2))


module InternAstParams = struct
  include FullAstParams
  type polarity = upol
end

module InternAst = Ast (InternAstParams)

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

type sort_check_env = {
  prelude : prelude;

  tycons_vars : TyConsVar.t StringEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
  definitions: DefVar.t StringEnv.t;

  tycons_sort : (sort list * sort) TyConsVar.Env.t;
  prelude_typevar_sort : sort TyVar.Env.t;

  varpols : PolVar.t Var.Env.t;
  covarpols : PolVar.t CoVar.Env.t;
  tyvarpols : upol TyVar.Env.t;
  unifier : upol PolVar.Env.t;
  }


let empty_sortcheck = {
  prelude = InternAst.empty_prelude;

  tycons_vars = StringEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
  definitions = StringEnv.empty;

  tycons_sort = TyConsVar.Env.empty;
  prelude_typevar_sort = TyVar.Env.empty;

  varpols = Var.Env.empty;
  covarpols = CoVar.Env.empty;
  tyvarpols = TyVar.Env.empty;
(* INVARIANT: if a value in 'unifier' as a Loc node, then it is at the root.
   This implies it must be the only one appearing in the value *)
  unifier = PolVar.Env.empty
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

let get_var scope v = StringEnv.find v scope.vars

let get_covar scope a = StringEnv.find a scope.covars

let get_tyvar scope t = StringEnv.find t scope.tyvars

let empty_scope = { vars = StringEnv.empty;
                    covars = StringEnv.empty;
                    tyvars = StringEnv.empty}



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
