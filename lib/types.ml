open Misc
open Vars

type polarity = Positive | Negative
type box_kind = Linear | Affine | Exponential
type 'var sort =
  | Base of polarity
  | Index of 'var
  | Arrow of 'var sort * 'var sort

let linear = Linear
let affine = Affine
let exp = Exponential

let positive = Positive
let negative = Negative

let sort_postype = Base Positive
let sort_negtype = Base Negative
let sort_base p = Base p
let sort_idx i = Index i
let sort_arrow arg ret = List.fold_right (fun arg so -> Arrow (arg,so)) arg ret

let rec unmk_arrow sort = match sort with
  | Arrow (s,t) -> let (args,ret) = unmk_arrow t in (s::args,ret)
  | _ -> ([], sort)

let is_base_sort = function
  | Base _ -> true
  | _ -> false

let rec is_index_sort = function
  | Base _ -> false
  | Index _ -> true
  | Arrow (s,t) -> is_index_sort s && is_index_sort t

let rec is_monotype_sort = function
  | Base _ -> true
  | Index _ -> false
  | Arrow (s,t) -> is_index_sort s && is_monotype_sort t

let rec is_polytype_sort = function
  | Base _ -> true
  | Index _ -> false
  | Arrow _ as so when is_monotype_sort so -> true
  | Arrow (s,t) -> is_monotype_sort s && is_polytype_sort t

let is_valid_sort so = is_index_sort so || is_polytype_sort so

let string_of_polarity  = function
  | Positive -> "+"
  | Negative -> "-"
let rec string_of_sort kv = function
  | Base p -> string_of_polarity p
  | Index i -> kv i
  | Arrow (s,t) -> "(" ^ (string_of_sort kv s) ^ " -> " ^ (string_of_sort kv t) ^")"
let string_of_box_kind = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"

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


let type_cons_names =
  ["unit"; "zero"; "top"; "bottom"; "prod"; "sum"; "fun"; "choice"; "thunk"; "closure"]

type 'tycons type_cons =
  | Unit
  | Zero
  | Top
  | Bottom
  | Thunk
  | Closure
  | Prod of int
  | Sum of int
  | Fun of int
  | Choice of int
  | Cons of 'tycons

let string_of_type_cons kvar cons =

  match cons with
  | Unit -> "unit"
  | Zero -> "zero"
  | Top -> "top"
  | Bottom -> "bottom"
  | Thunk -> "thunk"
  | Closure -> "closure"
  | Prod _ -> "prod"
  | Sum _ -> "sum"
  | Fun _ -> "fun"
  | Choice _ -> "choice"
  | Cons var -> (kvar var)


type ('tycons, 'var) pre_typ =
  | TCons of {node : 'tycons type_cons;
              loc : position}
  | TApp of {tfun : ('tycons, 'var) pre_typ;
             args : ('tycons, 'var) pre_typ list;
             loc : position}
  | TBox of {kind : box_kind;
             node : ('tycons, 'var) pre_typ;
             loc : position}
  | TVar of {node : 'var;
             loc : position}
  | TFix of ('tycons, 'var) pre_typ
  | TPos of ('tycons, 'var) pre_typ
  | TNeg of ('tycons, 'var) pre_typ
  | TInternal of 'var
type typ = (TyConsVar.t, TyVar.t) pre_typ


let pos t = TPos t
let neg t = TNeg t
let tvar ?loc:(loc = dummy_pos) node = TVar {node; loc}
let posvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let negvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let boxed ?loc:(loc = dummy_pos) kind node = TBox {kind; node; loc}
let fix t = TFix t
let cons ?loc:(loc = dummy_pos) node = TCons {node; loc}
let app ?loc:(loc = dummy_pos) tfun args = TApp {tfun; args; loc}

let unit_t = cons Unit
let zero = cons Zero
let top = cons Top
let bottom = cons Bottom
let prod ts = app (cons (Prod (List.length ts))) ts
let sum ts = app (cons (Sum (List.length ts))) ts
let func ts = app (cons (Fun (List.length ts - 1))) ts
let choice ts = app (cons (Choice (List.length ts))) ts
let typecons v args = app (cons (Cons v)) args
let thunk_t t = app (cons Thunk) [t]
let closure_t t = app (cons Closure) [t]

let rec string_of_type string_of_cons string_of_var = function
  | TVar v -> string_of_var v.node
  | TPos t -> "+" ^ string_of_type string_of_cons string_of_var t
  | TNeg t -> "-" ^ string_of_type string_of_cons string_of_var t
  | TInternal n -> string_of_var n
  | TFix t -> "(fix " ^ string_of_type string_of_cons string_of_var t ^ ")"
  | TCons {node;_} -> string_of_type_cons string_of_cons node
  | TBox box -> Printf.sprintf "(%s %s)"
                  (string_of_box_kind box.kind)
                  (string_of_type string_of_cons string_of_var box.node)
  | TApp {tfun; args;_} ->
    let tfun = string_of_type string_of_cons string_of_var tfun in
    match args with
    | [] -> tfun
    | arg :: args ->
      let arg = string_of_type string_of_cons string_of_var arg in
      let args = List.fold_left
          (fun str arg' -> str ^ " " ^ string_of_type string_of_cons string_of_var arg')
          arg
          args in
      Printf.sprintf "(%s %s)" tfun args
