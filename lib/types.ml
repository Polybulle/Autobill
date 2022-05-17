open Vars


type polarity = [`Positive | `Negative]
type extended_polarity = [`Positive | `Negative | `Ambiguous]
type box_kind = Linear | Affine | Exponential
type 'p pre_sort = Base of 'p
type sort = extended_polarity pre_sort

let linear = Linear
let affine = Affine
let exp = Exponential
let positive = `Positive
let negative = `Negative
let ambiguous = `Ambiguous
let sort_postype : sort = Base `Positive
let sort_negtype : sort = Base `Negative
let sort_neuttype : sort = Base `Ambiguous
let sort_base p = Base p

let string_of_polarity = function
  | `Positive -> "(+)"
  | `Negative -> "(-)"
  | `Ambiguous -> "(~)"
let string_of_pre_sorts k = function
  | Base p -> k p
let string_of_box_kind = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"
let string_of_sort = string_of_pre_sorts string_of_polarity


type 't type_cons =
  | Unit
  | Zero
  | Top
  | Bottom
  | Prod of 't * 't
  | Sum of 't * 't
  | Fun of 't * 't
  | Choice of 't * 't
  | Cons of Vars.TyVar.t * 't list

type typ =
  | TCons of typ type_cons
  | TBox of box_kind * typ
  | TVar of TyVar.t
  | TPos of typ
  | TNeg of typ

let pos t = TPos t
let neg t = TNeg t
let tvar v = TVar v
let posvar v = tvar v
let negvar v = tvar v
let boxed k t = TBox (k,t)
let cons dat = TCons dat

let unit_t = Unit
let zero = Zero
let top = Top
let bottom = Bottom
let prod a b = Prod (a,b)
let sum a b = Sum (a,b)
let func a b = Fun (a,b)
let choice a b = Choice (a,b)
let typecons v args = Cons (v,args)

let pp_texp cons args =
  Util.paren (List.fold_left (fun a b -> a ^ " " ^ b) cons args)

let string_of_type_cons k = function
  | Unit -> "unit"
  | Zero -> "zero"
  | Top -> "top"
  | Bottom -> "bottom"
  | Prod (a,b) -> pp_texp "prod" [k a; k b]
  | Sum (a,b) -> pp_texp "sum" [k a; k b]
  | Fun (a,b) -> pp_texp "fun" [k a; k b]
  | Choice (a,b) -> pp_texp "choice" [k a; k b]
  | Cons (var,args) -> pp_texp (Vars.TyVar.to_string var) (List.map k args)

let rec string_of_type = function
  | TVar v -> TyVar.to_string v
  | TPos t -> "+" ^ string_of_type t
  | TNeg t -> "-" ^ string_of_type t
  | TCons dat -> string_of_type_cons string_of_type dat
  | TBox (k,t) -> pp_texp "+box" [string_of_box_kind k; string_of_type t]
