open Vars
open Util
open Constructors

type polarity = Positive | Negative
type box_kind = Linear | Affine | Exponential
type 'var sort =
  | Base of polarity
  | Index of 'var

let linear = Linear
let affine = Affine
let exp = Exponential

let positive = Positive
let negative = Negative

let sort_postype = Base Positive
let sort_negtype = Base Negative
let sort_base p = Base p
let sort_idx i = Index i

let string_of_polarity  = function
  | Positive -> "+"
  | Negative -> "-"
let string_of_sort = function
  | Base p -> string_of_polarity p
  | Index i -> SortVar.to_string i
let string_of_box_kind = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"


type ('tycons, 'var) pre_typ =
  | TCons of {node : ('tycons, ('tycons, 'var) pre_typ) type_cons; loc : position}
  | TBox of {kind : box_kind; node : ('tycons, 'var) pre_typ; loc : position}
  | TVar of {node : 'var; loc : position}
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
let cons ?loc:(loc = dummy_pos) node = TCons {node; loc}

let rec string_of_type string_of_cons string_of_var = function
  | TVar v -> string_of_var v.node
  | TPos t -> "+" ^ string_of_type string_of_cons string_of_var t
  | TNeg t -> "-" ^ string_of_type string_of_cons string_of_var t
  | TInternal n -> "<" ^ string_of_var n ^ ">"
  | TFix t -> "(fix " ^ string_of_type string_of_cons string_of_var t ^ ")"
  | TCons dat -> string_of_type_cons
                   string_of_cons
                   (string_of_type string_of_cons string_of_var)
                   dat.node
  | TBox box -> Printf.sprintf "(%s %s)"
                  (string_of_box_kind box.kind)
                  (string_of_type string_of_cons string_of_var box.node)
