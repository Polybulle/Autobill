open Vars
open Util
open Constructors

type 'var pre_polarity = Positive | Negative | PVar of 'var
type polarity = PolVar.t pre_polarity
type box_kind = Linear | Affine | Exponential
type ('pvar, 'var) pre_sort = Base of 'pvar pre_polarity | SortVar of 'var
type sort = (PolVar.t, SortVar.t) pre_sort

let linear = Linear
let affine = Affine
let exp = Exponential

let positive = Positive
let negative = Negative
let pvar v = PVar v

let sort_postype = Base Positive
let sort_negtype = Base Negative
let sort_var v = SortVar v
let sort_base p = Base p

let string_of_polarity = function
  | Positive -> "+"
  | Negative -> "-"
  | PVar n -> "~" ^ PolVar.to_string n
let string_of_pre_sorts k = function
  | Base p -> k p
  | SortVar v -> SortVar.to_string v
let string_of_box_kind = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"
let string_of_sort = string_of_pre_sorts string_of_polarity


type 'var pre_typ =
  | TCons of {node : ('var, 'var pre_typ) type_cons; loc : position}
  | TBox of {kind : box_kind; node : 'var pre_typ; loc : position}
  | TVar of {node : 'var; loc : position}
  | TPos of 'var pre_typ
  | TNeg of 'var pre_typ
  | TInternal of 'var
type typ = TyVar.t pre_typ


let pos t = TPos t
let neg t = TNeg t
let tvar ?loc:(loc = dummy_pos) node = TVar {node; loc}
let posvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let negvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let boxed ?loc:(loc = dummy_pos) kind node = TBox {kind; node; loc}
let cons ?loc:(loc = dummy_pos) node = TCons {node; loc}

let rec string_of_type string_of_var = function
  | TVar v -> string_of_var v.node
  | TPos t -> "+" ^ string_of_type string_of_var t
  | TNeg t -> "-" ^ string_of_type string_of_var t
  | TInternal n -> "<" ^ string_of_var n ^ ">"
  | TCons dat -> string_of_type_cons string_of_var (string_of_type string_of_var) dat.node
  | TBox box -> Printf.sprintf "(%s %s)"
                  (string_of_box_kind box.kind)
                  (string_of_type string_of_var box.node)
