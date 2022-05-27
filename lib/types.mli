open Vars
open Util
open Constructors

type box_kind = Linear | Affine | Exponential

type 'var pre_polarity = Positive | Negative | PVar of 'var
type polarity = PolVar.t pre_polarity

type ('pvar, 'var) pre_sort = Base of 'pvar pre_polarity | SortVar of 'var
type sort = (PolVar.t, SortVar.t) pre_sort

val linear : box_kind
val affine : box_kind
val exp : box_kind
val positive : 'a pre_polarity
val negative : 'a pre_polarity
val pvar : 'a -> 'a pre_polarity
val sort_postype : ('a, 'b) pre_sort
val sort_negtype : ('a, 'b) pre_sort
val sort_base : 'a pre_polarity -> ('a, 'b) pre_sort
val sort_var : 'b -> ('a, 'b) pre_sort

val string_of_polarity : polarity -> string
val string_of_sort : sort -> string
val string_of_box_kind : box_kind -> string

type 'var pre_typ =
  | TCons of {node : ('var, 'var pre_typ) type_cons; loc : position}
  | TBox of {kind : box_kind; node : 'var pre_typ; loc : position}
  | TVar of {node : 'var; loc : position}
  | TPos of 'var pre_typ
  | TNeg of 'var pre_typ
  | TInternal of 'var
type typ = TyVar.t pre_typ

val pos : 'a pre_typ -> 'a pre_typ
val neg : 'a pre_typ -> 'a pre_typ
val tvar : ?loc:position -> 'a -> 'a pre_typ
val posvar : ?loc:position -> 'a -> 'a pre_typ
val negvar : ?loc:position -> 'a -> 'a pre_typ
val boxed : ?loc:position -> box_kind -> 'a pre_typ -> 'a pre_typ
val cons : ?loc:position -> ('a, 'a pre_typ) type_cons -> 'a pre_typ

val string_of_type : ('a -> string) -> 'a pre_typ -> string
