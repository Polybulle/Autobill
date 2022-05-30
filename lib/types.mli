open Vars
open Util
open Constructors

type box_kind = Linear | Affine | Exponential

type polarity = Positive | Negative
type sort =
  | Base of polarity
  | Dep of sort * sort

val linear : box_kind
val affine : box_kind
val exp : box_kind

val positive : polarity
val negative : polarity

val sort_postype : sort
val sort_negtype : sort
val sort_base : polarity -> sort
val sort_dep : sort -> sort -> sort

val string_of_polarity : polarity -> string
val string_of_box_kind : box_kind -> string
val string_of_sort : sort -> string

type ('tycons, 'var) pre_typ =
  | TCons of {node : ('tycons, ('tycons, 'var) pre_typ) type_cons; loc : position}
  | TBox of {kind : box_kind; node : ('tycons, 'var) pre_typ; loc : position}
  | TVar of {node : 'var; loc : position}
  | TPos of ('tycons, 'var) pre_typ
  | TNeg of ('tycons, 'var) pre_typ
  | TInternal of 'var
type typ = (TyConsVar.t, TyVar.t) pre_typ

val pos : ('tycons, 'a) pre_typ -> ('tycons, 'a) pre_typ
val neg : ('tycons, 'a) pre_typ -> ('tycons, 'a) pre_typ
val tvar : ?loc:position -> 'a -> ('tycons, 'a) pre_typ
val posvar : ?loc:position -> 'a -> ('tycons, 'a) pre_typ
val negvar : ?loc:position -> 'a -> ('tycons, 'a) pre_typ
val boxed : ?loc:position -> box_kind -> ('tycons, 'a) pre_typ -> ('tycons, 'a) pre_typ
val cons : ?loc:position -> ('tycons, ('tycons, 'a) pre_typ) type_cons -> ('tycons, 'a) pre_typ

val string_of_type : ('tycons -> string) -> ('a -> string) -> ('tycons, 'a) pre_typ -> string
