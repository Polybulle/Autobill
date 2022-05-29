open Vars
open Util
open Constructors

type box_kind = Linear | Affine | Exponential

type 'var pre_polarity = Positive | Negative | PVar of 'var
type polarity = PolVar.t pre_polarity
type ('pvar, 'var) pre_sort =
  | Base of 'pvar pre_polarity
  | Dep of ('pvar, 'var) pre_sort * ('pvar, 'var) pre_sort
  | SortVar of 'var
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

val string_of_pre_polarity : ('a -> string) -> 'a pre_polarity -> string
val string_of_box_kind : box_kind -> string
val string_of_pre_sorts :
  ('a pre_polarity -> string) ->
  ('b -> string) ->
  ('a, 'b) pre_sort ->
  string

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
