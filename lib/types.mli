open Vars
open Util

type box_kind = Linear | Affine | Exponential
type extended_polarity = [`Positive | `Negative | `Ambiguous]
type sort

val linear : box_kind
val affine : box_kind
val exp : box_kind
val positive : extended_polarity
val negative : extended_polarity
val ambiguous : extended_polarity
val sort_postype : sort
val sort_negtype : sort
val sort_neuttype : sort
val sort_base : extended_polarity -> sort
val string_of_polarity : extended_polarity -> string
val string_of_sort : sort -> string
val string_of_box_kind : box_kind -> string

type 't type_cons =
    Unit
  | Zero
  | Top
  | Bottom
  | Prod of 't * 't
  | Sum of 't * 't
  | Fun of 't * 't
  | Choice of 't * 't
  | Cons of TyVar.t * 't list

type typ =
  | TCons of {node : typ type_cons; loc : position}
  | TBox of {kind : box_kind; node : typ; loc : position}
  | TVar of {node : TyVar.t; loc : position}
  | TPos of typ
  | TNeg of typ

val pos : typ -> typ
val neg : typ -> typ
val tvar : ?loc:position -> TyVar.t -> typ
val posvar : ?loc:position -> TyVar.t -> typ
val negvar : ?loc:position -> TyVar.t -> typ
val boxed : ?loc:position -> box_kind -> typ -> typ
val cons : ?loc:position -> typ type_cons -> typ

val unit_t : 'a type_cons
val zero : 'a type_cons
val top : 'a type_cons
val bottom : 'a type_cons
val prod : 'a -> 'a -> 'a type_cons
val sum : 'a -> 'a -> 'a type_cons
val func : 'a -> 'a -> 'a type_cons
val choice : 'a -> 'a -> 'a type_cons
val typecons : TyVar.t -> 'a list -> 'a type_cons

val string_of_type_cons : ('a -> string) -> 'a type_cons -> string
val string_of_type : typ -> string
