open Vars

type box_kind = Linear | Affine | Exponential
type extended_polarity = [`Positive | `Negative | `Ambiguous]
type sort

val linear : box_kind
val affine : box_kind
val exp : box_kind
val sort_postype : sort
val sort_negtype : sort
val sort_neuttype : sort
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
    TCons of typ type_cons
  | TBox of box_kind * typ
  | TVar of TyVar.t
  | TPos of typ
  | TNeg of typ

val pos : typ -> typ
val neg : typ -> typ
val tvar : TyVar.t -> typ
val posvar : TyVar.t -> typ
val negvar : TyVar.t -> typ
val boxed : box_kind -> typ -> typ
val cons : typ type_cons -> typ

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
val string_of_typ_annot : typ option -> string
val string_of_binding : Var.t * typ option -> string
val string_of_cobinding : CoVar.t * typ option -> string
