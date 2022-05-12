open Vars
open Constructors

type ill_sorts = PosType | NegType | NeuType
type ill_boxes = Linear | Affine | Exponential

module type ITypes = sig

  type sort
  type box_kind
  type postype
  type negtype
  type typ

  val linear : box_kind
  val affine : box_kind
  val exp : box_kind

  val sort_postype : sort
  val sort_negtype : sort
  val sort_neuttype : sort

  val pos : postype -> typ
  val neg : negtype -> typ
  val tvar : TyVar.t -> typ
  val posvar : TyVar.t -> postype
  val negvar : TyVar.t -> negtype
  val boxed : box_kind -> typ -> postype
  val data : typ pos_type_cons -> postype
  val codata : typ neg_type_cons -> negtype

  val string_of_sort : sort -> string
  val string_of_box_kind : box_kind -> string
  val string_of_type : typ -> string
  val string_of_postype : postype -> string
  val string_of_negtype : negtype -> string
  val string_of_binding : Var.t * typ -> string
  val string_of_cobinding : CoVar.t * typ -> string

end


module type IPreTypes =
  sig
    type pretyp
    include ITypes
      with type box_kind = ill_boxes
       and type sort = ill_sorts
       and type negtype = pretyp
       and type postype = pretyp
       and type typ = pretyp
    val omitted : pretyp
end

module FullTypes : ITypes

module PreTypes  : IPreTypes
