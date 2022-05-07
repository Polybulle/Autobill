open Vars
open Constructors

module type ITypes = sig

  module MyVars : AllVars
  module Constructors : Constructors

  open MyVars
  open Constructors

  type sort
  type box_kind
  type postype
  type negtype
  type typ

  val pos : postype -> typ
  val neg : negtype -> typ
  val tvar : TyVar.t -> typ
  val posvar : PosVar.t -> postype
  val negvar : NegVar.t -> negtype
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

type ill_boxes = Linear | Affine | Exponential
type string_boxes = Box of string

module PreTypes : sig
  include ITypes
    with module MyVars = StringVar
     and module Constructors = TextConstructors
     and type box_kind = string_boxes
end

module FullTypes (MyVars : AllVars) (Constructors : Constructors) : sig
  include ITypes
    with module MyVars = MyVars
     and module Constructors = Constructors
     and type box_kind = ill_boxes
  end
