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

module FullTypes (MyVars : AllVars) (Constructors : Constructors) = struct

  module MyVars = MyVars
  module Constructors = Constructors

  open Constructors
  open MyVars

  type sort = Type | PosType | NegType
  type box_kind = ill_boxes

  type postype =
    | PVar of PosVar.t
    | Boxed of typ * box_kind
    | Data of typ pos_type_cons
  and negtype =
    | NVar of NegVar.t
    | CoData of typ neg_type_cons
  and typ =
    | TVar of TyVar.t
    | TPos of postype
    | TNeg of negtype

  let pos p = TPos p
  let neg n = TNeg n
  let tvar a = TVar a
  let posvar a = PVar a
  let negvar a = NVar a
  let boxed k t = Boxed (t, k)
  let data c = Data c
  let codata d = CoData d

  let string_of_sort = function
    | Type -> "*"
    | PosType -> "+"
    | NegType -> "~"

  let string_of_box_kind = function
    | Linear -> "lin"
    | Affine -> "aff"
    | Exponential -> "exp"

  let rec string_of_type = function
    | TVar v -> TyVar.to_string v
    | TPos p -> string_of_postype p
    | TNeg n -> string_of_negtype n
  and string_of_postype = function
    | PVar v -> PosVar.to_string v
    | Boxed (t, box) -> pp_texp (string_of_box_kind box) [string_of_type t]
    | Data c -> string_of_pos_type_cons string_of_type c
  and string_of_negtype = function
    | NVar v -> NegVar.to_string v
    | CoData c -> string_of_neg_type_cons string_of_type c

  let typ_annot s = if String.get s 0 = '{' then s else "{" ^ s ^ "}"

  let string_of_binding (v,t) =
    (Var.to_string v) ^ " " ^ typ_annot (string_of_type t)
  let string_of_cobinding (a,t) =
    (CoVar.to_string a) ^ " " ^ typ_annot (string_of_type t)
end
