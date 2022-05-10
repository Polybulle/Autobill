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

  val linear : box_kind
  val affine : box_kind
  val exp : box_kind

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

type ill_sorts = PosType | NegType
type ill_boxes = Linear | Affine | Exponential
type string_boxes = Box of string

let string_of_ill_sorts = function
  | PosType -> "+"
  | NegType -> "~"
let string_of_ill_boxes = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"

let typ_annot s = if String.get s 0 = '{' then s else "{" ^ s ^ "}"

module PreTypes (MyVars : AllVars) (Constructors : Constructors) = struct

  module MyVars = MyVars
  module Constructors = Constructors

  type sort = ill_sorts
  type box_kind = string_boxes
  type typ =
    | TPosCons of typ Constructors.pos_type_cons
    | TNegCons of typ Constructors.neg_type_cons
    | TBox of box_kind * typ
    | TVar of MyVars.TyVar.t
    | TPos of typ
    | TNeg of typ
    | TOmited
  type postype = typ
  type negtype = typ

  let linear = Box "linear"
  let affine = Box "affine"
  let exp = Box "exp"

  let pos t = TPos t
  let neg t = TNeg t
  let tvar v = TVar v
  let posvar v = pos (tvar v)
  let negvar v = neg (tvar v)
  let boxed k t = TBox (k,t)
  let data dat = TPosCons dat
  let codata dat = TNegCons dat

  let string_of_sort = string_of_ill_sorts
  let string_of_box_kind (Box s) = s
  let string_of_tvar v = MyVars.TyVar.to_string v
  let rec string_of_type = function
    | TVar v -> "?" ^ string_of_tvar v
    | TPos (TVar v) -> "+" ^ string_of_tvar v
    | TNeg (TVar v) -> "~" ^ string_of_tvar v
    | TPos t | TNeg t -> string_of_type t
    | TPosCons dat -> Constructors.string_of_pos_type_cons string_of_type dat
    | TNegCons dat -> Constructors.string_of_neg_type_cons string_of_type dat
    | TBox (Box k,t) -> pp_texp "box" [k; string_of_type t]
    | TOmited -> "_omitted"
  let string_of_postype = string_of_type
  let string_of_negtype = string_of_type
  let string_of_binding (v,t) =
    MyVars.Var.to_string v ^ " " ^ typ_annot (string_of_type t)
  let string_of_cobinding (a,t) =
    MyVars.CoVar.to_string a ^ " " ^ typ_annot (string_of_type t)
end

module FullTypes (MyVars : AllVars) (Constructors : Constructors) = struct

  module MyVars = MyVars
  module Constructors = Constructors

  open Constructors
  open MyVars

  type box_kind = ill_boxes
  type sort = ill_sorts

  type postype =
    | PVar of TyVar.t
    | Boxed of typ * box_kind
    | Data of typ pos_type_cons
  and negtype =
    | NVar of TyVar.t
    | CoData of typ neg_type_cons
  and typ =
    | TVar of TyVar.t
    | TPos of postype
    | TNeg of negtype

  let linear = Linear
  let affine = Affine
  let exp = Exponential

  let pos p = TPos p
  let neg n = TNeg n
  let tvar a = TVar a
  let posvar a = PVar a
  let negvar a = NVar a
  let boxed k t = Boxed (t, k)
  let data c = Data c
  let codata d = CoData d

  let string_of_sort = string_of_ill_sorts
  let string_of_box_kind = string_of_ill_boxes
  let rec string_of_type = function
    | TVar v -> TyVar.to_string v
    | TPos p -> string_of_postype p
    | TNeg n -> string_of_negtype n
  and string_of_postype = function
    | PVar v -> TyVar.to_string v
    | Boxed (t, box) -> pp_texp (string_of_box_kind box) [string_of_type t]
    | Data c -> string_of_pos_type_cons string_of_type c
  and string_of_negtype = function
    | NVar v -> TyVar.to_string v
    | CoData c -> string_of_neg_type_cons string_of_type c

  let string_of_binding (v,t) =
    (Var.to_string v) ^ " " ^ typ_annot (string_of_type t)
  let string_of_cobinding (a,t) =
    (CoVar.to_string a) ^ " " ^ typ_annot (string_of_type t)
end
