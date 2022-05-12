open Vars
open Constructors

 type ill_sorts = PosType | NegType | NeuType
type ill_boxes = Linear | Affine | Exponential

let string_of_ill_sorts = function
  | PosType -> "typ+"
  | NegType -> "typ-"
  | NeuType -> "typ~"
let string_of_ill_boxes = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"

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


module type IPreTypes = sig
  type pretyp
  include ITypes
     with type box_kind = ill_boxes
     and type sort = ill_sorts
     and type negtype = pretyp
     and type postype = pretyp
     and type typ = pretyp
  val omitted : pretyp
end

let typ_annot s = if String.get s 0 = '{' then s else "{" ^ s ^ "}"

module PreTypes  = struct

  type sort = ill_sorts
  type box_kind = ill_boxes
  type pretyp =
    | TPosCons of pretyp Constructors.pos_type_cons
    | TNegCons of pretyp Constructors.neg_type_cons
    | TBox of box_kind * pretyp
    | TVar of TyVar.t
    | TPos of pretyp
    | TNeg of pretyp
    | TOmitted
  type typ = pretyp
  type postype = typ
  type negtype = typ

  let linear = Linear
  let affine = Affine
  let exp = Exponential
  let sort_postype = PosType
  let sort_negtype = NegType
  let sort_neuttype = NeuType

  let pos t = TPos t
  let neg t = TNeg t
  let tvar v = TVar v
  let posvar v = pos (tvar v)
  let negvar v = neg (tvar v)
  let boxed k t = TBox (k,t)
  let data dat = TPosCons dat
  let codata dat = TNegCons dat
  let omitted = TOmitted

  let string_of_sort = string_of_ill_sorts
  let string_of_box_kind = string_of_ill_boxes
  let string_of_tvar v = TyVar.to_string v
  let rec string_of_type = function
    | TVar v -> "?" ^ string_of_tvar v
    | TPos (TVar v) -> "+" ^ string_of_tvar v
    | TNeg (TVar v) -> "~" ^ string_of_tvar v
    | TPos t | TNeg t -> string_of_type t
    | TPosCons dat -> Constructors.string_of_pos_type_cons string_of_type dat
    | TNegCons dat -> Constructors.string_of_neg_type_cons string_of_type dat
    | TBox (k,t) -> pp_texp "box" [string_of_box_kind k; string_of_type t]
    | TOmitted -> "_omitted"
  let string_of_postype = string_of_type
  let string_of_negtype = string_of_type
  let string_of_binding (v,t) =
    Var.to_string v ^ " " ^ typ_annot (string_of_type t)
  let string_of_cobinding (a,t) =
    CoVar.to_string a ^ " " ^ typ_annot (string_of_type t)
end

module FullTypes  = struct

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
  let sort_postype = PosType
  let sort_negtype = NegType
  let sort_neuttype = NeuType

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
