open Vars
open Constructors

module Types (Vars : Var) (Constructors : Constructors) = struct
  open Constructors
  open Vars

  type sort = SoType | SoPosType | SoNegType

  type box_kind = Linear | Affine | Exponential

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

  let pos p = TPos p
  let neg n = TNeg n
  let tvar a = TVar a
  let posvar a = PVar a
  let negvar a = NVar a
  let boxed t = Boxed (t, Linear)
  let aff t = Boxed (t, Affine)
  let exp t = Boxed (t, Exponential)
  let data c = Data c
  let codata d = CoData d

  let sort_to_string = function
    | SoType -> "type"
    | SoPosType -> "positive"
    | SoNegType -> "negative"

  let box_kind_to_string = function
    | Linear -> "boxed"
    | Affine -> "affine"
    | Exponential -> "exp"

  let rec string_of_type = function
    | TVar v -> TyVar.to_string v
    | TPos p -> string_of_postype p
    | TNeg n -> string_of_negtype n
  and string_of_postype = function
    | PVar v -> TyVar.to_string v
    | Boxed (t, box) -> pp_texp (box_kind_to_string box) [string_of_type t]
    | Data c -> string_of_pos_type_cons string_of_type c
  and string_of_negtype = function
    | NVar v -> TyVar.to_string v
    | CoData c -> string_of_neg_type_cons string_of_type c

  let string_of_binding (v,t) =
    (Var.to_string v) ^ " " ^ (string_of_type t)
  let string_of_cobinding (a,t) =
    (CoVar.to_string a) ^ " " ^ (string_of_type t)
end
