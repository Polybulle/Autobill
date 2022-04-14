module Types :
  functor (Vars : Vars.Var)
    (Constructors : Constructors.Constructors) ->
  sig
    type sort = SoType | SoPosType | SoNegType
    type box_kind = Linear | Affine | Exponential
    type postype =
        PVar of Vars.TyVar.t
      | Boxed of typ * box_kind
      | Data of typ Constructors.pos_type_cons
    and negtype =
        NVar of Vars.TyVar.t
      | CoData of typ Constructors.neg_type_cons
    and typ = TVar of Vars.TyVar.t | TPos of postype | TNeg of negtype
    val pos : postype -> typ
    val neg : negtype -> typ
    val tvar : Vars.TyVar.t -> typ
    val posvar : Vars.TyVar.t -> postype
    val negvar : Vars.TyVar.t -> negtype
    val boxed : typ -> postype
    val aff : typ -> postype
    val exp : typ -> postype
    val data : typ Constructors.pos_type_cons -> postype
    val codata : typ Constructors.neg_type_cons -> negtype
    val sort_to_string : sort -> string
    val box_kind_to_string : box_kind -> string
    val string_of_type : typ -> string
    val string_of_postype : postype -> string
    val string_of_negtype : negtype -> string
    val string_of_binding : Vars.Var.t * typ -> string
    val string_of_cobinding : Vars.CoVar.t * typ -> string
  end
