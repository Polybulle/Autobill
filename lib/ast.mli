module LCalc :
  functor (Vars : Vars.Var)
    (Constructors : Constructors.Constructors) ->
    sig
      module Var :
        sig
          type t = Vars.Var.t
          val of_string : string -> t
          val to_string : t -> string
          val fresh : unit -> t
        end
      module CoVar :
        sig
          type t = Vars.CoVar.t
          val of_string : string -> t
          val to_string : t -> string
          val fresh : unit -> t
        end
      module TyVar :
        sig
          type t = Vars.TyVar.t
          val of_string : string -> t
          val to_string : t -> string
          val fresh : unit -> t
        end
      module ConsVar :
        sig
          type t = Vars.ConsVar.t
          val of_string : string -> t
          val to_string : t -> string
          val fresh : unit -> t
        end
      type 'x constructor = 'x Constructors.constructor
      type 't pos_type_cons = 't Constructors.pos_type_cons
      type 't neg_type_cons = 't Constructors.neg_type_cons
      type ('x, 'a) destructor = ('x, 'a) Constructors.destructor
      val string_of_constructor : ('x -> string) -> 'x constructor -> string
      val string_of_pos_type_cons :
        ('t -> string) -> 't pos_type_cons -> string
      val string_of_neg_type_cons :
        ('t -> string) -> 't neg_type_cons -> string
      val string_of_destructor :
        ('x -> string) -> ('a -> string) -> ('x, 'a) destructor -> string
      type sort =
        Types.Types(Vars)(Constructors).sort =
          SoType
        | SoPosType
        | SoNegType
      type box_kind =
        Types.Types(Vars)(Constructors).box_kind =
          Linear
        | Affine
        | Exponential
      type postype =
        Types.Types(Vars)(Constructors).postype =
          PVar of Vars.TyVar.t
        | Boxed of typ * box_kind
        | Data of typ Constructors.pos_type_cons
      and negtype =
        Types.Types(Vars)(Constructors).negtype =
          NVar of Vars.TyVar.t
        | CoData of typ Constructors.neg_type_cons
      and typ =
        Types.Types(Vars)(Constructors).typ =
          TVar of Vars.TyVar.t
        | TPos of postype
        | TNeg of negtype
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
      type pattern = (Var.t * typ) constructor
      type copattern = (Var.t * typ, CoVar.t * typ) destructor
      val string_of_pattern : (Vars.Var.t * typ) constructor -> string
      val string_of_copattern :
        (Vars.Var.t * typ, Vars.CoVar.t * typ) destructor -> string
      type value =
          Var of Var.t
        | Bind of CoVar.t * negtype * command
        | Box of box_kind * CoVar.t * typ * command
        | Cons of value constructor
        | CoDestr of (copattern * command) list
      and stack =
          CoVar of CoVar.t
        | CoBind of Var.t * postype * command
        | CoBox of box_kind * stack
        | Destr of (value, stack) destructor
        | CoCons of (pattern * command) list
      and command = CPos of term * stack | CNeg of value * environment
      and term = Force of CoVar.t * postype * command | Val of value
      and environment = CoForce of Var.t * negtype * command | Stack of stack
      val string_of_value : value -> string
      val string_of_stack : stack -> string
      val string_of_term : term -> string
      val string_of_env : environment -> string
      val string_of_command : command -> string
      module V :
        sig
          type t = value
          val var : Var.t -> value
          val bind : CoVar.t -> negtype -> command -> value
          val box : CoVar.t -> typ -> command -> value
          val aff : CoVar.t -> typ -> command -> value
          val exp : CoVar.t -> typ -> command -> value
          val cons : value constructor -> value
          val cocase : (copattern * command) list -> value
        end
      module S :
        sig
          type t = stack
          val var : CoVar.t -> stack
          val bind : Var.t -> postype -> command -> stack
          val box : stack -> stack
          val aff : stack -> stack
          val exp : stack -> stack
          val destr : (value, stack) destructor -> stack
          val case : (pattern * command) list -> stack
        end
      module T :
        sig
          type t = term
          val str_bind : CoVar.t -> postype -> command -> term
          val vall : value -> term
        end
      module E :
        sig
          type t = environment
          val str_bind : Var.t -> negtype -> command -> environment
          val stack : stack -> environment
        end
      module C :
        sig
          type t = command
          val ( |+| ) : T.t -> S.t -> command
          val ( |-| ) : V.t -> E.t -> command
          val ( |>| ) : V.t -> S.t -> command
          val ( |<| ) : V.t -> S.t -> command
        end
      val ( |=> ) : 'a -> 'b -> 'a * 'b
      type t = command
      val ( |+| ) : T.t -> S.t -> command
      val ( |-| ) : V.t -> E.t -> command
      val ( |>| ) : V.t -> S.t -> command
      val ( |<| ) : V.t -> S.t -> command
    end
