
module EmptyCalc :
  sig
    module Var :
      sig
        type t = Vars.StringVar.Var.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module CoVar :
      sig
        type t = Vars.StringVar.CoVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module TyVar :
      sig
        type t = Vars.StringVar.TyVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module ConsVar :
      sig
        type t = Vars.StringVar.ConsVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    type 'x constructor = 'x Constructors.Empty.constructor
    type 't pos_type_cons = 't Constructors.Empty.pos_type_cons
    type 't neg_type_cons = 't Constructors.Empty.neg_type_cons
    type ('x, 'a) destructor =
        ('x, 'a) Constructors.Empty.destructor
    val string_of_constructor : ('x -> string) -> 'x constructor -> string
    val string_of_pos_type_cons :
      ('t -> string) -> 't pos_type_cons -> string
    val string_of_neg_type_cons :
      ('t -> string) -> 't neg_type_cons -> string
    val string_of_destructor :
      ('x -> string) -> ('a -> string) -> ('x, 'a) destructor -> string
    type sort =
      Types.Types(Vars.StringVar)(Constructors.Empty).sort =
        SoType
      | SoPosType
      | SoNegType
    type box_kind =
      Types.Types(Vars.StringVar)(Constructors.Empty).box_kind =
        Linear
      | Affine
      | Exponential
    type postype =
      Types.Types(Vars.StringVar)(Constructors.Empty).postype =
        PVar of Vars.StringVar.TyVar.t
      | Boxed of typ * box_kind
      | Data of typ Constructors.Empty.pos_type_cons
    and negtype =
      Types.Types(Vars.StringVar)(Constructors.Empty).negtype =
        NVar of Vars.StringVar.TyVar.t
      | CoData of typ Constructors.Empty.neg_type_cons
    and typ =
      Types.Types(Vars.StringVar)(Constructors.Empty).typ =
        TVar of Vars.StringVar.TyVar.t
      | TPos of postype
      | TNeg of negtype
    val pos : postype -> typ
    val neg : negtype -> typ
    val tvar : Vars.StringVar.TyVar.t -> typ
    val posvar : Vars.StringVar.TyVar.t -> postype
    val negvar : Vars.StringVar.TyVar.t -> negtype
    val boxed : typ -> postype
    val aff : typ -> postype
    val exp : typ -> postype
    val data : typ Constructors.Empty.pos_type_cons -> postype
    val codata : typ Constructors.Empty.neg_type_cons -> negtype
    val sort_to_string : sort -> string
    val box_kind_to_string : box_kind -> string
    val string_of_type : typ -> string
    val string_of_postype : postype -> string
    val string_of_negtype : negtype -> string
    val string_of_binding : Vars.StringVar.Var.t * typ -> string
    val string_of_cobinding : Vars.StringVar.CoVar.t * typ -> string
    type pattern = (Var.t * typ) constructor
    type copattern = (Var.t * typ, CoVar.t * typ) destructor
    val string_of_pattern :
      (Vars.StringVar.Var.t * typ) constructor -> string
    val string_of_copattern :
      (Vars.StringVar.Var.t * typ,
       Vars.StringVar.CoVar.t * typ)
      destructor -> string
    type value =
      Ast.LCalc(Vars.StringVar)(Constructors.Empty).value =
        Var of Var.t
      | Bind of CoVar.t * negtype * command
      | Box of box_kind * CoVar.t * typ * command
      | Cons of value constructor
      | CoDestr of (copattern * command) list
    and stack =
      Ast.LCalc(Vars.StringVar)(Constructors.Empty).stack =
        CoVar of CoVar.t
      | CoBind of Var.t * postype * command
      | CoBox of box_kind * stack
      | Destr of (value, stack) destructor
      | CoCons of (pattern * command) list
    and command =
      Ast.LCalc(Vars.StringVar)(Constructors.Empty).command =
        CPos of term * stack
      | CNeg of value * environment
    and term =
      Ast.LCalc(Vars.StringVar)(Constructors.Empty).term =
        Force of CoVar.t * postype * command
      | Val of value
    and environment =
      Ast.LCalc(Vars.StringVar)(Constructors.Empty).environment =
        CoForce of Var.t * negtype * command
      | Stack of stack
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
module ILLCalc :
  sig
    module Var :
      sig
        type t = Vars.StringVar.Var.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module CoVar :
      sig
        type t = Vars.StringVar.CoVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module TyVar :
      sig
        type t = Vars.StringVar.TyVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module ConsVar :
      sig
        type t = Vars.StringVar.ConsVar.t
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    type sort =
      Types.Types(Vars.StringVar)(Constructors.ILL).sort =
        SoType
      | SoPosType
      | SoNegType
    type box_kind =
      Types.Types(Vars.StringVar)(Constructors.ILL).box_kind =
        Linear
      | Affine
      | Exponential
    type postype =
      Types.Types(Vars.StringVar)(Constructors.ILL).postype =
        PVar of Vars.StringVar.TyVar.t
      | Boxed of typ * box_kind
      | Data of typ Constructors.ILL.pos_type_cons
    and negtype =
      Types.Types(Vars.StringVar)(Constructors.ILL).negtype =
        NVar of Vars.StringVar.TyVar.t
      | CoData of typ Constructors.ILL.neg_type_cons
    and typ =
      Types.Types(Vars.StringVar)(Constructors.ILL).typ =
        TVar of Vars.StringVar.TyVar.t
      | TPos of postype
      | TNeg of negtype
    val pos : postype -> typ
    val neg : negtype -> typ
    val tvar : Vars.StringVar.TyVar.t -> typ
    val posvar : Vars.StringVar.TyVar.t -> postype
    val negvar : Vars.StringVar.TyVar.t -> negtype
    val boxed : typ -> postype
    val aff : typ -> postype
    val exp : typ -> postype
    val data : typ Constructors.ILL.pos_type_cons -> postype
    val codata : typ Constructors.ILL.neg_type_cons -> negtype
    val sort_to_string : sort -> string
    val box_kind_to_string : box_kind -> string
    val string_of_type : typ -> string
    val string_of_postype : postype -> string
    val string_of_negtype : negtype -> string
    val string_of_binding : Vars.StringVar.Var.t * typ -> string
    val string_of_cobinding : Vars.StringVar.CoVar.t * typ -> string
    type pattern = (Var.t * typ) Constructors.ILL.constructor
    type copattern =
        (Var.t * typ, CoVar.t * typ) Constructors.ILL.destructor
    val string_of_pattern :
      (Vars.StringVar.Var.t * typ)
      Constructors.ILL.constructor -> string
    val string_of_copattern :
      (Vars.StringVar.Var.t * typ,
       Vars.StringVar.CoVar.t * typ)
      Constructors.ILL.destructor -> string
    type value =
      Ast.LCalc(Vars.StringVar)(Constructors.ILL).value =
        Var of Var.t
      | Bind of CoVar.t * negtype * command
      | Box of box_kind * CoVar.t * typ * command
      | Cons of value Constructors.ILL.constructor
      | CoDestr of (copattern * command) list
    and stack =
      Ast.LCalc(Vars.StringVar)(Constructors.ILL).stack =
        CoVar of CoVar.t
      | CoBind of Var.t * postype * command
      | CoBox of box_kind * stack
      | Destr of (value, stack) Constructors.ILL.destructor
      | CoCons of (pattern * command) list
    and command =
      Ast.LCalc(Vars.StringVar)(Constructors.ILL).command =
        CPos of term * stack
      | CNeg of value * environment
    and term =
      Ast.LCalc(Vars.StringVar)(Constructors.ILL).term =
        Force of CoVar.t * postype * command
      | Val of value
    and environment =
      Ast.LCalc(Vars.StringVar)(Constructors.ILL).environment =
        CoForce of Var.t * negtype * command
      | Stack of stack
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
        val cons : value Constructors.ILL.constructor -> value
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
        val destr :
          (value, stack) Constructors.ILL.destructor -> stack
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
    type 't pos_type_cons =
      't Constructors.ILL.pos_type_cons =
        Unit
      | Zero
      | Prod of 't * 't
      | Sum of 't * 't
    val zero : 'a pos_type_cons
    val prod : 'a -> 'a -> 'a pos_type_cons
    val sum : 'a -> 'a -> 'a pos_type_cons
    val string_of_pos_type_cons :
      ('a -> string) -> 'a pos_type_cons -> string
    type 't neg_type_cons =
      't Constructors.ILL.neg_type_cons =
        Top
      | Bottom
      | Fun of 't * 't
      | Choice of 't * 't
    val top : 'a neg_type_cons
    val bottom : 'a neg_type_cons
    val func : 'a -> 'a -> 'a neg_type_cons
    val choice : 'a -> 'a -> 'a neg_type_cons
    val string_of_neg_type_cons :
      ('a -> string) -> 'a neg_type_cons -> string
    type 'x constructor =
      'x Constructors.ILL.constructor =
        Unit
      | Pair of 'x * 'x
      | Fst of 'x
      | Snd of 'x
    val unit : 'a constructor
    val pair : 'a -> 'a -> 'a constructor
    val fst : 'a -> 'a constructor
    val snd : 'a -> 'a constructor
    val string_of_constructor : ('a -> string) -> 'a constructor -> string
    type ('x, 'a) destructor =
      ('x, 'a) Constructors.ILL.destructor =
        Call of 'x * 'a
      | Yes of 'a
      | No of 'a
    val call : 'a -> 'b -> ('a, 'b) destructor
    val yes : 'a -> ('b, 'a) destructor
    val no : 'a -> ('b, 'a) destructor
    val string_of_destructor :
      ('a -> string) -> ('b -> string) -> ('a, 'b) destructor -> string
  end
val test1 : EmptyCalc.command
val test2 : EmptyCalc.command
val test3 : ILLCalc.term
val test4 : ILLCalc.value
