open Types

module LCalc : functor (Types : ITypes) -> sig

  open Types
  open Types.MyVars
  open Types.Constructors

  type pattern = (Var.t * typ) constructor
  type copattern = (Var.t * typ, CoVar.t * typ) destructor
  type polarity = Pos | Neg | Unknown
  val string_of_pattern : (Var.t * typ) constructor -> string
  val string_of_copattern : (Var.t * typ, CoVar.t * typ) destructor -> string

  type value =
      Var of Var.t
    | Bind of CoVar.t * negtype * command
    | Force of CoVar.t * postype * command
    | Box of box_kind * CoVar.t * typ * command
    | Cons of value constructor
    | Destr of (copattern * command) list
  and stack =
      CoVar of CoVar.t
    | CoBind of Var.t * postype * command
    | CoForce of Var.t * negtype * command
    | CoBox of box_kind * stack
    | CoDestr of (value, stack) destructor
    | CoCons of (pattern * command) list
  and command = Command of polarity * value * stack

  val string_of_value : value -> string
  val string_of_stack : stack -> string
  val string_of_command : command -> string

  module V : sig
    type t = value
    val var : Var.t -> value
    val bind : CoVar.t -> negtype -> command -> value
    val str_bind : CoVar.t -> postype -> command -> value
    val box : box_kind -> CoVar.t -> typ -> command -> value
    val cons : value constructor -> value
    val cocase : (value, stack) destructor -> stack
  end

  module S : sig
    type t = stack
    val var : CoVar.t -> stack
    val bind : Var.t -> postype -> command -> stack
    val str_bind : Var.t -> negtype -> command -> stack
    val box : box_kind -> stack -> stack
    val destr : (copattern * command) list -> value
    val case : (pattern * command) list -> stack
  end

  type t = command
  val ( |+| ) : V.t -> S.t -> command
  val ( |~| ) : V.t -> S.t -> command
  val ( |?| ) : V.t -> S.t -> command
  val ( |=> ) : 'a -> 'b -> 'a * 'b

end
