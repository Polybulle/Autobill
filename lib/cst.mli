open Types
open Constructors
open Util

type zero = |

type pol = unit pre_polarity
type sort = (unit, zero) pre_sort
type typ = string pre_typ
type tyvar = string
type var = string
type consvar = string
type destrvar = string

type pattern = (consvar, var * typ option) constructor
type copattern =(destrvar, var * typ option, typ option) destructor

type value =
  | Var of {
      node : var;
      loc : position;
    }
  | Bindcc of {
      typ : typ option;
      po : pol;
      cmd : command;
      loc : position;
    }
  | Box of {
      kind : box_kind;
      typ : typ option;
      cmd : command;
      loc : position;
    }
  | Cons of {
      node : (consvar, value) constructor;
      loc : position;
    }
  | Destr of {
      node : (copattern * command) list;
      loc : position;
    }

and stack =
  | Ret of {
      loc : position;
    }
  | CoBind of {
      name : var;
      typ : typ option;
      po : pol;
      cmd : command;
      loc : position;
    }
  | CoBox of {
      kind : box_kind;
      stk : stack;
      loc : position;
    }
  | CoDestr of {
      node : (destrvar, value, stack) destructor;
      loc : position;
    }
  | CoCons of { node : (pattern * command) list;
      loc : position;
    }

and command =
  | Command of {
      po : pol;
      valu : value;
      stk : stack;
      typ : typ option;
      loc : position;
    }

type program_item =
  | Type_declaration of {
      name : tyvar;
      sort : sort;
      loc : position;
    }
  | Type_definition of {
      name : tyvar;
      sort : sort option;
      args : (tyvar * sort option) list;
      content : typ;
      loc : position;
    }
  | Data_definition of {
      name : tyvar;
      args : (tyvar * sort option) list;
      content : (consvar, typ) constructor list;
      loc : position;
    }
  | Codata_definition of { name : tyvar; args : (tyvar * sort option) list;
      content : (destrvar, typ, typ) destructor list;
      loc : position;
    }
  | Term_definition of { name : var; typ : typ option; content : value;
      loc : position;
    }
  | Env_definition of { name : var; typ : typ option; content : stack;
      loc : position;
    }
  | Cmd_definition of { name : var; typ : typ option; content : command;
      loc : position;
    }

type program = program_item list

val loc_of_value : value -> position
val loc_of_stack : stack -> position
val loc_of_cmd : command -> position
val loc_of_item : program_item -> position

module V : sig
  type t = value
  val var : ?loc:position -> var -> value
  val bindcc : ?loc:position -> pol -> typ option -> command -> value
  val box : ?loc:position -> box_kind -> typ option -> command -> value
  val cons : ?loc:position -> (consvar, value) constructor -> value
  val case : ?loc:position -> (copattern * command) list -> value
end

module S : sig
  type t = stack
  val ret : ?loc:position -> unit -> stack
  val bind : ?loc:position -> pol -> typ option -> var -> command -> stack
  val box : ?loc:position -> box_kind -> stack -> stack
  val destr : ?loc:position -> (destrvar, value, stack) destructor -> stack
  val case : ?loc:position -> (pattern * command) list -> stack
end

type t = command
val cmd : ?loc:position ->  pol -> typ option -> value -> stack -> command
val ( |+| ) : V.t -> S.t -> command
val ( |-| ) : V.t -> S.t -> command
val ( |~| ) : V.t -> S.t -> command
val ( |=> ) : 'a -> 'b -> 'a * 'b
