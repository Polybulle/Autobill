open Vars
open Types
open Constructors
open Util

type pattern = (Var.t * typ option) constructor
type copattern = (Var.t * typ option, typ option) destructor

type value =
    Var of {
      node : Var.t;
      loc : position;
    }
  | Bindcc of {
      typ : typ option;
      po : extended_polarity;
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
      node : value constructor;
      loc : position;
    }
  | Destr of {
      node : (copattern * command) list;
      loc : position;
    }

and stack =
    Ret of {
      loc : position;
    }
  | CoBind of {
      name : Var.t;
      typ : typ option;
      po : extended_polarity;
      cmd : command;
      loc : position;
    }
  | CoBox of {
      kind : box_kind;
      stk : stack;
      loc : position;
    }
  | CoDestr of {
      node : (value, stack) destructor;
      loc : position;
    }
  | CoCons of { node : (pattern * command) list; loc : position; }

and command = Command of {
    po : extended_polarity;
    valu : value;
    stk : stack;
    typ : typ option;
    loc : position;
  }

type program_item =
    Type_declaration of {
      name : TyVar.t;
      sort : sort;
      loc : position;
    }
  | Type_definition of {
      name : TyVar.t;
      sort : sort option;
      args : (TyVar.t * sort option) list;
      content : typ; loc : position;
    }
  | Data_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : typ constructor list;
      loc : position;
    }
  | Codata_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : (typ, typ) destructor list;
      loc : position;
    }
  | Term_definition of {
      name : Var.t;
      typ : typ option;
      content : value;
      loc : position;
    }
  | Env_definition of {
      name : Var.t;
      typ : typ option;
      content : stack;
      loc : position;
    }
  | Cmd_definition of {
      name : Var.t;
      typ : typ option;
      content : command;
      loc : position;
    }

type program = program_item list

val loc_of_value : value -> position
val loc_of_stack : stack -> position
val loc_of_cmd : command -> position
val loc_of_item : program_item -> position

module V :
sig
  type t = value
  val var : ?loc:position -> Var.t -> value
  val bindcc : ?loc:position -> extended_polarity -> typ option -> command -> value
  val box : ?loc:position -> box_kind -> typ option -> command -> value
  val cons : ?loc:position -> value constructor -> value
  val case : ?loc:position -> (copattern * command) list -> value
end

module S :
sig
  type t = stack
  val ret : ?loc:position -> unit -> stack
  val bind :?loc:position -> extended_polarity -> typ option -> Var.t -> command -> stack
  val box : ?loc:position -> box_kind -> stack -> stack
  val destr : ?loc:position -> (value, stack) destructor -> stack
  val case : ?loc:position -> (pattern * command) list -> stack
end

type t = command
val cmd : ?loc:position -> extended_polarity -> typ option -> value -> stack -> command
val ( |+| ) : V.t -> S.t -> command
val ( |-| ) : V.t -> S.t -> command
val ( |~| ) : V.t -> S.t -> command
val ( |=> ) : 'a -> 'b -> 'a * 'b
