open Vars
open Types
open Constructors

type pattern = (Var.t * typ option) constructor
type copattern = (Var.t * typ option, CoVar.t * typ option) destructor

type value =
  | Var of Var.t
  | Bind of {
      name : CoVar.t;
      typ : typ option;
      po : extended_polarity;
      cmd : command;
    }
  | Box of {
      kind : box_kind;
      name : CoVar.t;
      typ : typ option;
      cmd : command;
    }
  | Cons of value constructor
  | Destr of (copattern * command) list

and stack =
  | CoVar of CoVar.t
  | CoBind of {
      name : Var.t;
      typ : typ option;
      po : extended_polarity;
      cmd : command;
    }
  | CoBox of {
      kind : box_kind;
      stk : stack;
    }
  | CoDestr of (value, stack) destructor
  | CoCons of (pattern * command) list

and command = Command of {
      po : extended_polarity;
      valu : value;
      stk : stack;
      typ : typ option;
    }

type program_item =
  | Type_declaration of {
      name : TyVar.t;
      sort : sort;
    }
  | Type_definition of {
      name : TyVar.t;
      sort : sort;
      args : (TyVar.t * sort) list;
      content : typ;
    }
  | Data_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort) list;
      content : typ constructor list;
    }
  | Codata_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort) list;
      content : (typ, typ) destructor list;
    }
  | Term_definition of {
      name : Var.t;
      typ : typ;
      content : value;
    }
  | Env_definition of {
      name : Var.t;
      typ : typ;
      content : stack;
    }
  | Cmd_definition of {
      name : Var.t;
      content : command;
    }

type program = program_item list
module V :
  sig
    type t = value
    val var : Var.t -> value
    val bind : extended_polarity -> typ option -> CoVar.t -> command -> value
    val box : box_kind -> CoVar.t -> typ option -> command -> value
    val cons : value constructor -> value
    val case : (copattern * command) list -> value
  end
module S :
  sig
    type t = stack
    val var : CoVar.t -> stack
    val bind : extended_polarity -> typ option -> Var.t -> command -> stack
    val box : box_kind -> stack -> stack
    val destr : (value, stack) destructor -> stack
    val case : (pattern * command) list -> stack
  end
type t = command
val cmd :
  extended_polarity ->
  typ option -> value -> stack -> command
val ( |+| ) : V.t -> S.t -> command
val ( |-| ) : V.t -> S.t -> command
val ( |~| ) : V.t -> S.t -> command
val ( |=> ) : 'a -> 'b -> 'a * 'b
