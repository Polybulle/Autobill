open Types
open Constructors
open Util

type zero = |

type typ = (string, string) pre_typ
type tyvar = string
type var = string
type consvar = string
type destrvar = string

type bind = var * typ option
type cont_bind = var * typ option

type pattern = (consvar, bind) constructor
type copattern =(destrvar, bind, cont_bind) destructor


type value =

  | Var of {
        node : var;
        loc : position
    }

  | CoTop of {loc : position}

  | Bindcc of {
      bind : cont_bind;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | Box of {
      kind : box_kind;
      bind : cont_bind;
      cmd : command;
      loc : position
    }

  | Fix of {
      self : bind;
      cont : cont_bind;
      cmd : command;
      loc : position
    }

  | Cons of {
      node : (consvar, value) constructor;
      loc : position
    }

  | Destr of {
      node : (copattern * command) list;
      loc : position;
    }

  | Pack of {
      cons : consvar;
      typs : typ list;
      content : value;
      loc : position
    }

  | Spec of {
      destr : destrvar;
      spec_vars : (tyvar * sort) list;
      bind : cont_bind;
      cmd : command;
      loc : position
    }

  | Macro_box of {
      kind : box_kind;
      valu : value;
      loc : position
    }

  | Macro_fun of {
      arg : var;
      typ : typ option;
      valu : value;
      loc : position;
    }


and stack =

  | Ret of { var : var; loc : position }

  | CoZero of {loc : position}

  | CoBind of {
      bind : bind;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | CoBox of {
      kind : box_kind;
      stk : stack;
      loc : position
    }

  | CoFix of {
      stk : stack;
      loc : position
    }

  | CoDestr of {
      node : (destrvar, value, stack) destructor;
      loc : position
    }

  | CoCons of {
      node : (pattern * command) list;
      loc : position
    }

  | CoPack of {
      cons : consvar;
      pack_vars : (tyvar * sort) list;
      bind : bind;
      cmd : command;
      loc : position
    }

  | CoSpec of {
      destr : destrvar;
      typs : typ list;
      content : stack;
      loc : position
    }

and command =
  | Command of {
      pol : polarity option;
      valu : value;
      stk : stack;
      typ : typ option;
      loc : position;
    }
  | Macro_term of {
      name : string;
      pol : polarity option;
      typ : typ option;
      valu : value;
      cmd : command;
      loc : position
    }
  | Macro_env of {
      typ : typ option;
      pol : polarity option;
      name : string;
      stk : stack;
      cmd : command;
      loc : position
    }
  | Macro_match_val of {
      patt : pattern;
      pol : polarity option;
      valu : value;
      cmd : command;
      loc : position
    }
  | Macro_match_stk of {
      copatt : copattern;
      pol : polarity option;
      stk : stack;
      cmd : command;
      loc : position
    }

type program_item =
  | Type_declaration of {
      name : tyvar;
      sort : sort;
      loc : position;
    }
  | Type_definition of {
      name : tyvar;
      sort : sort;
      args : (tyvar * sort) list;
      content : typ;
      loc : position;
    }
  | Data_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : (consvar, typ) constructor list;
      loc : position;
    }
  | Codata_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : (destrvar, typ, typ) destructor list;
      loc : position;
    }
  | Pack_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      cons : consvar;
      private_typs : (tyvar * sort) list;
      arg_typs : typ list;
      loc : position;
    }
  | Spec_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      destr : destrvar;
      private_typs : (tyvar * sort) list;
      arg_typs : typ list;
      ret_typ : typ;
      loc : position;
    }
  | Term_definition of {
      name : var;
      typ : typ option;
      content : value;
      loc : position;
    }
  | Term_declaration of {
      name : var;
      typ : typ;
      loc : position
    }
  | Cmd_execution of {
      name : var option;
      typ : typ option;
      cont : var;
      content : command;
      loc : position;
    }

type program = program_item list

val loc_of_value : value -> position
val loc_of_stack : stack -> position
val loc_of_cmd : command -> position
val loc_of_item : program_item -> position

module V : sig
  type t = value
  val cotop : ?loc:position -> unit -> value
  val var : ?loc:position -> var -> value
  val bindcc : ?loc:position -> ?pol:polarity -> var -> typ option -> command -> value
  val box : ?loc:position -> box_kind -> var -> typ option -> command -> value
  val cons : ?loc:position -> (consvar, value) constructor -> value
  val case : ?loc:position -> (copattern * command) list -> value
  val macro_fun : ?loc:position -> var -> typ option -> value -> value
  val macro_box : ?loc:position -> box_kind -> value -> value
end

module S : sig
  type t = stack
  val cozero : ?loc:position -> unit -> stack
  val ret : ?loc:position -> var -> stack
  val bind : ?loc:position -> ?pol:polarity -> var -> typ option -> command -> stack
  val box : ?loc:position -> box_kind -> stack -> stack
  val destr : ?loc:position -> (destrvar, value, stack) destructor -> stack
  val case : ?loc:position -> (pattern * command) list -> stack
end

type t = command
val cmd : ?loc:position -> ?pol:polarity -> typ option -> value -> stack -> command
val ( |+| ) : V.t -> S.t -> command
val ( |-| ) : V.t -> S.t -> command
val ( |~| ) : V.t -> S.t -> command
val ( |=> ) : 'a -> 'b -> 'a * 'b
val cmd_let_val : ?loc:position -> ?pol:polarity -> string -> typ option -> value -> command -> command
val cmd_let_env : ?loc:position -> ?pol:polarity -> string -> typ option -> stack -> command -> command
val cmd_match_val : ?loc:position -> ?pol:polarity -> value -> pattern -> command -> command
val cmd_match_env : ?loc:position -> ?pol:polarity -> stack -> copattern -> command -> command
