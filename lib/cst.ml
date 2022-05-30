open Types
open Constructors
open Util

type zero = |

type typ = (string, string) pre_typ
type tyvar = string
type var = string
type consvar = string
type destrvar = string

type pattern = (consvar, var * typ option) constructor
type copattern = (destrvar, var * typ option, typ option) destructor

type value =

  | Var of {
        node : var;
        loc : position
    }

  | Bindcc of {
      typ : typ option;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | Box of {
      kind : box_kind;
      typ : typ option;
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

  | Ret of { loc : position }

  | CoBind of {
      name : var;
      typ : typ option;
      pol : polarity option;
      cmd : command;
      loc : position
    }

  | CoBox of {
      kind : box_kind;
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

and command =
  | Command of {
    pol : polarity option;
    valu : value;
    stk : stack;
    typ : typ option;
    loc : position
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
      cmd : command;
      loc : position
    }

type program_item =

  | Type_declaration of {
      name : tyvar;
      sort : sort;
      loc : position
    }

  | Type_definition of {
      name : tyvar;
      sort : sort;
      args : (tyvar * sort) list;
      content : typ;
      loc : position
    }

  | Data_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : ((consvar, typ) constructor) list;
      loc : position
    }

  | Codata_definition of {
      name : tyvar;
      args : (tyvar * sort) list;
      content : ((destrvar,typ,typ) destructor) list;
      loc : position
    }

  | Term_definition of {
      name : var;
      typ : typ option;
      content : value;
      loc : position
    }

  | Env_definition of {
      name : var;
      typ : typ option;
      content : stack;
      loc : position
    }

  | Cmd_definition of {
      name : var;
      typ : typ option;
      content : command;
      loc : position
    }

type program = program_item list

let loc_of_value = function
  | Var {loc;_} | Bindcc {loc;_} | Box {loc; _} | Cons {loc;_} | Destr {loc;_}
  | Macro_box {loc; _} | Macro_fun {loc; _} -> loc

let loc_of_stack = function
  | Ret {loc} | CoBind {loc;_} | CoBox {loc;_} | CoCons {loc;_} | CoDestr {loc;_} ->
    loc

let loc_of_cmd = function
  | Command {loc;_} | Macro_term {loc;_} | Macro_env {loc;_} | Macro_match_val {loc;_}
  | Macro_match_stk {loc;_} -> loc

let loc_of_item = function
  | Type_declaration {loc;_} | Type_definition {loc;_}
  | Data_definition {loc;_} | Codata_definition {loc;_}
  | Term_definition {loc;_} | Env_definition {loc;_} | Cmd_definition {loc;_} ->
    loc

module V = struct
  type t = value
  let var ?loc:(loc = dummy_pos) x = Var {node = x; loc}
  let bindcc ?loc:(loc = dummy_pos) ?pol:pol typ cmd = Bindcc {pol; typ; cmd; loc}
  let box ?loc:(loc = dummy_pos) kind typ cmd = Box {kind; typ; cmd; loc}
  let cons ?loc:(loc = dummy_pos) c = Cons {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = Destr {node = l; loc}
  let macro_fun ?loc:(loc = dummy_pos) arg typ valu = Macro_fun {loc; arg; typ; valu}
  let macro_box ?loc:(loc = dummy_pos) kind valu = Macro_box {loc; kind; valu}

end

module S = struct
  type t = stack
  let ret ?loc:(loc = dummy_pos) ()= Ret {loc}
  let bind ?loc:(loc = dummy_pos) ?pol:pol typ name cmd = CoBind {pol; typ; name; cmd; loc}
  let box ?loc:(loc = dummy_pos) kind stk = CoBox {kind; stk; loc}
  let destr ?loc:(loc = dummy_pos) c = CoDestr {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = CoCons {node = l; loc}
end

type t = command
let cmd ?loc:(loc = dummy_pos) ?pol:pol typ valu stk =
  Command {pol; typ; valu; stk; loc}
let (|+|) (t : V.t) (s : S.t) = cmd ~pol:Positive None t s
let (|-|) (v : V.t) (e : S.t) = cmd ~pol:Negative None v e
let (|~|) (t : V.t) (e : S.t) = cmd None t e
let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)

let cmd_let_val ?loc:(loc = dummy_pos) ?pol:pol name typ valu cmd =
  Macro_term {pol; loc; name; typ; valu; cmd}
let cmd_let_env ?loc:(loc = dummy_pos) ?pol:pol typ stk cmd =
  Macro_env {pol; loc; typ; stk; cmd}
let cmd_match_val ?loc:(loc = dummy_pos) ?pol:pol patt valu cmd =
  Macro_match_val {pol; loc; patt; valu; cmd}
let cmd_match_env ?loc:(loc = dummy_pos) ?pol:pol copatt cmd =
  Macro_match_stk {pol; loc; copatt; cmd}
