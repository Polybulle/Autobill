open Types
open Vars
open Constructors
open Util

type pattern = (Var.t * typ option) constructor
type copattern = (Var.t * typ option, typ option) destructor

type value =

  | Var of {
        node : Var.t;
        loc : position
    }

  | Bindcc of {
      typ : typ option;
      po : extended_polarity;
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
      node : value constructor;
      loc : position
    }

  | Destr of {
      node : (copattern * command) list;
      loc : position;
    }

and stack =

  | Ret of { loc : position }

  | CoBind of {
      name : Var.t;
      typ : typ option;
      po : extended_polarity;
      cmd : command;
      loc : position
    }

  | CoBox of {
      kind : box_kind;
      stk : stack;
      loc : position
    }

  | CoDestr of {
      node : (value, stack) destructor;
      loc : position
    }

  | CoCons of {
      node : (pattern * command) list;
      loc : position
    }

and command = Command of {
    po : extended_polarity;
    valu : value;
    stk : stack;
    typ : typ option;
    loc : position
  }

type program_item =

  | Type_declaration of {
      name : TyVar.t;
      sort : sort;
      loc : position
    }

  | Type_definition of {
      name : TyVar.t;
      sort : sort option;
      args : (TyVar.t * sort option) list;
      content : typ;
      loc : position
    }

  | Data_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : (typ constructor) list;
      loc : position
    }

  | Codata_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : ((typ,typ) destructor) list;
      loc : position
    }

  | Term_definition of {
      name : Var.t;
      typ : typ option;
      content : value;
      loc : position
    }

  | Env_definition of {
      name : Var.t;
      typ : typ option;
      content : stack;
      loc : position
    }

  | Cmd_definition of {
      name : Var.t;
      typ : typ option;
      content : command;
      loc : position
    }

type program = program_item list

let loc_of_value = function
  | Var {loc;_} | Bindcc {loc;_} | Box {loc; _} | Cons {loc;_} | Destr {loc;_} ->
    loc

let loc_of_stack = function
  | Ret {loc} | CoBind {loc;_} | CoBox {loc;_} | CoCons {loc;_} | CoDestr {loc;_} ->
    loc

let loc_of_cmd (Command c) = c.loc

let loc_of_item = function
  | Type_declaration {loc;_} | Type_definition {loc;_}
  | Data_definition {loc;_} | Codata_definition {loc;_}
  | Term_definition {loc;_} | Env_definition {loc;_} | Cmd_definition {loc;_} ->
    loc

module V = struct
  type t = value
  let var ?loc:(loc = dummy_pos) x = Var {node = x; loc}
  let bindcc ?loc:(loc = dummy_pos) po typ cmd = Bindcc {po; typ; cmd; loc}
  let box ?loc:(loc = dummy_pos) kind typ cmd = Box {kind; typ; cmd; loc}
  let cons ?loc:(loc = dummy_pos) c = Cons {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = Destr {node = l; loc}
end

module S = struct
  type t = stack
  let ret ?loc:(loc = dummy_pos) ()= Ret {loc}
  let bind ?loc:(loc = dummy_pos) po typ name cmd = CoBind {po; typ; name; cmd; loc}
  let box ?loc:(loc = dummy_pos) kind stk = CoBox {kind; stk; loc}
  let destr ?loc:(loc = dummy_pos) c = CoDestr {node = c; loc}
  let case ?loc:(loc = dummy_pos) l = CoCons {node = l; loc}
end

type t = command
let cmd ?loc:(loc = dummy_pos) po typ valu stk = Command {po; typ; valu; stk; loc}
let (|+|) (t : V.t) (s : S.t) = cmd `Positive None t s
let (|-|) (v : V.t) (e : S.t) = cmd `Negative None v e
let (|~|) (t : V.t) (e : S.t) = cmd `Ambiguous None t e
let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)
