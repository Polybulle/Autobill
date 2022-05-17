open Types
open Vars
open Constructors


type pattern = (Var.t * typ option) constructor
type copattern = (Var.t * typ option, typ option) destructor


type value =

  | Var of Var.t

  | Bindcc of {
      typ : typ option;
      po : extended_polarity;
      cmd : command
    }

  | Box of {
      kind : box_kind;
      typ : typ option;
      cmd : command
    }

  | Cons of value constructor

  | Destr of (copattern * command) list

and stack =

  | Ret

  | CoBind of {
      name : Var.t;
      typ : typ option;
      po : extended_polarity;
      cmd : command
    }

  | CoBox of {
      kind : box_kind;
      stk : stack
    }

  | CoDestr of (value, stack) destructor

  | CoCons of (pattern * command) list

and command = Command of {
    po : extended_polarity;
    valu : value;
    stk : stack;
    typ : typ option
  }

type program_item =

  | Type_declaration of {
      name : TyVar.t;
      sort : sort
    }

  | Type_definition of {
      name : TyVar.t;
      sort : sort option;
      args : (TyVar.t * sort option) list;
      content : typ
    }

  | Data_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : (typ constructor) list
    }

  | Codata_definition of {
      name : TyVar.t;
      args : (TyVar.t * sort option) list;
      content : ((typ,typ) destructor) list
    }

  | Term_definition of {
      name : Var.t;
      typ : typ option;
      content : value
    }

  | Env_definition of {
      name : Var.t;
      typ : typ option;
      content : stack
    }

  | Cmd_definition of {
      name : Var.t;
      typ : typ option;
      content : command
    }

type program = program_item list


module V = struct
  type t = value
  let var x = Var(x)
  let bindcc po typ cmd = Bindcc {po; typ; cmd}
  let box kind typ cmd = Box {kind; typ; cmd}
  let cons c = Cons c
  let case l = Destr l
end

module S = struct
  type t = stack
  let ret = Ret
  let bind po typ name cmd = CoBind {po; typ; name; cmd}
  let box kind stk = CoBox {kind; stk}
  let destr c = CoDestr c
  let case l = CoCons l
end

type t = command
let cmd po typ valu stk = Command {po; typ; valu; stk}
let (|+|) (t : V.t) (s : S.t) = cmd `Positive None t s
let (|-|) (v : V.t) (e : S.t) = cmd `Negative None v e
let (|~|) (t : V.t) (e : S.t) = cmd `Ambiguous None t e
let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)
