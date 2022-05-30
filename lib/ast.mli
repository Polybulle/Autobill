open Vars
open Constructors
open Types
open Util

module TyConsEnv : Map.S with type key = TyConsVar.t

module ConsEnv : Map.S with type key = ConsVar.t

module DestrEnv : Map.S with type key = DestrVar.t

type typ = (TyConsVar.t, TyVar.t) pre_typ
type val_bind = Var.t * typ
type pattern = (ConsVar.t, val_bind) constructor
type copattern = (DestrVar.t, val_bind, typ) destructor

type tycons_definition = {
  sort : sort;
  loc : position;
  args : (TyVar.t * sort) list;
  content : tycons_def_content
}
and tycons_def_content =
  | Declared
  | Defined of typ
  | Data of (ConsVar.t, typ) constructor list
  | Codata of (DestrVar.t, typ, typ) destructor list

type cons_definition = Consdef of {
  args : (TyVar.t * sort) list;
  content : (ConsVar.t, typ) constructor;
  resulting_type : typ
}

type destr_definition = Destrdef of {
  args : (TyVar.t * sort) list;
  content : (DestrVar.t, typ, typ) destructor;
  resulting_type : typ
}

type prelude = {
  tycons : tycons_definition TyConsEnv.t;
  cons : cons_definition ConsEnv.t;
  destr : destr_definition DestrEnv.t
}

type meta_value =
    MetaVal of {
      node : pre_value;
      val_typ : typ;
      loc : position;
    }
and pre_value =
  | Var of Var.t
  | Bindcc of {
      bind : typ;
      pol : polarity;
      cmd : command;
    }
  | Box of {
      kind : box_kind;
      bind : typ;
      cmd : command;
    }
  | Cons of (ConsVar.t, meta_value) constructor
  | Destr of (copattern * command) list

and meta_stack =
    MetaStack of {
      node : pre_stack;
      cont_typ : typ;
      final_typ : typ;
      loc : position;
    }
and pre_stack =
  | Ret
  | CoBind of {
      bind : val_bind;
      pol : polarity;
      cmd : command;
    }
  | CoBox of {
      kind : box_kind;
      stk : meta_stack;
    }
  | CoDestr of (DestrVar.t, meta_value, meta_stack) destructor
  | CoCons of (pattern * command) list
and command =
    Command of {
      pol : polarity;
      valu : meta_value;
      stk : meta_stack;
      mid_typ : typ;
      loc : position;
    }

type definition = Definition of {
      name : DefVar.t;
      typ : typ;
      sort : sort;
      cont : typ;
      content : content;
      loc : position
    }
and content =
  | Value_definition of meta_value
  | Stack_definition of meta_stack
  | Command_definition of command

type program = prelude * definition list

val empty_prelude : prelude

module V : sig
  type t = meta_value
  val var : Var.t -> t
  val bindcc : polarity -> typ -> command -> t
  val box : box_kind -> typ -> command -> t
  val cons : (ConsVar.t, t) constructor -> t
  val case : (copattern * command) list -> t
end

module S : sig
  type t = meta_stack
  val ret : t
  val bind : polarity -> val_bind -> command -> t
  val box : box_kind -> t -> t
  val destr : (DestrVar.t, V.t, t) destructor -> t
  val case : (pattern *  command) list -> t
end
