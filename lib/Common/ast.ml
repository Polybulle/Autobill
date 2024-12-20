open Types
open Vars
open Misc
open Prelude

module type AstParams = sig
  type val_bind
  type cont_bind
  type type_bind
  type cmd_annot
  type toplevel_bind_annot
  type polarity
  type sort
end

module FullAstParams = struct
  type sort = SortVar.t Types.sort
  type val_bind = Var.t * typ
  type cmd_annot = typ
  type toplevel_bind_annot = typ
  type cont_bind = CoVar.t * typ
  type type_bind = TyVar.t * sort
  type polarity = Types.polarity
end

module Ast (Params : AstParams) = struct

  include Params

  type typ = (TyConsVar.t, TyVar.t) pre_typ
  and constructor = (ConsVar.t, typ, meta_value) Constructors.constructor
  and destructor = (ConsVar.t, typ, meta_value, meta_stack) Constructors.destructor
  and pattern = (ConsVar.t, type_bind, val_bind) Constructors.constructor
  and copattern = (DestrVar.t, type_bind, val_bind, cont_bind) Constructors.destructor

  and meta_value =
      MetaVal of {
        node : pre_value;
        val_typ : typ;
        loc : position;
      }

  and pre_value =
    | Var of Var.t
    | CoTop
    | Bindcc of {
        bind : cont_bind;
        pol : polarity;
        cmd : command;
      }
    | Box of {
        kind : box_kind;
        bind : cont_bind;
        cmd : command;
      }
    | Fix of {
        bind : cont_bind;
        stk : meta_stack;
      }
    | Cons of constructor
    | Destr of {
        for_type : TyConsVar.t type_cons;
        cases : (copattern * command) list;
        default : (cont_bind * command) option;
      }

  and meta_stack =
      MetaStack of {
        node : pre_stack;
        cont_typ : typ;
        loc : position;
      }

  and pre_stack =
    | Ret of CoVar.t
    | CoZero
    | CoBind of {
        bind : val_bind;
        pol : polarity;
        cmd : command;
      }
    | CoBox of {
        kind : box_kind;
        stk : meta_stack;
      }
    | CoFix of meta_stack
    | CoDestr of destructor
    | CoCons of {
        for_type : TyConsVar.t type_cons;
        cases : (pattern * command) list;
        default : (val_bind * command) option;
      }


  and command = Command of {
      pol : polarity;
      loc : position;
      node : pre_command
    }

  and pre_command =
    | Interact of {
        valu : meta_value;
        mid_typ : cmd_annot;
        stk : meta_stack
      }
    | Trace of {
        comment : string option;
        dump : meta_value option;
        cmd : command
      }
    | Struct of {
        valu : meta_value;
        binds : val_bind list;
        cmd : command
      }
    | Pack of {
        stk : meta_stack;
        name : CoVar.t;
        cmd : command;
      }
    | Spec of {
        valu : meta_value;
        name : Var.t;
        cmd : command
      }

  type prog_item =
    | Value_declaration of {
        bind : Var.t * toplevel_bind_annot;
        pol : polarity;
        loc : position
      }
    | Value_definition  of {
        bind : val_bind;
        pol : polarity;
        loc : position;
        content : meta_value
      }

  type command_execution = Command_execution of {
      name : Var.t;
      pol : polarity;
      conttyp : toplevel_bind_annot;
      cont : CoVar.t;
      loc : position;
      content : command;
    }

  type goal = Goal of {
    polynomial : TyConsVar.t;
    args_number : int;
    degree : int
  }

  type program = {
    prelude : prelude;
    declarations : prog_item list;
    command : command_execution option;
    goal : goal option
  }

  let val_meta ?loc ?typ node =
    let loc = match loc with Some loc -> loc | None -> dummy_pos in
    let val_typ = match typ with Some typ -> typ | None -> tvar (TyVar.fresh ()) in
    MetaVal {node;loc;val_typ;}

  let stack_meta ?loc ?typ node =
    let loc = match loc with Some loc -> loc | None -> dummy_pos in
    let cont_typ = match typ with Some typ -> typ | None -> tvar (TyVar.fresh ()) in
    MetaStack {node;loc;cont_typ}

  module V = struct
    type t = meta_value
    let cotop ?loc ?typ () = val_meta ?loc ?typ CoTop
    let var ?loc ?typ x = val_meta ?loc ?typ (Var x)
    let bindcc ?loc ?typ pol bind cmd = val_meta ?loc ?typ (Bindcc {pol;cmd;bind})
    let box ?loc ?typ kind bind cmd = val_meta ?loc ?typ (Box {kind; bind; cmd})
    let cons ?loc ?typ c = val_meta ?loc ?typ (Cons c)
    let case ?loc ?typ ?default for_type cases = val_meta ?loc ?typ (Destr {for_type; cases; default})
  end

  module S = struct
    type t = meta_stack
    let cozero ?loc ?typ () = stack_meta ?loc ?typ CoZero
    let ret ?loc ?typ a = stack_meta ?loc ?typ (Ret a)
    let bind ?loc ?typ pol bind cmd = stack_meta ?loc ?typ (CoBind {pol; bind; cmd})
    let box ?loc ?typ kind stk = stack_meta ?loc ?typ (CoBox {kind; stk})
    let destr ?loc ?typ c = stack_meta ?loc ?typ (CoDestr c)
    let case ?loc ?typ ?default for_type cases  =
      stack_meta ?loc ?typ (CoCons {for_type; default; cases})
  end

end

module FullAst = Ast (FullAstParams)
