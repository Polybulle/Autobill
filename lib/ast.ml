open Types
open Vars
open Constructors
open Util

module TyConsEnv = Map.Make (struct
    type t = TyConsVar.t
    let compare = compare
  end)

module ConsEnv = Map.Make (struct
    type t = ConsVar.t
    let compare = compare
  end)

module DestrEnv = Map.Make (struct
    type t = DestrVar.t
    let compare = compare
  end)

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

let dummy_val_meta v = MetaVal {
    node = v;
    loc = Util.dummy_pos;
    val_typ = tvar (TyVar.fresh ())
  }

let dummy_stack_meta s = MetaStack {
    node = s;
    loc = Util.dummy_pos;
    cont_typ = tvar (TyVar.fresh ());
    final_typ = tvar (TyVar.fresh ());
  }

module V = struct
  type t = meta_value
  let var x = dummy_val_meta (Var x)
  let bindcc pol bind cmd = dummy_val_meta (Bindcc {pol;cmd;bind})
  let box kind bind cmd = dummy_val_meta (Box {kind; bind; cmd})
  let cons c = dummy_val_meta (Cons c)
  let case l = dummy_val_meta (Destr l)
end

module S = struct
  type t = meta_stack
  let ret = dummy_stack_meta (Ret)
  let bind pol bind cmd = dummy_stack_meta (CoBind {pol; bind; cmd})
  let box kind stk = dummy_stack_meta (CoBox {kind; stk})
  let destr c = dummy_stack_meta (CoDestr c)
  let case l = dummy_stack_meta (CoCons l)
end

let empty_prelude = {
  tycons = TyConsEnv.empty;
  cons = ConsEnv.empty;
  destr = DestrEnv.empty
}
