open Types
open Constructors
open Vars

module Program (Vars : AllVars) (Cons: Constructors) = struct

  module Types = PreTypes (Vars) (Cons)

  open Types

  open Ast.LCalc (Types)

  type type_declaration = {
    name : Vars.TyVar.t;
    sort : sort
  }

  type type_definition = {
    name : Vars.TyVar.t;
    sort : sort;
    args : (Vars.TyVar.t * sort);
    content : typ
  }

  type data_definition = {
    name : Vars.TyVar.t;
    args : (Vars.TyVar.t * sort);
    content : (Vars.ConsVar.t * typ list) list
  }

  type codata_definition = {
    name : Vars.TyVar.t;
    args : (Vars.TyVar.t * sort);
    content : (Vars.ConsVar.t * typ list * typ) list
  }

  type term_definition = {
    name : Vars.Var.t;
    typ : typ;
    content : V.t
  }

  type env_definition = {
    name : Vars.Var.t;
    typ : typ;
    content : S.t
  }

  type cmd_definition = {
    name : Vars.Var.t;
    typ : typ;
    content : command
  }


end
