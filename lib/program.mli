open Types
open Constructors
open Vars

module Program :
  functor (Vars : AllVars)
    (Cons : Constructors) ->
    sig
      module Types : ITypes
      open Vars
      type program_item =
          Type_declaration of { name : TyVar.t; sort : Types.sort; }
        | Type_definition of { name : TyVar.t; sort : Types.sort;
            args : (TyVar.t * Types.sort) list; content : Types.typ;
          }
        | Data_definition of { name : TyVar.t;
            args : (TyVar.t * Types.sort) list;
            content : (ConsVar.t * Types.typ list) list;
          }
        | Codata_definition of { name : TyVar.t;
            args : (TyVar.t * Types.sort) list;
            content : (ConsVar.t * Types.typ list * Types.typ) list;
          }
        | Term_definition of { name : Var.t; typ : Types.typ;
            content : Ast.LCalc(Types).value;
          }
        | Env_definition of { name : Var.t; typ : Types.typ;
            content : Ast.LCalc(Types).stack;
          }
        | Cmd_definition of { name : Var.t;
            content : Ast.LCalc(Types).command;
          }
      type program = program_item list
      val list_to_string :
        ?interspace:string -> ('a -> string) -> 'a list -> string
      val cons_def_to_string :
        ?cont:Types.typ -> ConsVar.t -> Types.typ list -> string
      val tbind_to_string : TyVar.t * Types.sort -> string
      val lhs_to_string :
        TyVar.t -> (TyVar.t * Types.sort) list -> string
      val item_to_string : program_item -> string
      val program_to_string : program_item list -> string
    end
