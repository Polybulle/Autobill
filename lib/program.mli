open Ast
open Types
open Vars

module Program :
    sig

      open PreTypes

      open LCalc (PreTypes)


      type program_item =
          Type_declaration of { name : TyVar.t; sort : sort; }
        | Type_definition of { name : TyVar.t; sort : sort;
            args : (TyVar.t * sort) list; content : typ;
          }
        | Data_definition of { name : TyVar.t;
            args : (TyVar.t * sort) list;
            content : (typ Constructors.constructor) list;
          }
        | Codata_definition of { name : TyVar.t;
            args : (TyVar.t * sort) list;
            content : ((typ,typ) Constructors.destructor) list;
          }
        | Term_definition of { name : Var.t; typ : typ; content : V.t; }
        | Env_definition of { name : Var.t; typ : typ; content : S.t; }
        | Cmd_definition of { name : Var.t; content : command; }
      type program = program_item list
      val list_to_string :
        ?interspace:string -> ('a -> string) -> 'a list -> string
      val cons_def_to_string :
        ?cont:typ -> ConsVar.t -> typ list -> string
      val tbind_to_string : TyVar.t * sort -> string
      val lhs_to_string :
        TyVar.t -> (TyVar.t * sort) list -> string
      val item_to_string : program_item -> string
      val program_to_string : program_item list -> string
    end
