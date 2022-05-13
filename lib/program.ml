open Types

module Program = struct

  module Types = PreTypes
  module Calc = Ast.LCalc (Types)
  open Calc
  open Vars
  open Types

  type program_item =

    | Type_declaration of {
        name : TyVar.t;
        sort : sort
      }

    | Type_definition of {
        name : TyVar.t;
        sort : sort;
        args : (TyVar.t * sort) list;
        content : typ
      }

    | Data_definition of {
        name : TyVar.t;
        args : (TyVar.t * sort) list;
        content : (typ Constructors.constructor) list
      }

    | Codata_definition of {
        name : TyVar.t;
        args : (TyVar.t * sort) list;
        content : ((typ,typ) Constructors.destructor) list
      }

    | Term_definition of {
        name : Var.t;
        typ : typ;
        content : V.t
      }

    | Env_definition of {
        name : Var.t;
        typ : typ;
        content : S.t
      }

    | Cmd_definition of {
        name : Var.t;
        content : command
      }

  type program = program_item list

  let list_to_string ?interspace:inter k ls =
    let inter = Option.value ~default:"" inter in
    match ls with
    | [] -> ""
    | [x] -> k x
    | ls -> List.fold_left (fun acc arg -> if acc = "" then k arg else acc ^ inter ^ k arg) "" ls

  let cons_def_to_string ?cont:cont cons args =
    let cont = match cont with
      | None -> ""
      | Some cont -> " cont " ^ string_of_type cont in
    let args =
        list_to_string ~interspace:" * " string_of_type args in
    Printf.sprintf "| %s of %s%s" (ConsVar.to_string cons) args cont

  let tbind_to_string (t, so) =
    Printf.sprintf "{%s %s}" (TyVar.to_string t) (string_of_sort so)

  let lhs_to_string name args =
    if args = [] then
      TyVar.to_string name
    else
      let args = list_to_string ~interspace:" " tbind_to_string args  in
      Printf.sprintf "{%s %s}" (TyVar.to_string name) args


  let item_to_string = function

    | Type_declaration {name; sort} ->
      Printf.sprintf "decl type %s : %s"
        (TyVar.to_string name)
        (string_of_sort sort)

    | Type_definition {name; sort; args; content} ->
      Printf.sprintf "type %s : %s = %s"
        (lhs_to_string name args)
        (string_of_sort sort)
        (string_of_type content)

    | Data_definition {name; args; content} ->
      let aux def = "\n  | " ^ Constructors.definition_of_constructor string_of_type def in
      Printf.sprintf "data %s =%s"
        (lhs_to_string name args)
        (list_to_string aux content)

    | Codata_definition {name; args; content} ->
      let aux def = "\n  | " ^ Constructors.definition_of_destructor string_of_type def in
      Printf.sprintf "codata %s =%s"
        (lhs_to_string name args)
        (list_to_string aux content)

    | Term_definition {name; typ; content} ->
      Printf.sprintf "term %s : %s = %s"
        (Var.to_string name)
        (string_of_type typ)
        (string_of_value content)

    | Env_definition {name; typ; content} ->
      Printf.sprintf "env %s : %s = %s"
        (Var.to_string name)
        (string_of_type typ)
        (string_of_stack content)

    | Cmd_definition {name; content} ->
      Printf.sprintf "cmd %s = %s"
        (Var.to_string name)
        (string_of_command content)

  let program_to_string prog =
    list_to_string ~interspace:";\n\n" item_to_string prog

end
