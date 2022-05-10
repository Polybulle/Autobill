open Types
open Constructors
open Vars

module Program (Vars : AllVars) (Cons: Constructors) = struct

  module Types = PreTypes (Vars) (Cons)

  open Types

  open Ast.LCalc (Types)

  type program_item =

    | Type_declaration of {
        name : Vars.TyVar.t;
        sort : sort
      }

    | Type_definition of {
        name : Vars.TyVar.t;
        sort : sort;
        args : (Vars.TyVar.t * sort) list;
        content : typ
      }

    | Data_definition of {
        name : Vars.TyVar.t;
        args : (Vars.TyVar.t * sort) list;
        content : (Vars.ConsVar.t * typ list) list
      }

    | Codata_definition of {
        name : Vars.TyVar.t;
        args : (Vars.TyVar.t * sort) list;
        content : (Vars.ConsVar.t * typ list * typ) list
      }

    | Term_definition of {
        name : Vars.Var.t;
        typ : typ;
        content : V.t
      }

    | Env_definition of {
        name : Vars.Var.t;
        typ : typ;
        content : S.t
      }

    | Cmd_definition of {
        name : Vars.Var.t;
        content : command
      }

  type program = program_item list

  let list_to_string ?interspace:inter k ls =
    let inter = Option.value ~default:"" inter in
    match ls with
    | [] -> ""
    | [x] -> k x
    | ls -> List.fold_left (fun acc arg -> acc ^ inter ^ k arg) "" ls

  let cons_def_to_string ?cont:cont cons args =
    let cont = match cont with
      | None -> ""
      | Some cont -> " cont " ^ string_of_type cont in
    let args =
        list_to_string ~interspace:" * " string_of_type args in
    Printf.sprintf "| %s of %s%s" (Vars.ConsVar.to_string cons) args cont

  let tbind_to_string (t, so) =
    Printf.sprintf "{%s %s}" (Vars.TyVar.to_string t) (string_of_sort so)

  let lhs_to_string name args =
    let args = list_to_string ~interspace:" " tbind_to_string args  in
    Printf.sprintf "{%s %s}" (Vars.TyVar.to_string name) args


  let item_to_string = function

    | Type_declaration {name; sort} ->
      Printf.sprintf "type %s : %s"
        (Vars.TyVar.to_string name)
        (string_of_sort sort)

    | Type_definition {name; sort; args; content} ->
      Printf.sprintf "type %s : %s = %s"
        (lhs_to_string name args)
        (string_of_sort sort)
        (string_of_type content)

    | Data_definition {name; args; content} ->
      let aux (cons, args) = cons_def_to_string cons args in
      Printf.sprintf "data %s =\n%s\nend"
        (lhs_to_string name args)
        (list_to_string ~interspace:"\n  " aux content)

    | Codata_definition {name; args; content} ->
      let aux (cons, args, cont) = cons_def_to_string ~cont:cont cons args in
      Printf.sprintf "data %s =\n%s\nend"
        (lhs_to_string name args)
        (list_to_string ~interspace:"\n  " aux content)

    | Term_definition {name; typ; content} ->
      Printf.sprintf "term %s : %s = %s"
        (Vars.Var.to_string name)
        (Types.string_of_type typ)
        (string_of_value content)

    | Env_definition {name; typ; content} ->
      Printf.sprintf "env %s : %s = %s"
        (Vars.Var.to_string name)
        (Types.string_of_type typ)
        (string_of_stack content)

    | Cmd_definition {name; content} ->
      Printf.sprintf "cmd %s = %s"
        (Vars.Var.to_string name)
        (string_of_command content)

  let program_to_string prog =
    list_to_string ~interspace:";\n\n" item_to_string prog

end
