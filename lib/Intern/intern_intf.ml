open Util
open Intern_common
open Intern_prelude
open Intern_prog
open Intern_prettyPrinter

let internalize_prelude prog =
  try
    let tyconses, tycons_sorts = internalize_typcons prog in
    let env = {
      empty_sortcheck with
      tycons_vars = tyconses;
      tycons_sort = tycons_sorts;
    } in
    let (prelude, env) = List.fold_left
           sort_check_one_item
        (InternAst.empty_prelude, env)
        prog in
    let env = {env with prelude = prelude} in
    let is_not_prelude = function
      | Cst.Term_definition _
      | Cst.Cmd_execution _
      | Cst.Term_declaration _
        -> true
      | _ -> false in
    let prog = List.filter is_not_prelude prog in
    (prog, env)
  with
  | Bad_sort {loc; actual; expected} ->
    raise (Failure (
        Printf.sprintf "%s: FATAL sort error, wanted %s, got %s"
          (string_of_position loc)
          (Types.string_of_sort expected)
          (Types.string_of_sort actual)))
  | Undefined_type {name; loc} ->
    raise (Failure (
        Printf.sprintf "%s: FATAL undefined type %s"
          (string_of_position loc)
          name ))


let string_of_intern_ast prog =
  pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let pp_stringenv fmt env =
  let open Format in
  let pp_bind fmt (s,v) = fprintf fmt "%s %n\n" s v in
  let pp_sep fmt () = pp_print_space fmt () in
  pp_print_list ~pp_sep pp_bind fmt (StringEnv.bindings env)

let intern_prog env prog =
  let go (prog, env, decl) item =
    let decl, item, env = intern_definition env decl item in
    (item :: prog, env, decl) in
  let decl = StringEnv.empty in
  let prog, env,_ = List.fold_left go ([],env,decl) prog in
  List.rev prog, env

let internalize prog =
  let prog, env = internalize_prelude prog in
  let prog, env = intern_prog env prog in
  prog, env
