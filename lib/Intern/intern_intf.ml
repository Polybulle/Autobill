open Intern_common
open Intern_prelude
open Intern_prog

let internalize_prelude prog =
  let env = empty_sortcheck in
  let env = internalize_all_sortvar env prog in
  let env = internalize_all_typcons env prog in
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

let intern_prog env prog =
  let go (prog, env, decl) item =
    let decl, item, env = intern_definition env decl item in
    (item :: prog, env, decl) in
  let decl = StringEnv.empty in
  let prog, env,_ = List.fold_left go ([],env,decl) prog in
  List.rev prog, env

let string_of_intern_ast = Intern_prettyPrinter.string_of_intern_ast

let internalize prog =
  let prog, env = internalize_prelude prog in
  let prog, env = intern_prog env prog in
  prog, env
