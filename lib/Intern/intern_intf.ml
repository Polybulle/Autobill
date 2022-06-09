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
    let is_not_prelude = function
      | Cst.Term_definition _ | Cst.Env_definition _ | Cst.Cmd_definition _
        -> true
      | _ -> false in
    let prog = List.filter is_not_prelude prog in
    (prog, prelude, env)
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

let intern_prog env prog =
  let go (prog, env) item =
    let item, env = intern_definition env item in
    (item :: prog, env) in
  let prog, env = List.fold_left go ([],env) prog in
  List.rev prog, env

let internalize prog =
  let prog, prelude, env = internalize_prelude prog in
  let prog, env = intern_prog env prog in
  prelude, prog, env

let polarity_inference ?debug prelude env prog =

  let env =
    List.fold_left (Intern_pol_inference.unify_def ?debug prelude) env prog in
  let prog = List.map (Intern_export.export_ast env) prog in
  (prelude, prog)


let intern_error_wrapper f =
  let wrap ?loc str = begin
    let loc = match loc with
      | None -> ""
      | Some loc ->"(" ^ Util.string_of_position loc ^ ")" in
    print_endline ("FATAL" ^ loc ^ ": " ^ str);
    exit 1 end in
  try f () with

  | Intern_common.Ambiguous_polarity loc ->
    wrap ("ambiguous polarity at " ^ (Util.string_of_position loc))

  | Intern_common.Double_definition name ->
    wrap ("the name " ^ name ^ " is defined twice")

  | Intern_common.Bad_sort {loc; actual; expected} ->
    wrap ~loc ("conflicting sorts, expected "
               ^ Types.string_of_sort expected
               ^ ", got "
               ^ Types.string_of_sort actual)

  | Intern_common.Undefined_type {name; loc} ->
    wrap ~loc ("The type " ^ name ^ "is undefined")

  | Intern_common.Bad_type_cons_arity {cons; loc} ->
    wrap ~loc ("This type application has the wrong arity for" ^ cons)

  | Intern_common.Bad_constructor_name {loc} ->
    wrap ~loc ("This constructor/destructor name is reserved")

  | Intern_common.Higher_order_type_argument {loc; name} ->
    wrap ~loc ("Unsupported: the type argument "
               ^ name
               ^ " has a higher-order sort")

  | Intern_common.Undefined_var (name, loc) ->
    wrap ~loc ("Undefined variable " ^ name)

  | Intern_common.Undefined_constructor (name, loc) ->
    wrap ~loc ("Undefined constructor " ^ name)

  | Intern_common.Undefined_destructor (name, loc) ->
    wrap ~loc ("Undefined destructor " ^ name)

  | Intern_common.Polarity_mismatch (loc1, loc2) ->
    wrap ("The polarities of expressions at "
         ^ string_of_position loc1
         ^ " and "
         ^ string_of_position loc2
         ^ "disagree")
