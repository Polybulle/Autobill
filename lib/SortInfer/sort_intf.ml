open Preprocess_ast
open SortInfer
open Sort_export

let polarity_inference (prog:  Preprocess_ast.program) =
  let env = initial_sort_check_env prog.prelude in
  let env = unify_prog env prog in
  export_ast env prog
