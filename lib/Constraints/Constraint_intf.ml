open AaraCompress
open Format
open LP_of_FOL
open MZN_of_LP
open SMT_of_LP

include Constraint_common

let fail_non_poly () =
  Misc.fail_invariant_break
    "Sorting error: Non polynomial parameter constructor in constraint "

let optim_program_of_prog = optim_program_of_prog

let post_constraint_as_string opt =
  let post = FirstOrder.FullFOL.compress_logic ~remove_loc:true opt in
  let post = AaraCompress.compress_unification post in
  pp_set_geometry str_formatter ~margin:80 ~max_indent:60;
  SMT_of_FOL.pp_as_smt str_formatter post;
  pp_print_newline str_formatter ();
  flush_str_formatter ()

(* let aara_constraint_as_string opt = *)
(*   let post = FirstOrder.FullFOL.compress_logic opt.formula in *)
(*   let post = compress_unification post in *)
(*   let post = FirstOrder.FullFOL.compress_logic post in *)
(*   (\* let post = compress_unification post in *\) *)
(*   (\* let post = FirstOrder.FullFOL.compress_logic post in *\) *)
(*   (\* let post = compress_unification post in *\) *)
(*   pp_set_geometry str_formatter ~margin:180 ~max_indent:170; *)
(*   FirstOrder.FullFOL.pp_formula str_formatter post.formula; *)
(*   pp_print_newline str_formatter (); *)
(*   flush_str_formatter () *)

let skolem_constraint_as_string opt =
  let module P = struct
      let opt = opt
    end in
  let module S = Skolemize.Converter (P) in
  let post = FirstOrder.FullFOL.compress_logic ~remove_loc:true opt in
  let post = AaraCompress.compress_unification post in
  let skol = S.convert_fol post in
  let skol = Skolem.compress skol in
  Skolem.pp_smt str_formatter skol;
  flush_str_formatter ()

let lp_of_fol post_con =
  try
    let post_con = FirstOrder.FullFOL.compress_logic post_con in
    let post_con = compress_unification post_con in
    let post_con = FirstOrder.FullFOL.compress_logic post_con in
    (* let post_con = compress_unification post_con in *)
    (* let post_con = FirstOrder.FullFOL.compress_logic post_con in *)
    (* let post_con = compress_unification post_con in *)
    let res = LP_of_FOL.convert post_con in
    res

  with
  | Invariant_break_not_convertible_to_optimization info ->
    Misc.fatal_error "Generating complexity model" info

let smt_of_fol f =
  let f = FirstOrder.FullFOL.compress_logic f in
  let f = compress_unification f in
  let f = FirstOrder.FullFOL.compress_logic f in
  SMT_of_FOL.pp_as_smt str_formatter f;
  flush_str_formatter ()

let mzn_of_lp lp =
  pp_mzn str_formatter lp;
  flush_str_formatter ()

let smt_of_lp lp =
  pp_smtlib str_formatter lp;
  flush_str_formatter ()
