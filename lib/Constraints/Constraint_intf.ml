open AaraCompress
open Format
open LP_of_FOL
open MZN_of_LP
open SMT_of_LP

let convert goal post_con =
  try
    let post_con = compress_unification post_con in
    let res = LP_of_FOL.convert post_con goal in
    res

  with
  | Invariant_break_not_convertible_to_optimization info ->
    Misc.fatal_error "Generating complexity model" info

let smt_of_fol goal f =
  SMT_of_FOL.pp_as_smt str_formatter (goal, f);
  flush_str_formatter ()

let mzn_of_lp lp =
  pp_mzn str_formatter lp;
  flush_str_formatter ()

let smt_of_lp lp =
  pp_smtlib str_formatter lp;
  flush_str_formatter ()
