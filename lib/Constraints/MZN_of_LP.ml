open Vars
open Format
open Polynomials
open LP_of_FOL

let pp_global fmt v =
  fprintf fmt "var int: %a;"
    (TyVar.pp ~debug:true) v

let pp_poly fmt p =
  let pp_scalar fmt (_,v) = fprintf fmt "constraint %a = 0;" Scalar.pp v in
  pp_print_list ~pp_sep:pp_print_newline pp_scalar fmt (Poly.P.bindings p)


let pp_goal fmt lp =

  let n_objs = Array.length lp.objectives in

  let objs = Array.init n_objs (fun _ -> TyVar.of_string "O") in

  let mk_obj i =
    let pp_sep fmt () = pp_print_string fmt " + " in
    fprintf fmt "var int: %a = %a;@."
      (TyVar.pp ~debug:true) objs.(i)
      (pp_print_list ~pp_sep (TyVar.pp ~debug:true)) lp.objectives.(i) in

  for i = 0 to n_objs-1 do mk_obj i done;

  let pp_obj fmt obj =
    fprintf fmt "int_min_goal(%a)" (TyVar.pp ~debug:true) obj in

  let pp_sep fmt () = pp_print_string fmt ", " in

  fprintf fmt "solve :: goal_hierarchy([%a]) satisfy;@.@."
    (pp_print_array ~pp_sep pp_obj) objs

let pp_doc fmt doc = ()

let pp_mzn fmt lp =
   fprintf fmt "@[<v 0>include \"experimental.mzn\";@.%a@.%a@.%a@.%a@.@]"
    (pp_print_list pp_global) lp.variables
    (pp_print_list ~pp_sep:pp_print_newline pp_poly) lp.null_constraints
    pp_goal lp
    pp_doc lp.objective_doc
