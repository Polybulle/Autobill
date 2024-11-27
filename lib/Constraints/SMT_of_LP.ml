open Vars
open Format
open Polynomials
open LP_of_FOL

let rec pp_scalar fmt (s:Scalar.t) = match Scalar.simplify s with
| Scalar.Param v -> TyVar.pp ~debug:true fmt v
| Scalar.Cons c -> TyConsVar.pp ~debug:true fmt c
| Scalar.Cst n -> pp_print_int fmt n
| Scalar.Mult (a, b) -> fprintf fmt "(* %a %a)" pp_scalar a pp_scalar b
| Scalar.Add (a, b) -> fprintf fmt "(+ %a %a)" pp_scalar a pp_scalar b
| Scalar.Pow (_, _) -> assert false


let pp_global fmt v =
  fprintf fmt "(declare-const %a Int) (assert (<= 0 %a))"
    (TyVar.pp ~debug:true) v
    (TyVar.pp ~debug:true) v

let pp_null fmt (_,v) = fprintf fmt "(assert (= 0 %a))" pp_scalar v

let pp_poly fmt p =
  pp_print_list ~pp_sep:pp_print_newline pp_null fmt (Poly.P.bindings p)

let pp_goal fmt xs =
  let pp_scals = pp_print_list ~pp_sep:pp_print_space (TyVar.pp ~debug:true) in
  let pp_one fmt x = fprintf fmt "(assert (<= 0 %a)) (minimize %a)" pp_scals x pp_scals x in
  pp_print_array ~pp_sep:pp_print_newline pp_one fmt xs

let pp_output fmt doc =
  fprintf fmt "(echo \"Goal is %s\")" doc

let pp_smtlib fmt lp =
  fprintf fmt "@[<v 0>%a@.@.%a@.@.%a@.@.(check-sat)@.%a@.(get-objectives)@]"
    (pp_print_list pp_global) lp.variables
    (pp_print_list ~pp_sep:pp_print_newline pp_poly) lp.null_constraints
    pp_goal lp.objectives
    pp_output lp.objective_doc
