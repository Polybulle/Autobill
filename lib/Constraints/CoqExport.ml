open Vars
open Types
open FirstOrder
open FullFOL
open Format

let export_tyvar fmt v = pp_print_string fmt (String.lowercase_ascii (TyVar.to_string v))

let export_tyconsvar fmt v = pp_print_string fmt (String.lowercase_ascii (TyConsVar.to_string v))

let export_relvar fmt v = pp_print_string fmt (String.lowercase_ascii (RelVar.to_string v))

let export_tycons fmt c = match c with
  | Cons c -> export_tyconsvar fmt c
  | _ -> assert false

let pp_list pp fmt l = pp_print_list ~pp_sep:pp_print_space pp fmt l

let pp_and pp fmt l = pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,/\\ ") pp fmt l

let rec export_typ fmt (t:typ) = match t with
| TCons c -> export_tycons fmt c.node
| TApp {tfun; args = []; _} -> export_typ fmt tfun
| TApp {tfun; args; _} ->
  fprintf fmt "(%a %a)" export_typ tfun (pp_list export_typ) args
| TVar {node;_} | TInternal node -> export_tyvar fmt node

let export_eqn fmt = function
  | Eq (a,b,_) -> fprintf fmt "@[(%a = %a)@]" export_typ a export_typ b
  | Rel (r, args) -> fprintf fmt "@[(%a %a)@]" export_relvar r (pp_list export_typ) args

let export_eqns fmt = function
  | [] -> fprintf fmt "True"
  | eqns ->  fprintf fmt "@[<v 2>(%a)@]" (pp_and export_eqn) eqns

let export_binder str fmt vs =
  let pp_bind fmt v = fprintf fmt "(%a:nat)" export_tyvar v in
  if vs = [] then ()
  else fprintf fmt "@[%s %a,@]@," str (pp_list pp_bind) vs


let rec export fmt (c : formula) = match c with
| PTrue -> fprintf fmt "True"
| PFalse -> fprintf fmt "False"
| PLoc (loc, c) -> fprintf fmt "@[<v 0>(* at location %s *)@,%a@]"
                     (Misc.string_of_position loc)
                     export c
| PEqn eqns -> export_eqns fmt eqns
| PAnd [] | PCases [] -> fprintf fmt "True"
| PAnd [c] | PCases [c] -> export fmt c
| PAnd cs | PCases cs -> fprintf fmt "@[<v 1>(%a)@]" (pp_and export) cs
| PExists ([], [], eqns, c) -> export fmt (PAnd [PEqn eqns; c])
| PExists (xs, ys, eqns, c) ->
  fprintf fmt "@[<v 2>(%a %a@,/\\ %a)@]"
    (export_binder "exists") (xs@ys)
    export_eqns eqns
    export c
| PForall (xs, ys, eqns, c) ->
  fprintf fmt "@[<v 2>(%a %a %a@ -> %a)@]"
    (export_binder "forall") (ys)
    (export_binder "exists") (xs)
    (pp_and export_eqn) eqns
    export c
