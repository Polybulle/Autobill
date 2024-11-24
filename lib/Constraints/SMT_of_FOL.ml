open Format
open Vars
open FirstOrder
open FullFOL
open Ast
open FullAst


let rec pp_term fmt (t:Types.typ) = match t with
  | Types.TVar {node=v;_}
  | Types.TInternal v ->  Vars.TyVar.pp ~debug:true fmt v
  | Types.TCons {node=Cons c;_} ->
    let name = if c = Primitives.nat_zero then "0"
               else if c = Primitives.nat_one then "1"
               else if c = Primitives.nat_add then "+"
               else if c = Primitives.nat_mult then "*"
               else  TyConsVar.to_string ~debug:true c in
     pp_print_string fmt name
  | Types.TCons _ ->
     Misc.fail_invariant_break "During SMT-LIB export, found a base-sort type constructor"
  | Types.TApp { tfun; args; _ } ->
     let pp_one fmt arg = fprintf fmt " %a" pp_term arg in
     fprintf fmt "(%a%a)"
       pp_term tfun
       (pp_print_list pp_one) args

let pp_var_decl fmt x = fprintf fmt "(%a Int)" FullFOL.pp_var x

let pp_eqn fmt = function
  | Eq (x,y,_) -> fprintf fmt "(= %a %a)" pp_term x pp_term y
  | Rel (rel, args) ->
     if rel = Primitives.nat_leq then
       fprintf fmt "(<= %a)" (pp_print_list ~pp_sep:pp_print_space pp_term) args
     else
       fprintf fmt "(%a %a)" pp_rel rel (pp_print_list ~pp_sep:pp_print_space pp_term) args

let pp_var_decls = pp_print_list ~pp_sep:pp_print_space pp_var_decl
let pp_eqns = pp_print_list ~pp_sep:pp_print_space pp_eqn

let rec pp fmt (f:formula) = match f with
  | PTrue -> fprintf fmt "true"
  | PFalse -> fprintf fmt "false"
  | PLoc (_, f) -> pp fmt f
  | PEqn eqns ->
     fprintf fmt "(and %a)" (pp_print_list ~pp_sep:pp_print_space pp_eqn) eqns
  | PAnd fs
    | PCases fs -> fprintf fmt "(and %a)" (pp_print_list ~pp_sep:pp_print_space pp) fs
  | PExists (xs, ys, eqns, f) ->
     fprintf fmt "(exists (%a) %a)" pp_var_decls (xs@ys) pp (PAnd [PEqn eqns; f])
  | PForall ([], [], assume, witness, f) ->
     fprintf fmt "(=> %a %a)"
       pp_eqns assume
       pp (PAnd [PEqn witness; f])
  | PForall ([], exists, assume, witness, f) ->
     fprintf fmt "(exists (%a) (=> (and %a) %a))"
       pp_var_decls exists
       pp_eqns assume
       pp (PAnd [PEqn witness; f])
  | PForall (univs, [], assume, witness, f) ->
     fprintf fmt "(forall (%a) (=> (and %a) %a))"
       pp_var_decls univs
       pp_eqns assume
       pp (PAnd [PEqn witness; f])
  | PForall (univs, exists, assume, witness, f) ->
     fprintf fmt "(forall (%a) (exists (%a) (=> (and %a) %a)))"
       pp_var_decls univs
       pp_var_decls exists
       pp_eqns assume
       pp (PAnd [PEqn witness; f])


let mk_args (Goal goal) : TyVar.t list =

  let names = ["X"; "Y"; "Z"; "T"; "S"; "R"; "N"; "M"; "P"; "Q"] in
  List.init goal.args_number (fun i ->
      if i < List.length names then
        TyVar.of_string (List.nth names i)
      else
        TyVar.fresh ())

let mk_poly (Goal goal) =
  let params = ref [] in
  let add_param _ =
    let v = TyVar.fresh () in
    params := v :: !params;
    v in
  let goal_args = mk_args (Goal goal) in
  let args = List.map (fun v -> Polynomials.V v) goal_args in
  let poly = Polynomials.free_poly ~callback:add_param ~base:args ~degree:goal.degree in
  (goal.polynomial, poly, args, !params)

let pp_poly_body fmt p =
  let open Polynomials in
  let pp_mono fmt m =
    match m with
    | Mono.Unit -> pp_print_string fmt "1"
    | Mono {powers;_} ->
       pp_print_string fmt "(*";
       Mono.M.iter (fun v n ->
           for _ = 1 to n do
             fprintf fmt " %a" (Polynomials.pp_var ~debug:true) v
           done) powers;
       pp_print_string fmt ")" in
  pp_print_string fmt "(+";
  Poly.P.iter (fun m s ->
      fprintf fmt " (* %a %a)" Scalar.pp s pp_mono m) p;
  pp_print_string fmt ")"

let pp_poly_call fmt (name, args) =
  fprintf fmt "(%a %a)"
    (TyConsVar.pp ~debug:true) name
    (pp_print_list ~pp_sep:pp_print_space Polynomials.pp_var) args

let pp_param_decl fmt param =
  fprintf fmt "(declare-const %a Int) (assert (<= 0 %a))"
    (TyVar.pp ~debug:true) param
    (TyVar.pp ~debug:true) param

let pp_poly_assert fmt (name, formula, args, _) =
  let pp_one fmt v = fprintf fmt "(%a Int)" (Polynomials.pp_var ~debug:true) v in
  let pp_bind fmt vs = pp_print_list ~pp_sep:pp_print_space pp_one fmt vs in
  fprintf fmt "(assert (forall (%a) (= %a %a)))"
    pp_bind args
    pp_poly_call (name, args)
    pp_poly_body formula


let pp_poly_decl fmt (poly, n) =
  let aux fmt n =
    for _ = 1 to n do pp_print_string fmt "Int "done in
  fprintf fmt "(declare-fun %a (%a) Int)" (TyConsVar.pp ~debug:true) poly aux n


let pp_poly fmt goal =
  let (name, formula, args, params) = mk_poly goal in
  pp_print_list ~pp_sep:pp_print_newline pp_param_decl fmt params;
  pp_print_newline fmt ();
  pp_poly_decl fmt (name, List.length args);
  pp_print_newline fmt ();
  pp_poly_assert fmt (name,formula,args,params)

let pp_as_smt fmt (goal, formula) =
  fprintf fmt "%a\n(assert %a)\n\n(check-sat)\n(get-model)\n"
    (pp_print_option pp_poly) goal
    pp formula

let output (goal, formula) =
  pp_as_smt str_formatter (goal, formula);
  flush_str_formatter ()
