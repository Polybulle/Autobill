open Vars
open FirstOrder
open FullFOL
open Primitives
open Format
open Ast
open FullAst

open Polynomials

exception Invariant_break_not_convertible_to_optimization of string

let fail_untranslatable mess = raise (Invariant_break_not_convertible_to_optimization mess)


type lp = {
    variables : TyVar.t list;
    null_constraints : Poly.t list;
    objectives : TyVar.t list array;
    objective_doc : string
  }

let globals = ref []
let mk_param () =
  let p = TyVar.fresh () in
  globals := p :: !globals;
  p

let goal_args (Goal goal) : TyVar.t list =
  let names = ["X"; "Y"; "Z"; "T"; "S"; "R"; "N"; "M"; "P"; "Q"] in
  List.init goal.args_number (fun i ->
      if i < List.length names then
        TyVar.of_string (List.nth names i)
      else
        TyVar.fresh ())


let output name args p =
  fprintf str_formatter "%a(%a) = %a"
    (TyConsVar.pp ~debug:false) name
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ", ") (Polynomials.pp_var ~debug:false)) args
    Poly.pp p;
  flush_str_formatter ()



let poly_of_goal (Goal goal)  =
  let dmax = goal.degree in
  let arr = Array.make (dmax+1) [] in
  let deg = function Mono.Unit -> 0 | Mono {degree;_} -> degree in
  let callback m =
    let x = mk_param () in
    let d = deg m in
    arr.(dmax-d) <- x :: arr.(dmax-d);
    x in
  let args = List.map (fun v -> Polynomials.V v) (goal_args (Goal goal)) in
  let poly = free_poly ~callback ~base:args ~degree:goal.degree in
  (args, arr, poly)



let convert f (Goal goal) =

  let env = ref TyVar.Env.empty in
  let env_add x t = (env := TyVar.Env.add x t !env) in
  let env_get x = TyVar.Env.find x !env in
  let add_var x = env_add x (Poly.of_mono (Mono.of_var (V x))) in
  let add_param vars x =
    let p = free_poly ~callback:(fun _ -> mk_param ()) ~base:vars ~degree:goal.degree in
    env_add x p
  in

  let goal_args, goal_scalars, goal_poly = poly_of_goal (Goal goal) in


  let rec convert_term (t:term) = match t with
    | TVar {node=v;_} | TInternal v -> env_get v
    | TCons {node=Cons c;_} -> begin
      if c = nat_zero then Poly.zero
      else if c = nat_one then Poly.unit
      else if c = nat_large then Poly.scale (Scalar.of_int 1000) (Poly.of_mono Mono.unit)
      else if c = goal.polynomial then goal_poly
      else (* Poly.scale (Scalar.Cons c) (Poly.unit) *)
        Poly.of_mono (Mono.of_var (C c))
      end
    | TApp {tfun = TCons {node = Cons c; _}; args; _} ->
      if c = nat_add then
        List.fold_left (fun acc t -> Poly.add acc (convert_term t)) Poly.zero args
      else if c = nat_mult then
        List.fold_left (fun acc t -> Poly.mult acc (convert_term t)) Poly.unit args
      else if c = goal.polynomial then begin
        if (List.length args <> goal.args_number) then
          Misc.fail_invariant_break
            "Infered polynomial cannot be evaluated due to bad sorting";
        let args = List.map convert_term args in
        let instanciate_args v =
          let rec find_idx i = function
            | w::t -> if v=w then i else find_idx (i+1) t
            | [] ->
              Misc.fail_invariant_break "Infered polynomial has an unexpected free variable" in
          List.nth args (find_idx 0 goal_args) in
        Poly.subst instanciate_args goal_poly
        end
      else
        fail_untranslatable "Application with an uninterpreatble head"
    | TApp {tfun = TApp {tfun;args=args1;_}; args=args2; loc} ->
      convert_term (TApp {tfun; args = args1 @ args2; loc})
    | TApp {tfun = (TVar _ | TInternal _ | TCons _); _} ->
      fail_untranslatable "Application with an uninterpreatble head"
    | TCons _ ->
      fail_untranslatable "Undefined constructor"
  in

  let convert_eqn eqn =
    let res = match eqn with
      | Eq (t,u,_) -> Poly.sub (convert_term t) (convert_term u)
      | Rel _ -> fail_untranslatable "unsupported relation" in
    (* Format.fprintf std_formatter "// %a -> %a\n" pp_eqn eqn (Poly.pp ~for_mzn:false) res; *)
    res in



  let output = output goal.polynomial goal_args goal_poly in

  let convert_eqns (eqns : eqn list) = List.map convert_eqn eqns in

  let rec convert_fol (vars : polyVar list) (f : formula) =
    match f with
    | PTrue -> []
    | PFalse -> fail_untranslatable "Refutation patterns are unsupported"
    | PLoc (_, f) -> convert_fol vars f
    | PEqn eqns -> convert_eqns eqns
    | PAnd fs | PCases fs -> List.concat (List.map (convert_fol vars) fs)
    | (PForall (xs,ys,eqns,eqns',f)) ->
      List.iter add_var xs;
      let vars = vars @ (List.map (fun v -> V v) xs) in
      List.iter (add_param vars) ys;
      if eqns <> [] then fail_untranslatable "Implications are unsupported";
      (convert_eqns eqns') @ (convert_fol vars f)
    | PExists (xs,ys,eqns,f) ->
      List.iter (add_param vars) (xs@ys);
      (convert_eqns eqns) @ (convert_fol vars f) in

  {
    variables = !globals;
    null_constraints = convert_fol [] f;
    objectives = goal_scalars;
    objective_doc = output
  }
