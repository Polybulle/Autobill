open Vars
open FirstOrder
open FullFOL
open Primitives
open Format

open Polynomials

let convert_to_optimization f (Goal goal : Ast.FullAst.goal) =

  let globals = ref [] in
  let mk_param () =
    let p = TyVar.fresh () in
    globals := p :: !globals;
    p in
  let goal_args : TyVar.t list = List.init goal.args_number (fun _ -> TyVar.fresh ()) in
  let goal_poly =
    let acc = ref Poly.zero in
    List.iter (fun arg ->
        let a = mk_param () in
        acc := Poly.add !acc (Poly.scale (Scalar.of_param a) (Poly.of_mono (Mono.of_var arg)))
      ) goal_args;
    !acc
  in

  let env = ref TyVar.Env.empty in
  let env_add x t = (env := TyVar.Env.add x t !env) in
  let env_get x = TyVar.Env.find x !env in

  let add_var x = env_add x (Poly.of_mono (Mono.of_var x)) in
  let add_param vars x =
    let t = List.fold_left (fun p v ->
        let a = mk_param () in
        Poly.add p (Poly.scale (Scalar.of_param a) (Poly.of_mono (Mono.of_var v)))
      ) Poly.zero vars in
    let cst_term = mk_param () in
    env_add x (Poly.add t (Poly.scale (Param cst_term) Poly.unit))
  in

  let rec convert_term (t:term) = match t with
    | TVar {node=v;_} | TInternal v -> env_get v
    | TCons {node=Cons c;_} -> begin
        if c = nat_zero then Poly.zero
        else if c = nat_one then Poly.unit
        else if c = nat_large then Poly.scale (Scalar.of_int 1000) (Poly.of_mono Mono.unit)
        else failwith "unimplemented parameter constant"
      end
    | TApp {tfun = TCons {node = Cons c; _}; args; _} ->
      if c = nat_add then
        List.fold_left (fun acc t -> Poly.add acc (convert_term t)) Poly.zero args
      else if c = nat_mult then
        List.fold_left (fun acc t -> Poly.mult acc (convert_term t)) Poly.zero args
      else if c = goal.polynomial then begin
          assert (List.length args = goal.args_number);
          let args = List.map convert_term args in
          goal_poly |> Poly.subst (fun v ->
              let rec find_idx i = function
                | w::t -> if v=w then i else find_idx (i+1) t
                | [] -> assert false in
              let i = find_idx 0 goal_args in
              List.nth args i
            )
        end
      else
        failwith "unsupported"
    | _ -> assert false (* Impossible on indices *) in

  let convert_eqn eqn =
    match eqn with
    | Eq (t,u,_) -> Poly.sub (convert_term t) (convert_term u)
    | Rel _ -> failwith "unimplemented" in

  let convert_eqns (eqns : eqn list) =
    List.map convert_eqn eqns in

  let rec convert_fol vars (f : formula) =
    match f with
    | PTrue -> []
    | PFalse -> failwith "unimplemented"
    | PLoc (_, f) -> convert_fol vars f
    | PEqn eqns -> convert_eqns eqns
    | PAnd fs | PCases fs -> List.concat (List.map (convert_fol vars) fs)
    | PForall (xs,ys,eqns,f) ->
      List.iter add_var (xs@ys);
      assert (eqns = []);
      convert_fol (xs@ys@vars) f
    | PExists (xs,ys,eqns,f) ->
      List.iter (add_param vars) (xs@ys);
      (convert_eqns eqns) @ (convert_fol vars f) in

  let f = convert_fol [] f in
  let goal = Poly.eval (fun _ -> Scalar.of_int 1000) goal_poly in
  (!globals, f, goal)


let pp_solution fmt (globals, polys, goal) =
  let pp_global fmt v = fprintf fmt "var int: %a; constraint %a >= 0;" TyVar.pp v TyVar.pp v in
  let pp_scalar fmt (_,v) = fprintf fmt "constraint %a = 0;" Scalar.pp v in
  let pp_poly fmt p = pp_print_list pp_scalar fmt (Poly.P.bindings p) in
  let pp_goal fmt p = fprintf fmt "solve minimize %a;" Scalar.pp p in
  fprintf fmt "@[<v 0>%a@.%a@.%a@.@]"
    (pp_print_list pp_global) globals
    (pp_print_list pp_poly) polys
    pp_goal goal
