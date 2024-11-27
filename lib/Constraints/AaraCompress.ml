open FirstOrder
open FullFOL
open Prelude
open Constraint_common

module Subst = Map.Make (struct
    type t = var
    let compare = compare
  end)

let rec apply_term s (t : term) = match t with
  | TCons _ -> t
  | TApp {tfun;args;loc} -> TApp {
      tfun = apply_term s tfun;
      args = List.map (apply_term s) args;
      loc }
  | TVar {node=v;_}| TInternal v -> match Subst.find_opt v s with
    | None -> t
    | Some t -> apply_term s t

let apply_eqn s eqn =
  match eqn with
    | Eq (a,b,so) -> Eq (apply_term s a, apply_term s b, so)
    | Rel (r, args) -> Rel (r, List.map (apply_term s) args) 

let is_valid_for rank x t =
  let vs = freevars_of_typ t in
  let r_max =
    if vs = [] then Int.max_int else
    List.fold_left (fun r v -> min r (rank v)) Int.max_int vs in
  (not (List.mem x vs)) && r_max >= (rank x)

let add_binding s x t = Subst.add x t s

(* let extend_with_override s s' = *)
(*     let s' = Subst.map (apply_term s) s' in *)
(*     Subst.merge (fun _ a b -> *)
(*       match a,b with *)
(*       | Some x, None | None, Some x -> Some x *)
(*       | None, None -> None *)
(*       | Some _, Some x -> Some x *)
(*     ) s s' *)

let eqn_to_subst subst (rank : var -> int) eqn =
  let eqn = apply_eqn subst eqn in
  let remove_eqn = (subst, None) in
  let no_change = (subst, Some eqn) in
  let add x t = (Subst.add x t subst, None) in
  match eqn with
  | Rel _ -> no_change
  | Eq (a,b,_) -> begin
    match a, b with
    | (TVar {node=x;_} | TInternal x) , (TVar {node=y;_} | TInternal y) ->
      if x = y then remove_eqn
      else if (rank y) < (rank x) then add x b
      else add y a
    | (TVar {node=x;_} | TInternal x), t
    | t, (TVar {node=x;_} | TInternal x) ->
      if is_valid_for rank x t then add x t else no_change
    | _ -> no_change
    end

let eqns_to_subst rank subst eqns =

  let rec go subst acc = function
    | [] -> subst, acc
    | eqn::rest ->
      match eqn_to_subst subst rank eqn with
      | subst, Some eqn -> go subst (eqn::acc) rest
      | subst, None -> go subst acc rest in

  let subst, eqns = go subst [] eqns in
  subst, List.map (apply_eqn subst) eqns

let compress_unification optim =

  let ranks = ref Subst.empty in
  let get x =
    try (Subst.find x !ranks) with _ -> raise (Failure (Vars.TyVar.to_string x)) in
  let set r x = ranks := Subst.add x r !ranks in

  let rec transform s r f =
    match f with

    | PTrue | PFalse -> f
    | PEqn eqns -> PEqn (List.map (apply_eqn s) eqns)
    | PLoc (loc, f) -> PLoc (loc, transform s r f)
    | PAnd fs -> PAnd (List.map (transform s r) fs)
    | PCases fs -> PCases (List.map (transform s r) fs)

    | PExists (xs, ys, eqns, f) ->
      List.iter (set (r+1)) xs;
      List.iter (set (r+2)) ys;
      let s', eqns = eqns_to_subst get s eqns in
      let f = transform s' (r+2) f in
      let fvs =
        List.of_seq (S.to_seq (S.union (freevars_of_formula f) (freevars_of_eqns eqns))) in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PExists (filter xs, filter ys, eqns, f)

    | PForall (xs, ys, eqns, eqns', f) ->
      List.iter (set (r+1)) xs;
      List.iter (set (r+2)) ys;
      let s', eqns = eqns_to_subst get s eqns in
      let s'', eqns' = eqns_to_subst get s' eqns' in
      let f = transform s'' (r+2) f in
      let fvs =
        let fvs = freevars_of_formula f in
        let fvs = S.union fvs (freevars_of_eqns eqns) in
        let fvs = S.union fvs (freevars_of_eqns eqns') in
        List.of_seq (S.to_seq fvs) in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PForall (filter xs, filter ys, eqns, eqns', f) in

  {optim with formula = transform Subst.empty 0 optim.formula}
