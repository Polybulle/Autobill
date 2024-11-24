open FirstOrder
open FullFOL


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

let is_cyclic_for x t = List.mem x (freevars_of_typ t)

let add_binding s x t = Subst.add x t s

let extend_with_override s s' =
    let s' = Subst.map (apply_term s) s' in
    Subst.merge (fun _ a b ->
      match a,b with
      | Some x, None | None, Some x -> Some x
      | None, None -> None
      | Some x, Some _ -> Some x
    ) s s'

let eqn_to_subst rank eqn = match eqn with
  | Rel _ -> None
  | Eq (a,b,_) -> match a, b with
    | ((TVar {node=x;_} | TInternal x) as t) , ((TVar {node=y;_} | TInternal y) as u) ->
      if x = y then
        Some Subst.empty
      else if rank y < rank x && not (is_cyclic_for x u) then
        Some (Subst.singleton x u)
      else if not (is_cyclic_for y t) then
        Some (Subst.singleton y t)
      else
        None
    | (TVar {node=x;_} | TInternal x), t
    | t, (TVar {node=x;_} | TInternal x) ->
      if is_cyclic_for x t then None else Some (Subst.singleton x t)
    | _ -> None

let rec eqns_to_subst rank eqns =

  let rec go subst acc = function
    | [] -> (subst, acc)
    | eqn::rest -> match eqn_to_subst rank eqn with
      | Some s -> go (extend_with_override subst s) acc rest
      | None -> go subst (eqn::acc) rest in

  go Subst.empty [] eqns

let substitute_variables f =

  let ranks = ref Subst.empty in
  let get_rank x =
    try Subst.find x !ranks with _ -> raise (Failure (Vars.TyVar.to_string x)) in
  let set_rank r x = ranks := Subst.add x r !ranks in

  (* let rec build r f = match f with *)
  (*   | PTrue | PFalse -> Subst.empty *)
  (*   | PLoc (_, f) -> build r f *)
  (*   | PEqn eqns -> eqns_to_subst get_rank eqns *)
  (*   | PAnd fs -> *)
  (*     List.fold_left extend_with_override Subst.empty (List.map (build r) fs) *)
  (*   | PCases fs -> *)
  (*     List.fold_left extend_with_override Subst.empty (List.map (build r) fs) *)
  (*   | PExists (xs, ys, eqns, f) -> *)
  (*     List.iter (set_rank (r+1)) xs; *)
  (*     List.iter (set_rank (r+2)) ys; *)
  (*     let s = eqns_to_subst get_rank eqns in *)
  (*     let s' = build (r+2) f in *)
  (*     extend_with_override s' s *)
  (*   | PForall (xs, ys, _, _, _) -> *)
  (*     List.iter (set_rank (r+1)) xs; *)
  (*     List.iter (set_rank (r+2)) ys; *)
  (*     Subst.empty in *)

  (* let rec apply s f = match f with *)
  (*   | PTrue | PFalse -> f *)
  (*   | PLoc (loc, f) -> PLoc (loc, apply s f) *)
  (*   | PAnd fs -> PAnd (List.map (apply s) fs) *)
  (*   | PEqn eqns -> PEqn (List.map (apply_eqn s) eqns) *)
  (*   | PCases fs -> PCases (List.map (apply s) fs) *)
  (*   | PExists (xs, ys, eqns, f) -> *)
  (*     PExists (xs, ys, List.map (apply_eqn s) eqns, apply s f) *)
  (*   | PForall (xs, ys, eqns, eqns', f) -> *)
  (*     PForall (xs, ys, List.map (apply_eqn s) eqns, List.map (apply_eqn s) eqns', apply s f) in *)

  let rec transform s r f =
    match f with
    | PTrue | PFalse | PEqn _ -> f
    | PLoc (loc, f) -> PLoc (loc, transform s r f)
    | PAnd fs -> PAnd (List.map (transform s r) fs)
    | PCases fs -> PCases (List.map (transform s r) fs)
    | PExists (xs, ys, eqns, f) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      let f = transform s (r+2) f in
      let fvs =
        List.of_seq (S.to_seq (S.union (freevars_of_formula f) (freevars_of_eqns eqns))) in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PExists (filter xs, filter ys, eqns, transform s (r+2) f)
    | PForall (xs, ys, eqns, eqns', f) ->
      List.iter (set_rank (r+1)) xs;
      List.iter (set_rank (r+2)) ys;
      let s, eqns = eqns_to_subst get_rank (List.map (apply_eqn s) eqns) in
      let eqns' = List.map (apply_eqn s) eqns' in
      let f = transform s (r+2) f in
      let fvs =
        let fvs = freevars_of_formula f in
        let fvs = S.union fvs (freevars_of_eqns eqns) in
        let fvs = S.union fvs (freevars_of_eqns eqns') in
        List.of_seq (S.to_seq fvs) in
      let filter = List.filter (fun x -> List.mem x fvs) in
      PForall (filter xs, filter ys, eqns, eqns', f) in

  transform Subst.empty 0 f

let compress_unification f =
  substitute_variables f
