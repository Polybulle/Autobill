open FirstOrder
open Misc

type ('sort, 'rel, 'var, 'term) compress_quantifiers_t =
  | Univ of 'var list * ('sort, 'rel, 'term) eqn list
  | Exist of 'var list * ('sort, 'rel, 'term) eqn list


let rec compress_logic c =

  let canary = ref true in

  let kill () = (canary := false) in

  let rec compress_eqns eqns =
    let rec remove_ids = function
    | [] -> []
    | eqn::eqns -> match eqn with
      | Eq (a,b,_) when a = b -> kill (); remove_ids eqns
      | _ -> eqn :: (remove_ids eqns) in
    List.fold_left insert_nodup [] (remove_ids eqns)

  and advance c ctx = match c with
    | PTrue | PEqn [] -> shortcut_true ctx
    | PFalse -> shortcut_false ctx
    | PEqn eqns -> backtrack (PEqn (compress_eqns eqns)) ctx
    | PLoc (loc, c) -> advance c (lift_loc loc ctx)
    | PAnd [] -> backtrack PTrue ctx
    | PAnd (x::xs) -> advance x (lift_and xs ctx)
    | PExists ([], [], x) -> kill (); advance x ctx
    | PForall ([], [], x) -> kill (); advance x ctx
    | PExists (vs, eqns, x) ->
      advance x (lift_quant (Exist (vs, compress_eqns eqns)) ctx)
    | PForall (vs, eqns, x) ->
      advance x (lift_quant (Univ (vs, compress_eqns eqns)) ctx)

  and backtrack c ctx = match ctx with
    | KEmpty -> c
    | KLoc (loc, ctx) -> backtrack (PLoc (loc, c)) ctx
    | KAnd ([], ctx, []) -> kill (); backtrack c ctx
    | KAnd (xs, ctx, []) -> backtrack (PAnd (c::xs)) ctx
    | KAnd (xs, ctx, y::ys) -> advance y (KAnd (c::xs, ctx, ys))
    | KForall (vs, eqns, ctx) -> backtrack (PForall (vs, eqns, c)) ctx
    | KExists (vs, eqns, ctx) -> backtrack (PExists (vs, eqns, c)) ctx

  and lift_loc loc = function
    | KLoc (_, ctx) -> kill (); KLoc (loc, ctx)
    | ctx -> KLoc (loc, ctx)

  and lift_quant vs ctx = match vs,ctx with
    | Exist (vs, eqns), KExists (vs', eqns', ctx') ->
      let vs = List.fold_left Misc.insert_nodup vs vs' in
      kill (); KExists (vs, eqns@eqns', ctx')
    | Univ (vs, eqns), KForall (vs', eqns', ctx') ->
      let vs = List.fold_left Misc.insert_nodup vs vs' in
      kill (); KForall (vs, eqns@eqns', ctx')
    | Exist (vs, eqns), _ ->
      let vs = List.fold_left Misc.insert_nodup [] vs  in
      KExists (vs, eqns, ctx)
    | Univ (vs, eqns), _ ->
      let vs = List.fold_left Misc.insert_nodup [] vs  in
      KForall (vs, eqns, ctx)

  and lift_and cs ctx = match ctx with
    | KAnd (xs, ctx, ys) -> kill (); KAnd (xs, ctx, cs @ ys)
    | ctx -> KAnd ([], ctx, cs)

  and shortcut_false ctx = match ctx with
    | KEmpty -> PFalse
    | KLoc (_, ctx)
    | KAnd (_, ctx, _)
    | KExists (_, _, ctx) -> kill (); shortcut_false ctx
    | KForall _ -> backtrack PFalse ctx

  and shortcut_true ctx = match ctx with
    | KEmpty -> PTrue
    | KLoc (_, ctx)
    | KForall (_, _, ctx) -> kill (); shortcut_true ctx
    | KAnd (xs, ctx, []) -> backtrack (PAnd xs) ctx
    | KAnd (xs, ctx, y::ys) -> advance y (KAnd (xs, ctx, ys))
    | KExists _ -> backtrack PTrue ctx in

  let c = advance c KEmpty in
  if !canary then c else compress_logic c


open Types

type 'term fol_var_multiplicity =
  | Not_used
  | Only_Root of 'term
  | Some_Non_Root

let remove_useless_vars (type a) con =

  let module S =
    Map.Make (struct
      type t = a let
      compare = compare
    end) in

  let _vars = ref S.empty in

  let add var term =
    let upd x = match x with
      | None -> assert false
      | Some Not_used -> Some (Only_Root term)
      | Some (Only_Root _) -> x
      | Some Some_Non_Root -> x in
    _vars := S.update var upd !_vars in

  let add_binder var = _vars := S.add var Not_used !_vars in

  let add_term var = _vars := S.add var Some_Non_Root !_vars in

  let rec fill_out c = match c with
    | PTrue | PFalse -> ()
    | PLoc (_, c) -> fill_out c
    | PEqn eqns -> List.iter fill_out_eqn eqns
    | PAnd cs -> List.iter fill_out cs
    | PForall (vars, eqns, c)
    | PExists (vars, eqns, c) ->
      List.iter add_binder vars;
      List.iter fill_out_eqn eqns;
      fill_out c

  and fill_out_eqn = function
    | Eq ((TVar {node;_} | TInternal node), term, _)
    | Eq (term, (TVar {node;_} | TInternal node), _) -> add node term
    | Eq (term1, term2, _) -> fill_out_term term1; fill_out_term term2
    | Rel (_, terms) -> List.iter fill_out_term terms

  and fill_out_term = function
    | TVar {node;_} | TInternal node -> add_term node
    | TPos typ -> fill_out_term typ
    | TNeg typ -> fill_out_term typ
    | TFix t -> fill_out_term t
    | TBox {node;_} -> fill_out_term node
    | TCons _ -> ()
    | TApp {tfun;args;_} ->
      fill_out_term tfun; List.iter fill_out_term args in

  let replace vars =
    let aux x = match S.find_opt x !_vars with
      | None -> assert false
      | Some (Only_Root _) -> false
      | Some Not_used -> false
      | Some Some_Non_Root -> true in
    List.filter aux vars in

  let rec subst typ = match typ with
    | TVar {node;_} | TInternal node ->
      begin match S.find_opt node !_vars with
        | Some (Only_Root t) -> t
        | _ -> TInternal node
      end
    | TPos typ -> subst typ
    | TNeg typ -> subst typ
    | TFix t -> TFix (subst t)
    | TBox {kind;node;loc} -> TBox {kind; loc; node = subst node}
    | TCons c -> TCons c
    | TApp {tfun;args;loc} ->
      TApp {tfun = subst tfun; args = List.map subst args; loc} in

  let rec go c = match c with
    | PTrue | PFalse -> c
    | PLoc (loc, c) -> PLoc (loc, go c)
    | PEqn eqns -> PEqn (List.map go_eqn eqns)
    | PAnd cs -> PAnd (List.map go cs)
    | PExists (vars, eqns, c) ->
      PExists (replace vars, List.map go_eqn eqns, go c)
    | PForall (vars, eqns, c) ->
      PForall (replace vars, List.map go_eqn eqns, go c)

  and go_eqn = function
    | Eq (a, b, so) -> Eq (subst a, subst b, so)
    | Rel (rel, args) -> Rel (rel, List.map subst args) in

  fill_out con; go con


let normalize con = compress_logic (remove_useless_vars con)
