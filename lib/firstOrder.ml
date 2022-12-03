open Misc
open Sexpr

type ('sort, 'rel, 'term) eqn =
  | Eq of 'term * 'term * 'sort
  | Rel of 'rel * 'term list

type ('sort, 'rel, 'var, 'term) formula =
    | PTrue
    | PFalse
    | PLoc of position * ('sort, 'rel, 'var, 'term) formula
    | PEqn of ('sort, 'rel, 'term) eqn list
    | PAnd of ('sort, 'rel, 'var, 'term) formula list
    | PExists of 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) formula
    | PForall of 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) formula

type ('sort, 'rel, 'var, 'term) ctx =
  | KEmpty
  | KLoc of position * ('sort, 'rel, 'var, 'term) ctx
  | KAnd of ('sort, 'rel, 'var, 'term) formula list
            * ('sort, 'rel, 'var, 'term) ctx
            * ('sort, 'rel, 'var, 'term) formula list
  | KForall of 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) ctx
  | KExists of 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) ctx


let eqns_to_sexpr pp_rel pp_term eqns =
  let go = function
    | Eq (a,b,_) -> S [K "="; pp_term a; pp_term b]
    | Rel (rel, args) -> S (K (pp_rel rel) :: List.map pp_term args) in
  match eqns with
  | [] -> K "T"
  | [eqn] -> go eqn
  | _ -> S (K "&" :: List.map go eqns)

let rec formula_to_sexpr pp_rel pp_var pp_term = function
    | PTrue -> V "T"
    | PFalse -> V "F"
    | PEqn eqns -> eqns_to_sexpr pp_rel pp_term eqns
    | PExists (vars, eqns, con) ->
      let l = match vars with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var vars) in
      S [K "∃"; l;
         eqns_to_sexpr pp_rel pp_term eqns;
         formula_to_sexpr pp_rel pp_var pp_term con]
    | PForall (vars, eqns, con) ->
      let l = match vars with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var vars) in
      S [K "∀"; l;
         eqns_to_sexpr pp_rel pp_term eqns;
         formula_to_sexpr pp_rel pp_var pp_term con]
    | PAnd cons ->
      S (K "&" :: List.map (formula_to_sexpr pp_rel pp_var pp_term) cons)
    | PLoc (loc, c) ->
      S [K "loc"; V (string_of_position loc); formula_to_sexpr pp_rel pp_var pp_term c]

let string_of_formula pp_rel pp_var pp_term f =
  Sexpr.to_string (formula_to_sexpr pp_rel pp_var pp_term f)

let map_eqns f eqns =
  let go = function
    | Eq (a,b, so) -> Eq (f a, f b, so)
    | Rel (rel, args) -> Rel (rel, List.map f args) in
  List.map go eqns

let rec map f_var f_term = function
  | PTrue -> PTrue
  | PFalse -> PFalse
  | PLoc (loc, c) -> PLoc (loc, map f_var f_term c)
  | PEqn eqns -> PEqn (map_eqns f_term eqns)
  | PAnd cs -> PAnd (List.map (map f_var f_term) cs)
  | PExists (xs, eqns, c) ->
    PExists (List.map f_var xs, map_eqns f_term eqns, map f_var f_term c)
  | PForall (xs, eqns, c) ->

    PForall (List.map f_var xs, map_eqns f_term eqns, map f_var f_term c)

type ('sort, 'rel, 'var, 'term) compress_quantifiers_t =
  | Univ of 'var list * ('sort, 'rel, 'term) eqn list
  | Exist of 'var list * ('sort, 'rel, 'term) eqn list


let rec compress_logic c =

  let canary = ref false in

  let kill () = (canary := true) in

  let rec advance c ctx = match c with
    | PTrue | PEqn [] -> shortcut_true ctx
    | PFalse -> shortcut_false ctx
    | PEqn _ -> backtrack c ctx
    | PLoc (loc, c) -> advance c (lift_loc loc ctx)
    | PAnd [] -> backtrack PTrue ctx
    | PAnd (x::xs) -> advance x (lift_and xs ctx)
    | PExists (vs, eqns, x) -> advance x (lift_quant (Exist (vs, eqns)) ctx)
    | PForall (vs, eqns, x) -> advance x (lift_quant (Univ (vs,eqns)) ctx)

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
      kill (); KExists (vs@vs', eqns@eqns', ctx')
    | Univ (vs, eqns), KForall (vs', eqns', ctx') ->
      kill (); KForall (vs@vs', eqns@eqns', ctx')
    | Exist (vs, eqns), _ -> KExists (vs, eqns, ctx)
    | Univ (vs, eqns), _ -> KForall (vs, eqns, ctx)

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
  if !canary then compress_logic c else c
