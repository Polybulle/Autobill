open Misc
open Sexpr

type rel = string

type 'term eqn =
  | Eq of 'term * 'term
  | Rel of rel * 'term list

type ('var, 'term) formula =
    | PTrue
    | PFalse
    | PLoc of position * ('var, 'term) formula
    | PEqn of 'term eqn list
    | PAnd of ('var, 'term) formula list
    | PExists of 'var list * 'term eqn list * ('var, 'term) formula
    | PForall of 'term list * 'term eqn list * ('var, 'term) formula

type ('var, 'term) ctx =
  | KEmpty
  | KLoc of position * ('var, 'term) ctx
  | KAnd of ('var, 'term) formula list * ('var, 'term) ctx * ('var, 'term) formula list
  | KForall of 'var list * 'term eqn list * ('var, 'term) ctx
  | KExists of 'var list * 'term eqn list * ('var, 'term) ctx

let rel_to_string rel = rel

let eqns_to_sexpr pp_term eqns =
  let go = function
    | Eq (a,b) -> S [K "="; pp_term a; pp_term b]
    | Rel (rel, args) -> S (K (rel_to_string rel) :: List.map pp_term args) in
  match eqns with
  | [] -> K "T"
  | [eqn] -> go eqn
  | _ -> S (K "&" :: List.map go eqns)

let rec formula_to_sexpr pp_var pp_term = function
    | PTrue -> V "T"
    | PFalse -> V "F"
    | PEqn eqns -> eqns_to_sexpr pp_term eqns
    | PExists (vars, eqns, con) ->
      let l = match vars with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var vars) in
      S [K "∃"; l; eqns_to_sexpr pp_term eqns; formula_to_sexpr pp_var pp_term con]
    | PForall (vars, eqns, con) ->
      let l = match vars with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var vars) in
      S [K "∀"; l; eqns_to_sexpr pp_term eqns; formula_to_sexpr pp_var pp_term con]
    | PAnd cons ->
      S (K "&" :: List.map (formula_to_sexpr pp_var pp_term) cons)
    | PLoc (loc, c) ->
      S [K "loc"; V (string_of_position loc); formula_to_sexpr pp_var pp_term c]

let string_of_formula pp_var pp_term f =
  Sexpr.to_string (formula_to_sexpr pp_var pp_term f)

let map_eqns f eqns =
  let go = function
    | Eq (a,b) -> Eq (f a, f b)
    | Rel (rel, args) -> Rel (rel, List.map f args) in
  List.map go eqns

let rec map f_var f_term= function
  | PTrue -> PTrue
  | PFalse -> PFalse
  | PLoc (loc, c) -> PLoc (loc, map f_var f_term c)
  | PEqn eqns -> PEqn (map_eqns f_term eqns)
  | PAnd cs -> PAnd (List.map (map f_var f_term) cs)
  | PExists (xs, eqns, c) ->
    PExists (List.map f_var xs, map_eqns f_term eqns, map f_var f_term c)
  | PForall (xs, eqns, c) ->

    PForall (List.map f_var xs, map_eqns f_term eqns, map f_var f_term c)
type ('var, 'term) compress_quantifiers_t =
  | Univ of 'var list * 'term eqn list
  | Exist of 'var list * 'term eqn list


let rec compress_logic c =

  let canary = ref false in

  let kill () = (canary := true) in

  let rec advance c ctx = match c with
    | PTrue
    | PFalse -> shortcut_false ctx
    | PEqn [] -> backtrack PTrue ctx
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
    | KAnd ([], ctx, ys) -> backtrack (PAnd (c::ys)) ctx
    | KAnd (x::xs, ctx, ys) -> advance x (KAnd (xs, ctx, c::ys))
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
    | KAnd (xs, ctx, ys) -> kill (); KAnd (cs @ xs, ctx, ys)
    | ctx -> ctx

  and shortcut_false ctx = match ctx with
    | KEmpty -> PFalse
    | KLoc (_, ctx)
    | KAnd (_, ctx, _)
    | KExists (_, _, ctx) -> kill (); shortcut_false ctx
    | KForall _ -> backtrack PFalse ctx in

  let c = advance c KEmpty in
  if !canary then compress_logic c else c
