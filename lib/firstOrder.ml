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
    | PForall of 'var list * 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) formula

type ('sort, 'rel, 'var, 'term) ctx =
  | KEmpty
  | KLoc of position * ('sort, 'rel, 'var, 'term) ctx
  | KAnd of ('sort, 'rel, 'var, 'term) formula list
            * ('sort, 'rel, 'var, 'term) ctx
            * ('sort, 'rel, 'var, 'term) formula list
  | KForall of 'var list * 'var list * ('sort, 'rel, 'term) eqn list * ('sort, 'rel, 'var, 'term) ctx
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
    | PForall (vars, exist, eqns, con) ->
      let l = match vars with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var vars) in
      let ll = match exist with
        | [v] -> pp_var v
        | _ -> S (List.map pp_var exist) in
      S [K "∀"; l;
         K "∃"; ll;
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
    PExists (List.map f_var xs,
             map_eqns f_term eqns,
             map f_var f_term c)
  | PForall (xs, ys, eqns, c) ->
    PForall (List.map f_var xs,
             List.map f_var ys,
             map_eqns f_term eqns,
             map f_var f_term c)
