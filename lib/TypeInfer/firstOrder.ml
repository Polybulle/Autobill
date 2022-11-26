open Misc
open Sexpr

type 'var formula =
    | PTrue
    | PFalse
    | PLoc of position * 'var formula
    | PEq of 'var * 'var
    | PRel of string * 'var list
    | PAnd of 'var formula list
    | PImplies of 'var formula list * 'var formula
    | PExists of 'var list * 'var formula
    | PForall of 'var list * 'var formula

type 'var ctx =
  | KEmpty
  | KLoc of position * 'var ctx
  | KAnd of 'var formula list * 'var ctx * 'var formula list
  | KImplies1 of 'var formula list * 'var ctx * 'var formula list * 'var formula
  | KImplies2 of 'var formula list * 'var ctx
  | KForall of 'var list * 'var ctx
  | KExists of 'var list * 'var ctx

let rec formula_to_sexpr pp = function
    | PTrue -> V "T"
    | PFalse -> V "F"
    | PEq (a, b) -> S [pp a; V "="; pp b]
    | PRel (rel, args) -> S (K rel :: List.map pp args)
    | PExists (vars, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∃"; l; formula_to_sexpr pp con]
    | PForall (vars, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∀"; l; formula_to_sexpr pp con]
    | PAnd cons ->
      S (K "&" :: List.map (formula_to_sexpr pp) cons)
    | PImplies (cons, concl) ->
      S [K "⇒";
         S (K "&" :: List.map (formula_to_sexpr pp) cons);
         formula_to_sexpr pp concl]
    | PLoc (loc, c) ->
      S [K "loc"; V (string_of_position loc); formula_to_sexpr pp c]

let string_of_formula pp f =
  Sexpr.to_string (formula_to_sexpr pp f)

let rec map f = function
  | PTrue -> PTrue
  | PFalse -> PFalse
  | PLoc (loc, c) -> PLoc (loc, map f c)
  | PEq (a, b) -> PEq (f a, f b)
  | PRel (r, xs) -> PRel (r, List.map f xs)
  | PAnd cs -> PAnd (List.map (map f) cs)
  | PImplies (xs,y) -> PImplies (List.map (map f) xs, map f y)
  | PExists (xs, c) -> PExists (List.map f xs, map f c)
  | PForall (xs, c) -> PForall (List.map f xs, map f c)

let compress_logic =

  let rec go c = match c with
    | PEq _ | PRel _ | PTrue | PFalse -> c
    | PLoc (loc, c) -> PLoc (loc, go c)
    | PAnd xs -> (try PAnd (go_and [] xs) with Failure _ -> PFalse)
    | PImplies (xs, y) -> go_implies (go_and [] xs) y
    | PExists (xs, y) -> PExists (xs, go y)
    | PForall (xs, y) -> PForall (xs, y)

  and go_and acc = function
    | [] -> acc
    | x::xs -> match go x with
      | PAnd (ys) -> go_and acc (List.rev_append xs ys)
      | PTrue -> go_and acc xs
      | PFalse -> raise (Failure "")
      | x -> go_and (x::acc) xs

  and go_implies xs y = match go y with
    | PImplies (xs, PImplies (ys, z)) -> go_implies (List.rev_append xs ys) z
    | y -> PImplies (xs, y)

  in go

type 'a compress_quantifiers_t =
  | Univ of 'a list
  | Exist of 'a list

let compress_quantifiers c =

  let rec advance (c : 'var formula) ctx = match c with
    | PTrue
    | PFalse
    | PEq _
    | PRel _ -> backtrack c ctx
    | PLoc (loc, c) -> advance c (KLoc (loc, ctx))
    | PAnd [] -> backtrack PTrue ctx
    | PAnd (x::xs) -> advance x (KAnd ([], ctx, xs))
    | PImplies ([], x) -> advance x ctx
    | PImplies (x::xs, y) -> advance x (KImplies1 ([], ctx, xs, y))
    | PExists (vs, x) -> advance x (lift (Exist vs) ctx)
    | PForall (vs, x) -> advance x (lift (Univ vs) ctx)

  and backtrack c (ctx : 'var ctx) = match ctx with
    | KEmpty -> c
    | KLoc (loc, ctx) -> backtrack (PLoc (loc, c)) ctx
    | KAnd ([], ctx, ys) -> backtrack (PAnd (c::ys)) ctx
    | KAnd (x::xs, ctx, ys) -> advance x (KAnd (xs, ctx, c::ys))
    | KImplies1 ([], ctx, ys, z) -> advance z (KImplies2 (c::ys, ctx))
    | KImplies1 (x::xs, ctx, ys, z) -> advance x (KImplies1 (xs, ctx, c::ys, z))
    | KImplies2 (xs, ctx) -> backtrack (PImplies (xs,c)) ctx
    | KForall (vs, ctx) -> backtrack (PForall (vs, c)) ctx
    | KExists (vs, ctx) -> backtrack (PExists (vs, c)) ctx

  and lift vs ctx = match vs with
    | Exist vs -> KExists (vs, ctx)
    | Univ vs -> KForall (vs, ctx)

  in advance c KEmpty

let normalize_formula c =
  compress_logic (compress_quantifiers (compress_logic c))
