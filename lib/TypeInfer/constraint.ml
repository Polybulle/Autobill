open UnionFind
open Sexpr


module Make (U : Unifier_params) = struct

  exception Invariant_break of string

  include UnionFind.Make (U)

  type con =
    | CTrue
    | CFalse
    | CEq of uvar * uvar
    | CAnd of con list
    | CExists of uvar list * con
    | CVar of string * uvar * specializer
    | CInst of uvar * scheme * specializer
    (* Définition d'une valeur polymorphique: CLet (x,a,c1,c2,quant,gen)
     * réprésente "let x = \a.c1 in c2".  *)
    | CLet of string * scheme * con * con * generalizer
    | CDef of string * uvar * con
    | CDecl of string * uvar * con

  type kontext =
    | KEmpty
    | KAnd of con list * kontext
    | KDef of string * uvar * kontext
    | KLet1 of string * uvar list * kontext * uvar * con * generalizer
    | KLet2 of string * scheme * kontext

  let eq u v = CEq (u,v)

  let cvar x u spec = CVar(x, u, spec)

  let instance u fvs v spec =  CInst (u, (fvs, v), spec)

  let ( @+ ) c d = CAnd [c;d]

  let exists xs con = CExists (xs,con)

  let rec con_to_sexpr pp = function
    | CTrue -> V "T"
    | CFalse -> V "F"
    | CDecl (x,u,con) ->
      S [K "decl"; S [V x; pp u]; con_to_sexpr pp con]
    | CEq (a, b) ->
      S [pp a; V "="; pp b]
    | CAnd cons ->
      S (K "&" :: List.map (con_to_sexpr pp) cons)
    | CExists (vars, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∃"; l; con_to_sexpr pp con]
    | CVar (x, t, _) ->
      S [V x; V ":"; pp t]
    | CInst (u, s, _) ->
      S [pp u; V "≤"; scheme_to_sexpr pp s]
    | CDef (x, t, c) ->
      S [K "def"; S [V x; pp t]; con_to_sexpr pp c]
    | CLet (var, (us, u), icon, kcon, _) ->
      S [K "let";
         V var;
         S [K "∀"; S (List.map pp us); pp u];
         con_to_sexpr pp icon;
         con_to_sexpr pp kcon]

  let kontext_to_sexpr ctx =
    let pp_uvar = uvar_to_sexpr in
    let pp_con = con_to_sexpr pp_uvar in
    let rec _aux ctx acc = match ctx with
      | KEmpty -> acc
      | KAnd (cons, ctx) ->
        _aux ctx (S [K "and"; block (List.map pp_con cons); acc])
      | KDef (v,u,ctx) ->
        _aux ctx (S [K "def"; V v; pp_uvar u; acc])
      | KLet1 (x, us, ctx, u, con, _) ->
        _aux ctx (S [K "letting"; V x; S (List.map pp_uvar us);
                     pp_uvar u;
                     K "kont"; pp_con con; acc])
      | KLet2 (x, s, ctx) ->
        _aux ctx (S [K "let-ed"; V x; scheme_to_sexpr pp_uvar s; acc]) in
    _aux ctx (V "###")

  let trace stack con =
    buffer ();
    print_endline "\n** context";
    print_sexpr (kontext_to_sexpr stack);
    print_endline "\n** constraint";
    print_sexpr (con_to_sexpr uvar_to_sexpr con);
    print_endline "\n** env";
    print_sexpr (_env_to_sexpr ());
    print_endline "\n** unification";
    print_sexpr (subst_to_sexpr !_state)



  let rec compress_cand c =
    let rec acc cc c = match c with
      | CAnd [] -> cc
      | CAnd ((CAnd t1)::t2) -> acc cc (CAnd (t1 @ t2))
      | CAnd (h::t) -> acc ((compress_cand h)::cc) (CAnd t)
      | CExists (u, con) -> CExists (u, compress_cand con) :: cc
      | CDef (x,u,con) -> CDef (x,u,compress_cand con) :: cc
      | CDecl (x,u,con) -> CDecl (x,u,compress_cand con) :: cc
      | CLet (x,s,con1,con2,gen) ->
        CLet (x,s, compress_cand con1, compress_cand con2,gen) :: cc
      | _ -> [c] @ cc in
    match acc [] c with
    | [d] -> d
    | l -> CAnd l

  let float_cexists c =
    let rec aux c = match c with
      | CExists (u,d) ->
        let fv,d = aux d in u@fv,d
      | CDef (x,u,d) ->
        let fv,d = aux d in
        let d = if fv = [u] || fv = [] then d else CExists (fv,d) in
        [], CDef (x,u,d)
      | CDecl (x,u,d) ->
        let fv,d = aux d in fv, CDecl (x,u,d)
      | CAnd cc ->
        let fvs, cc = List.split (List.map aux cc) in
        let fvs = List.concat fvs in
        fvs, CAnd cc
      | CLet (x,s, con1,con2, gen) ->
        let (fv1,con1), (fv2,con2) = aux con1, aux con2 in
        let con1 = if fv1 = [] then con1 else CExists (fv1,con1) in
        fv2, CLet (x,s, con1,con2, gen)
      | _ -> [],c in
    let fv,c = aux c in
    CExists (fv,c)

  let rec lookup_scheme stack x = match stack with
    | KEmpty -> raise
                  (Failure ("Broken invariant: Unbound var during constraint solving: " ^ x))
    | KAnd (_, ctx) -> lookup_scheme ctx x
    | KDef (y,a,ctx) ->
      if x = y then ([], a) else lookup_scheme ctx x
    | KLet1 (_, _, ctx, _, _, _) -> lookup_scheme ctx x
    | KLet2 (y, s, ctx) ->
      if x = y then refresh_scheme s else lookup_scheme ctx x

  let rec lift_exist us stack = match stack with
    | KEmpty -> KEmpty
    | KAnd (cons, ctx) -> KAnd (cons, lift_exist us ctx)
    | KDef (x,a,ctx) -> KDef (x,a, lift_exist us ctx)
    | KLet1 (x, vs, ctx, t, con, gen) ->
      KLet1 (x, vs @ us, ctx, t, con, gen)
    | KLet2 (x, s, ctx) ->
      KLet2 (x, s, lift_exist us ctx)

  let ( >>> ) con gen = con, gen


  exception Done

  let rec advance stack con = match con with

    | CTrue -> stack
    | CFalse -> raise (Failure ("Solving bottomed out of CFalse"))
    | CEq (a,b) -> lift_exist [unify a b] stack
    | CAnd [] -> stack
    | CAnd [con] -> advance stack con
    | CAnd (h::t) -> advance (KAnd (t, stack)) h
    | CExists (us, con) -> advance (lift_exist us stack) con
    | CDef (x,u,con) -> advance (KDef (x,u,stack)) con
    | CDecl (_,_, con) -> advance stack con

    | CVar (x,u,spec) ->
      let (vs, v) = lookup_scheme stack x in
      let stack = lift_exist vs stack in
      let w = unify u v in
      specialize spec vs;
      lift_exist [w] stack

    | CInst (u, (vs, v), spec) ->
      (* No need to refresh scheme here, it is single-use *)
      let stack = lift_exist vs stack in
      let w = unify u v in
      specialize spec vs;
      lift_exist [w] stack

    | CLet (x, (us, u), con1, con2, gen) ->
      enter ();
      let stack = KLet1 (x, us, stack, u, con2, gen) in
      advance stack con1

  and backtrack ?trace:(do_trace=false) stack =
    if do_trace then trace stack CTrue;
    match stack with

    | KEmpty -> raise Done
    | KAnd ([], stack) -> backtrack ~trace:do_trace stack
    | KAnd ([con],stack) -> stack, con
    | KAnd (h::t, stack) -> KAnd (t, stack), h
    | KDef (x, u, stack) -> define x ([],u); backtrack ~trace:do_trace stack
    | KLet2 (_, _, stack) -> backtrack ~trace:do_trace stack

    | KLet1 (x, us, stack', u, con2, gen) ->
      let tmp mess s =
        if do_trace then begin
          print_string (mess ^ " ");
          print_sexpr (scheme_to_sexpr uvar_to_sexpr s) end in


      (* Shorten paths we will take, normalize the variables *)
      let u = repr u in
      let us = List.map repr us in
      let us = List.fold_left Util.insert_nodup [] us in
      if not (occurs_check (us, u)) then begin
        print_sexpr (subst_to_sexpr !_state);
        raise (Cycle u);
      end;
      let s = (us, u) in

      (* lower each variable, lowered ranked vars first *)
      tmp "passed duplicate removal and cyclic check" s;
      Array.iteri lift_freevars (ranked_freevars_of_scheme s !_rank);

      (* We now know which vars can be lifted in the surronding scope! *)
      if do_trace then begin
        print_endline "=== after lifting ==";
        print_sexpr (subst_to_sexpr !_state)
      end;
      let s,old = extract_old_vars s !_rank in
      let stack' = lift_exist old stack' in
      tmp "after final lifting" s;

      leave ();
      define x s;
      generalize x gen s;
      KLet2 (x, s, stack'), con2

end
