open Sexpr
open Misc
open FirstOrder

include UnionFind

exception Invariant_break of string

type 'var post_con =
    | PTrue
    | PFalse
    | PLoc of Util.position * 'var post_con
    | PEq of 'var * 'var
    | PRel of string * 'var list
    | PAnd of 'var post_con list
    | PExists of 'var list * 'var post_con
    | PForall of 'var list * 'var post_con

let rec post_con_to_sexpr pp = function
    | PTrue -> V "T"
    | PFalse -> V "F"
    | PEq (a, b) -> S [pp a; V "="; pp b]
    | PRel (rel, args) -> S (K rel :: List.map pp args)
    | PExists (vars, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∃"; l; post_con_to_sexpr pp con]
    | PForall (vars, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∀"; l; post_con_to_sexpr pp con]
    | PAnd cons ->
      S (K "&" :: List.map (post_con_to_sexpr pp) cons)
    | PLoc (loc, c) ->
      S [K "loc"; V (Util.string_of_position loc); post_con_to_sexpr pp c]

module Make (U : Unifier_params) = struct

  include UnionFind.Make (U)

  type con =
    | CTrue
    | CFalse
    | CLoc of position * con
    | CEq of uvar * uvar
    | CAnd of con list
    | CExists of uvar list * con
    | CDef of string * uvar * con
    | CVar of string * uvar * specializer
    | CInst of uvar * scheme * specializer
    (* Définition d'une valeur polymorphique: CLet (x,a,c1,c2,gen)
     * réprésente "let x = \a.c1 in c2".  *)
    | CLet of {
        var : string;
        scheme : scheme;
        inner : con;
        outer : con;
        gen : generalizer;
        quantification_duty : uvar list
      }

  type kontext =
    | KEmpty
    | KLoc of position * kontext
    | KAnd of con list * kontext
    | KDef of string * uvar * kontext
    | KLet1 of {
        var : string;
        scheme : scheme;
        inner : kontext;
        outer : con;
        gen : generalizer;
        existentials : uvar list;
        quantification_duty : uvar list
      }
    | KLet2 of {
        var : string;
        scheme : scheme;
        existentials : uvar list;
        outer : kontext;
      }

  let eq u v = CEq (u,v)

  let cvar x u spec = CVar(x, u, spec)

  let instance u fvs v spec =  CInst (u, (fvs, v), spec)

  let ( @+ ) c d = CAnd [c;d]

  let exists xs con = CExists (xs,con)

  let rec con_to_sexpr pp = function
    | CTrue -> V "T"
    | CFalse -> V "F"
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
    | CLoc (loc, c) ->
      S [K "loc"; V (string_of_position loc); con_to_sexpr pp c]
    | CDef (x, t, c) ->
      S [K "def"; S [V x; pp t]; con_to_sexpr pp c]
    | CLet { var; scheme=(us,u); inner; outer; gen=_; quantification_duty } ->
      S [K "let";
         V var;
         S [K "∀";S (List.map pp quantification_duty); S (List.map pp us); pp u];
         con_to_sexpr pp inner;
         con_to_sexpr pp outer]

  let kontext_to_sexpr ctx =
    let pp_uvar = uvar_to_sexpr in
    let pp_con = con_to_sexpr pp_uvar in
    let rec _aux ctx acc = match ctx with
      | KEmpty -> acc
      | KLoc (loc, ctx) ->
        _aux ctx (S [K "loc"; V (string_of_position loc); acc])
      | KAnd (cons, ctx) ->
        _aux ctx (S [K "and"; block (List.map pp_con cons); acc])
      | KDef (v,u,ctx) ->
        _aux ctx (S [K "def"; V v; pp_uvar u; acc])
      | KLet1 {var;scheme=(us,u);
               inner;outer;gen=_
              ;quantification_duty;existentials} ->
        _aux inner (S ([K "letting"; V var;
                       S (List.map pp_uvar quantification_duty);
                       S (List.map pp_uvar us);
                       pp_uvar u]
                    @ (K "∃" :: List.map pp_uvar existentials)
                    @ [K "kont"; pp_con outer; acc]))
      | KLet2 { var; scheme; outer; existentials } ->
        _aux outer (S ([K "let-ed"; V var]
                       @ (K "∃" :: List.map pp_uvar existentials)
                       @ [scheme_to_sexpr pp_uvar scheme; acc])) in
    _aux ctx (V "###")

  let rec map_post f (post : 'a post_con) = match post with
    | PTrue -> PTrue
    | PFalse -> PFalse
    | PLoc (loc, c) -> PLoc (loc, map_post f c)
    | PEq (a, b) -> PEq (f a, f b)
    | PRel (r, xs) -> PRel (r, List.map f xs)
    | PAnd cs -> PAnd (List.map (map_post f) cs)
    | PExists (xs, c) -> PExists (List.map f xs, map_post f c)
    | PForall (xs, c) -> PForall (List.map f xs, map_post f c)

  let _trace stack con post =
    buffer ();
    print_endline "\n** context";
    print_sexpr (kontext_to_sexpr stack);
    print_endline "\n** constraint";
    print_sexpr (con_to_sexpr uvar_to_sexpr con);
    print_endline "\n** post";
    print_sexpr (post_con_to_sexpr uvar_to_sexpr post);
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
      | CLoc (loc, con) -> CLoc (loc, compress_cand con) :: cc
      | CDef (x,u,con) -> CDef (x,u,compress_cand con) :: cc
      | CLet lett ->
        CLet {lett with inner = compress_cand lett.inner;
                        outer = compress_cand lett.outer} :: cc
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
      | CAnd cc ->
        let fvs, cc = List.split (List.map aux cc) in
        let fvs = List.concat fvs in
        fvs, CAnd cc
      | CLoc (loc, c) ->
        let fvs, c = aux c in
        fvs, CLoc (loc, c)
      | CLet lett ->
        let (fv1,inner), (fv2,outer) = aux lett.inner, aux lett.outer in
        let inner = if fv1 = [] then inner else CExists (fv1,inner) in
        fv2, CLet {lett with inner; outer}
      | _ -> [],c in
    let fv,c = aux c in
    CExists (fv,c)

  let rec lookup_scheme stack x = match stack with
    | KEmpty -> raise
                  (Failure ("Broken invariant: Unbound var during constraint solving: " ^ x))
    | KAnd (_, ctx) -> lookup_scheme ctx x
    | KLoc (_, ctx) -> lookup_scheme ctx x
    | KDef (y,a,ctx) ->
      if x = y then
        ([], a)
      else
        lookup_scheme ctx x
    | KLet1 lett -> lookup_scheme lett.inner x
    | KLet2 lett ->
      if x = lett.var then
        refresh_scheme lett.scheme
      else lookup_scheme
          lett.outer x

  let rec lift_exist us stack = match stack with
    | KEmpty -> KEmpty
    | KAnd (cons, ctx) -> KAnd (cons, lift_exist us ctx)
    | KLoc (loc, ctx) -> KLoc (loc, lift_exist us ctx)
    | KDef (x,a,ctx) -> KDef (x,a, lift_exist us ctx)
    | KLet1 ({scheme=(vs,v);_} as lett)->
      let univs, exists =
        List.partition (fun x -> is_syntactic_sort (get_sort x)) vs in
      KLet1 {lett with
             scheme = (us @ univs, v);
             existentials = exists @ lett.existentials}
    | KLet2 lett ->
      KLet2 {lett with outer = lift_exist us lett.outer}

  exception Done

  exception Not_sufficiently_polymorphic of string

  type 'a elaboration = 'a -> con * (output_env -> 'a)

  let ( >>> ) con gen = con, gen

  let solve ?trace:(do_trace=false) (elab : 'a elaboration) (x : 'a) : 'a * 'b post_con =

    let rec entrypoint () =
      reset_unifier ();
      let con, gen = elab x in
      if do_trace then _trace KEmpty con PTrue;
      let con = compress_cand (float_cexists con) in
      let post = advance KEmpty con in
      let env = finalize_env () in
      gen env, post

   and advance stack con : uvar post_con =
      if do_trace then _trace stack con PTrue;
      match con with

      | CTrue -> backtrack stack PTrue
      | CFalse -> backtrack stack PFalse
      | CEq (a,b) ->
        backtrack stack (PAnd (List.map (fun (u,v) -> PEq (u,v)) (unify a b)))
      | CAnd [] -> backtrack stack PTrue
      | CAnd [con] -> advance stack con
      | CAnd (h::t) -> advance (KAnd (t, stack)) h
      | CExists (us, con) -> advance (lift_exist us stack) con
      | CDef (x,u,con) -> advance (KDef (x,u,stack)) con
      | CLoc (loc, con) -> advance (KLoc (loc, stack)) con

      | CVar (x,u,spec) ->
        let (vs, v) = lookup_scheme stack x in
        let stack = lift_exist vs stack in
        let eqs = unify u v in
        specialize spec vs;
        backtrack stack (PAnd (List.map (fun (u,v) -> PEq (u,v)) eqs))

      | CInst (u, (vs, v), spec) ->
        (* No need to refresh scheme here, it is single-use *)
        let stack = lift_exist vs stack in
        let eqs = unify u v in
        specialize spec vs;
        backtrack stack (PAnd (List.map (fun (u,v) -> PEq (u,v)) eqs))

      | CLet { var; scheme; inner; outer; gen; quantification_duty } ->
        enter ();
        let stack = KLet1 {var; scheme; outer; quantification_duty; gen;
                          inner = stack;
                          existentials = []} in
        advance stack inner

    and backtrack stack post : uvar post_con =
      if do_trace then _trace stack CTrue post;
      match stack with

      | KEmpty -> post
      | KAnd ([], stack) -> backtrack stack post
      | KLoc (loc, stack) -> backtrack stack (PLoc (loc, post))
      | KAnd (h::t, stack) ->
        let post' = advance (KAnd (t, stack)) h in PAnd [post; post']
      | KDef (x, u, stack) -> define x ([],u); backtrack stack post
      | KLet2 {outer;_} -> (*TODO*) backtrack outer post

      | KLet1 { var; scheme=(us,u);
                inner; outer; gen;
                quantification_duty; existentials } ->

        let tmp mess s =
          if do_trace then begin
            print_string (mess ^ " ");
            print_sexpr (scheme_to_sexpr uvar_to_sexpr s) end in

        tmp ("checking scheme") (us, u);
        (* Shorten paths we will take, normalize the variables *)
        let u = repr u in
        let us = List.map repr us in
        let us = List.fold_left insert_nodup [] us in
        (* There sould be no cycles in cells of syntactic sorts *)
        if not (occurs_check (us, u)) then begin
          print_sexpr (subst_to_sexpr !_state);
          raise (Cycle u);
        end;
        let scheme = (us, u) in
        tmp "checks passed, now lifting" scheme;
        (* lower each variable, lowest-ranked vars first *)
        Array.iteri lift_freevars (ranked_freevars_of_scheme scheme !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let scheme, old = extract_old_vars scheme !_rank in
        let xs = List.map repr (fst scheme) and ys = List.map repr quantification_duty in
        if not (have_same_elems xs ys) then
          raise (Not_sufficiently_polymorphic var);
          tmp "After lifting scheme" scheme;
        let inner' = lift_exist old inner in

        leave ();
        define var scheme;
        generalize var gen scheme;

        let existentials = List.map repr existentials in
        let post' = advance (KLet2 {var; scheme; outer=inner';existentials}) outer in
        let idx = List.filter (fun x -> not (is_syntactic_sort (get_sort x))) (fst scheme) in
        PAnd [PForall (idx, PExists (existentials, post)); post']
    in

    (* entrypoint is defined at the top of elaborate *)
    entrypoint ()

end
