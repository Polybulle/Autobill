open Sexpr
open Misc
open FirstOrder

include UnionFind

exception Invariant_break of string

module Make (U : Unifier_params) = struct

  include UnionFind.Make (U)

  type 'a post_con = (sort, rel, 'a, 'a) formula

  let existentials = ref []

  let model = ref []

  type con =
    | CTrue
    | CFalse
    | CLoc of position * con
    | CEq of uvar * uvar
    | CAnd of con list
    | CExists of uvar list * con
    | CGuardedExists of uvar list * (sort, rel, uvar) eqn list * con
    | CDef of string * uvar * con
    | CVar of string * uvar * specializer
    (* TODO combien d'équations ? *)
    | CGoal of {
        var : string;
        typ : uvar;
        inner : con;
        outer : con;
        gen : generalizer;
        quantification_duty : uvar list;
        accumulated : uvar list;
        univ_eqns : (sort, rel, uvar) eqn list;
        existentials : uvar list;
        exist_eqns : (sort, rel, uvar) eqn list;
      }

  type kontext =
    | KEmpty
    | KLoc of position * kontext
    | KAnd of con list * kontext
    | KDef of string * uvar * kontext
    | KLet1 of {
        var : string;
        typ : uvar;
        inner : kontext;
        outer : con;
        gen : generalizer;
        quantification_duty : uvar list;
        accumulated : uvar list;
        univ_eqns : (sort, rel, uvar) eqn list;
        existentials : uvar list;
        exist_eqns : (sort, rel, uvar) eqn list;
      }
    | KLet2 of {
        var : string;
        typ : uvar;
        outer : kontext;
        quantified : uvar list;
        eqns : (sort, rel, uvar) eqn list;
      }

  let eq u v = CEq (u,v)

  let cvar x u spec = CVar(x, u, spec)

  let ( @+ ) c d = CAnd [c;d]

  let exists ?st:(eqns) xs con =
    match eqns with
    | None -> CExists (xs,con)
    | Some eqns -> CGuardedExists (xs, eqns, con)

  let equations_to_sexpr pp eqns =
    let aux = function
      | Eq (a,b,_) -> S [K "="; pp a; pp b]
      | Rel (rel, args) ->
        S (K (string_of_rel rel) :: List.map pp args) in
    S (List.map aux eqns)

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
    | CGuardedExists (vars, eqns, con) ->
      let l = match vars with
        | [v] -> pp v
        | _ -> S (List.map pp vars) in
      S [K "∃"; l; equations_to_sexpr pp eqns; con_to_sexpr pp con]
    | CVar (x, t, _) ->
      S [V x; V ":"; pp t]
    | CLoc (loc, c) ->
      S [K "loc"; V (string_of_position loc); con_to_sexpr pp c]
    | CDef (x, t, c) ->
      S [K "def"; S [V x; pp t]; con_to_sexpr pp c]
    | CGoal { var; typ; accumulated;
              inner; outer; quantification_duty;
             univ_eqns; exist_eqns; _} ->
      S [K "let";
         V var;
         S [K "∀";S (List.map pp quantification_duty);
            equations_to_sexpr pp univ_eqns;
            K "=>";
            K "∃";S (List.map pp accumulated);
            equations_to_sexpr pp exist_eqns;
            K "&";
            pp typ];
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
      | KLet1 {var;typ;accumulated;gen=_;
               inner;outer;
               quantification_duty; existentials;
               exist_eqns; univ_eqns} ->
        _aux inner (S ([K "letting"; V var;
                        S (List.map pp_uvar quantification_duty);
                        S (List.map pp_uvar accumulated);
                        eqns_to_sexpr string_of_rel pp_uvar univ_eqns;
                        K "=>";
                        K "∃";
                        eqns_to_sexpr string_of_rel pp_uvar exist_eqns;
                        pp_uvar typ]
                       @ (K "∃" :: List.map pp_uvar existentials)
                       @ [K "kont"; pp_con outer; acc]))
      | KLet2 { var; typ; outer; eqns; quantified} ->
        _aux outer (S ([K "let-ed"; V var;
                        K "∀";
                        S (List.map pp_uvar quantified);
                        eqns_to_sexpr string_of_rel pp_uvar eqns;
                        pp_uvar typ;
                        acc])) in
    _aux ctx (V "###")

  let post_con_to_sexpr = FirstOrder.formula_to_sexpr
  let rel_to_sexpr rel = (string_of_rel rel)

  let _trace stack con post =
    buffer ();
    print_endline "\n** context";
    print_sexpr (kontext_to_sexpr stack);
    print_endline "\n** constraint";
    print_sexpr (con_to_sexpr uvar_to_sexpr con);
    print_endline "\n** post";
    print_sexpr (post_con_to_sexpr rel_to_sexpr uvar_to_sexpr uvar_to_sexpr post);
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
      | CGoal lett ->
        CGoal {lett with inner = compress_cand lett.inner;
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
      | CGoal lett ->
        let (fv1,inner), (fv2,outer) = aux lett.inner, aux lett.outer in
        let inner = if fv1 = [] then inner else CExists (fv1,inner) in
        fv2, CGoal {lett with inner; outer}
      | _ -> [],c in
    let fv,c = aux c in
    CExists (fv,c)

  let rec lookup_scheme stack x = match stack with
    | KEmpty -> raise
                  (Failure ("Broken invariant: Unbound var during constraint solving: " ^ x))
    | KAnd (_, ctx) -> lookup_scheme ctx x
    | KLoc (_, ctx) -> lookup_scheme ctx x
    | KDef (y,a,ctx) -> if x = y then ([], a, []) else lookup_scheme ctx x
    | KLet1 lett -> lookup_scheme lett.inner x
    | KLet2 lett ->
      if x = lett.var then
        refresh_scheme (lett.quantified, lett.typ, lett.eqns)
      else lookup_scheme
          lett.outer x

  let lift_exist us ?st:(eqns=[]) stack =
    let us, idx = List.partition (fun x -> is_syntactic_sort (get_sort x)) us in
    let rec go stack = match stack with
      | KEmpty -> begin
          existentials := idx @ !existentials;
          model := eqns @ !model;
          KEmpty
        end
      | KAnd (cons, ctx) -> KAnd (cons, go ctx)
      | KLoc (loc, ctx) -> KLoc (loc, go ctx)
      | KDef (x,a,ctx) -> KDef (x,a, go ctx)
      | KLet1 lett->
        KLet1 {lett with
               accumulated = us @ lett.accumulated;
               existentials = idx @ lett.existentials;
               exist_eqns = eqns @ lett.exist_eqns}
      | KLet2 lett -> KLet2 {lett with outer = go lett.outer} in
    go stack

  exception Done

  exception Not_sufficiently_polymorphic of string

  type 'a elaboration = 'a -> con * (output_env -> 'a)

  let ( >>> ) con gen = con, gen



  let finalize_post_con env (c : uvar post_con) =
    let model x = Eq (deep_of_var (env.get x), env.u x, get_sort x) in
    let finalize_eqns = List.map (function
      | Eq (a, b, so) -> Eq (env.u a, env.u b, so)
      | Rel (rel, args) -> Rel (rel, List.map env.u args)) in
    let rec go = function
      | PTrue -> PTrue
      | PFalse -> PFalse
      | PLoc (loc, c) -> PLoc (loc, go c)
      | PEqn eqns -> PEqn (finalize_eqns eqns)
      | PAnd cs -> PAnd (List.map go cs)
      | PExists (vars, [], c) ->
        PExists (List.map env.get vars, List.map model vars, go c)
      | PForall (vars, [], c) ->
        PForall (List.map env.get vars, List.map model vars, go c)
      | _ -> raise (Invalid_argument "") in
    go c

  let solve ?trace:(do_trace=false) (elab : 'a elaboration) (x : 'a) =

    let rec entrypoint () =
      reset_unifier ();
      let con, gen = elab x in
      if do_trace then _trace KEmpty con PTrue;
      let con = compress_cand (float_cexists con) in
      let post = advance KEmpty con in
      let env = finalize_env () in
      let post = finalize_post_con env post in
      gen env, post

   and advance stack con : uvar post_con =
      if do_trace then _trace stack con PTrue;
      match con with

      | CTrue -> backtrack stack PTrue
      | CFalse -> backtrack stack PFalse
      | CEq (a,b) ->
        backtrack stack (PEqn (List.map (fun (u,v) -> Eq (u,v, get_sort u)) (unify a b)))
      | CAnd [] -> backtrack stack PTrue
      | CAnd [con] -> advance stack con
      | CAnd (h::t) -> advance (KAnd (t, stack)) h
      | CDef (x,u,con) -> advance (KDef (x,u,stack)) con
      | CLoc (loc, con) -> advance (KLoc (loc, stack)) con

      | CExists (us, con) -> advance (lift_exist us stack) con
      | CGuardedExists (us, eqns, con) -> advance (lift_exist us ~st:eqns stack) con

      | CVar (x,u,spec) ->
        let (vs, v, eqns) = lookup_scheme stack x in
        let stack = lift_exist vs ~st:eqns stack in
        let eqs = unify u v in
        specialize spec vs;
        backtrack stack (PEqn (List.map (fun (u,v) -> Eq (u,v,get_sort u)) eqs))

      | CGoal { var; typ; accumulated;
                inner; outer; gen;
                quantification_duty; existentials;
                exist_eqns; univ_eqns} ->
        enter ();
        let stack = KLet1 {var; typ; outer; quantification_duty; gen;
                          inner = stack; existentials; accumulated;
                           univ_eqns; exist_eqns} in
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
      | KLet2 {outer;_} -> backtrack outer post

      | KLet1 { var; typ; inner; outer; gen;
                quantification_duty; existentials;
                exist_eqns; univ_eqns; accumulated} ->

        let tmp mess s =
          if do_trace then begin
            print_string (mess ^ " ");
            print_sexpr (scheme_to_sexpr uvar_to_sexpr s) end in

        tmp ("checking scheme") (accumulated, typ);
        (* Shorten paths we will take, normalize the variables *)
        let typ = repr typ in
        let accumulated = List.map repr accumulated in
        let accumulated = List.fold_left insert_nodup [] accumulated in
        (* There sould be no cycles in cells of syntactic sorts *)
        if not (occurs_check (accumulated, typ)) then begin
          print_sexpr (subst_to_sexpr !_state);
          raise (Cycle typ);
        end;
        let scheme = (accumulated, typ) in
        tmp "checks passed, now lifting" scheme;
        (* lower each variable, lowest-ranked vars first *)
        Array.iteri lift_freevars (ranked_freevars_of_scheme scheme !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let scheme, old = extract_old_vars scheme !_rank in
        let xs = List.map repr accumulated and ys = List.map repr quantification_duty in
        if not (is_sublist ys xs) then
          raise (Not_sufficiently_polymorphic var);
          tmp "After lifting scheme" scheme;
        let inner' = lift_exist old inner in

        leave ();
        define var scheme;
        generalize var gen scheme;

        let existentials = List.map repr existentials in
        let ctx = KLet2 {var; typ; quantified = accumulated;
                         outer=inner'; eqns = univ_eqns} in
        let post' = advance ctx outer in
        let idx = List.filter (fun x -> not (is_syntactic_sort (get_sort x))) (fst scheme) in
        PAnd [PForall (idx, univ_eqns, PExists (existentials, exist_eqns, post)); post']
    in

    (* entrypoint is defined at the top of elaborate *)
    entrypoint ()

end
