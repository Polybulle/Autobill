open Misc
open Format
open Prelude

include UnionFind

exception Type_error of string * position option

module Make (U : Unifier_params) = struct

  include UnionFind.Make (U)

  let existentials = ref []

  let model = ref []

  type post = formula

  type con =
    | CTrue
    | CFalse
    | CLoc of position * con
    | CEq of uvar * uvar
    | CAnd of con list
    | CCases of con list
    | CExists of uvar list * eqn list * con
    | CDef of int * string * uvar * con
    | CVar of int * uvar
    | CUniv of {
        typs : uvar list;
        inner : con;
        duty : uvar list;
        assume : eqn list;
        exists : uvar list;
        witness : eqn list
      }
    | CInferScheme of {
        var : int;
        name : string;
        typ : uvar;
        inner : con;
        outer : con;
        accumulated : uvar list;
        eqns : eqn list
      }

  type kontext =
    | KEmpty
    | KLoc of position * kontext
    | KAnd of post list * kontext * con list
    | KCases of post list * kontext * con list
    | KDef of int * string * uvar * kontext
    | KUniv of {
        typs : uvar list;
        inner : kontext;
        duty : uvar list;
        assume : eqn list;
        exists : uvar list;
        witness : eqn list;
      }
    | KInferSchemeInner of {
        var : int;
        name : string;
        typ : uvar;
        inner : kontext;
        outer : con;
        accumulated : uvar list;
        eqns : eqn list
      }
    | KInferSchemeOuter of {
        var : int;
        name : string;
        accumulated : uvar list;
        typ : uvar;
        eqns : eqn list;
        outer : kontext
      }


  let eq u v = CEq (u,v)

  let cvar x u = CVar(x, u)

  let ( @+ ) c d = CAnd [c;d]

  let exists ?st:(eqns=[]) xs con = CExists (xs, eqns, con)

  let pp_uvars fmt vars =
    fprintf fmt "@[(%a)@]" (pp_print_list ~pp_sep:pp_print_space pp_uvar) vars

  let rec pp_constraint fmt = function
    | CTrue -> pp_print_string fmt ":true"
    | CFalse -> pp_print_string fmt ":false"
    | CEq (a,b) -> fprintf fmt "(:eq-base %a %a)" pp_uvar a pp_uvar b
    | CVar (x, t) -> fprintf fmt "(:var-isa %d %a)" x pp_term t
    | CLoc (loc, c) ->
      fprintf fmt "@[<v 1>(:located \"%s\"@ %a)@]" (string_of_position loc) pp_constraint c
    | CAnd [] -> fprintf fmt ":true"
    | CAnd [c] -> pp_constraint fmt c
    | CAnd cons ->
      fprintf fmt "@[<v 1>(:and@ %a)@]" (pp_print_list ~pp_sep:pp_print_space pp_constraint) cons
    | CCases cons ->
      fprintf fmt "@[<v 1>(:cases@ %a)@]" (pp_print_list ~pp_sep:pp_print_space pp_constraint) cons
    | CExists (vars, eqns, con) ->
      fprintf fmt "@[<v 2>(:exists %a@ :witness %a@ %a@]"
        pp_uvars vars
        pp_eqns eqns
        pp_constraint con
    | CDef (x, s, t, c) ->
      fprintf fmt "@[<v 1>(:var-def %s=%d :isa %a :in@ %a)@]"
        s x pp_term t pp_constraint c
    | CInferScheme {var; name; typ; inner; outer; accumulated; eqns} ->
      fprintf fmt "@[<v 2>(:var-scheme %s=%d@ :forall %a@ :eqns %a@ :isa %d :witness %a@ :in %a)@]"
        name var
        pp_uvars accumulated
        pp_eqns eqns
        typ
        pp_constraint inner
        pp_constraint outer
    | CUniv {typs; inner; duty; assume; exists; witness} ->
      fprintf fmt "@[<v 1>(:forall %a :exists %a :assume %a :witness %a :goal %a@ :inner %a)@]"
        pp_uvars duty
        pp_vars exists
        pp_eqns assume
        pp_eqns witness
        pp_uvars typs
        pp_constraint inner


  let pp_kontext fmt ctx =

    let rec once ctx = match ctx with
      | KEmpty -> pp_print_cut fmt ()
      | KLoc (loc, ctx) ->
        fprintf fmt "--- (:located %s)@," (string_of_position loc);
        once ctx
      | KAnd (posts, ctx, rests) ->
        fprintf fmt "--- @[<v 1>(:and :done %a@ :todo %a)@]@,"
          (pp_print_list ~pp_sep:pp_print_cut pp_formula) posts
          (pp_print_list ~pp_sep:pp_print_cut pp_constraint) rests;
        once ctx
      | KCases (posts, ctx, rests) ->
        fprintf fmt "--- @[<v 1>(:cases :done %a@ :todo %a)@]@,"
          (pp_print_list ~pp_sep:pp_print_cut pp_formula) posts
          (pp_print_list ~pp_sep:pp_print_cut pp_constraint) rests;
        once ctx
      | KDef (x, s, t, ctx) ->
        fprintf fmt "--- (:var-def %s=%d :isa %a)@," s x pp_uvar t;
        once ctx
      | KUniv {typs; inner; duty; assume; exists; witness} ->
        fprintf fmt "--- @[<v 1>(:forall %a :assume %a@ :exists %a :witness %a :goal %a)@]@,"
          pp_uvars duty
          pp_eqns assume
          pp_uvars exists
          pp_eqns witness
          pp_uvars typs;
        once inner
      | KInferSchemeInner {var; name; typ; inner; outer; accumulated; eqns} ->
        fprintf fmt "--- @[<v 1>(:var-scheme-inner %s=%d@ :forall %a@ :eqns %a@ :isa %d :witness _@ :in %a)@]"
          name var
          pp_uvars accumulated
          pp_eqns eqns
          typ
          pp_constraint outer ;
        once inner
      | KInferSchemeOuter {var; name; typ; accumulated; eqns; outer} ->
        fprintf fmt "--- @[<v 1>(:var-scheme-outer %s=%d@ :forall %a@ :eqns %a@ :isa %da@]"
          name var
          pp_uvars accumulated
          pp_eqns eqns
          typ;
        once outer


    in
    pp_open_vbox fmt 0;
    once ctx;
    pp_close_box fmt ()

  let _trace stack con post =
    let fmt = std_formatter in begin
      pp_set_geometry fmt ~max_indent:180 ~margin:500;
      fprintf fmt "@.==========================================@.";
      fprintf fmt "============== NEW CYCLE =================@.";
      fprintf fmt "==========================================@.";
      fprintf fmt "@.------------context stack---------------@.";
      pp_kontext fmt stack;
      fprintf fmt "@.--------------constraint----------------@.";
      pp_constraint fmt con;
      fprintf fmt "@.------------post-constraint-------------@.";
      fprintf fmt "global vars: @[<h>%a@]@." pp_uvars !existentials;
      fprintf fmt "global eqns: %a@." pp_eqns !model;
      pp_formula fmt post;
      fprintf fmt "@.-------------type of variables-------------@.";
      let pp_bind fmt (x,(us,eqns,u)) =
        fprintf fmt "(:var-scheme %d :forall %a :where %a :isa %d)@."
          x
          pp_uvars us
          pp_eqns eqns
          u in
      pp_print_list pp_bind fmt !_nvar_env;
      fprintf fmt "@.-------------substitution-------------@.";
      fprintf fmt "%a@." pp_subst !_state;
    end

  let compress_and =

    let rec go = function
      | (CTrue | CFalse | CEq _ | CVar _) as x -> x
      | CLoc (loc, c) -> CLoc (loc, go c)
      | CAnd cs -> begin match collect_and [] cs with
          | [] -> CTrue
          | [c] -> c
          | cs -> CAnd cs
        end
      | CCases cs -> begin match collect_cases [] cs with
          | [] -> CTrue
          | [c] -> c
          | cs -> CCases cs
        end
      | CExists (vars, eqns, c) -> CExists (vars, eqns, go c)
      | CDef (v, name, u, c) -> CDef (v, name, u, go c)
      | CUniv univ -> CUniv {univ with inner = go univ.inner}
      | CInferScheme infer ->
        CInferScheme {infer with inner = go infer.inner; outer = go infer.outer}

    and collect_and acc =  function
      | [] -> acc
      | (CAnd cs)::cs' -> collect_and (collect_and acc cs) cs'
      | c::cs -> collect_and (c::acc) cs

    and collect_cases acc = function
      | [] -> acc
      | (CCases cs)::cs' -> collect_cases (collect_cases acc cs) cs'
      | c::cs -> collect_cases (c::acc) cs

  in go

  let rec lookup_scheme stack x = match stack with
    | KEmpty ->
     fail_invariant_break ("Unbound var during constraint solving: v" ^ string_of_int x)
    | KAnd (_, ctx, _)
    | KCases (_, ctx, _)
    | KLoc (_, ctx)
    | KUniv { inner = ctx;_} -> lookup_scheme ctx x
    | KDef (y,_,typ,ctx)
    | KInferSchemeInner {var = y; typ; inner = ctx; _}
         -> if x = y then ([], typ, []) else lookup_scheme ctx x
    | KInferSchemeOuter {var = y; accumulated; typ; eqns; outer = ctx; _}
        -> if x = y then refresh_scheme (accumulated, typ, eqns) else lookup_scheme ctx x


  let check_freevars_in_stack eqns stack =
    let fvs = freevars_of_eqns (fun x -> UFOL.S.singleton x) eqns in
    let rec go fvs = function
      | KEmpty -> UFOL.S.elements fvs
      | KAnd (_, ctx, _)
      | KCases (_, ctx, _)
      | KLoc (_, ctx)
      | KDef (_, _, _, ctx)
      | KInferSchemeOuter {outer = ctx; _}
        -> go fvs ctx
      | KUniv {duty; exists=e; inner = ctx; _}
        -> go UFOL.S.(diff fvs (union (of_list duty) (of_list e))) ctx
      | KInferSchemeInner {accumulated; inner = ctx; _}
        -> go (UFOL.S.diff fvs (UFOL.S.of_list accumulated)) ctx in
    let fvs = go fvs stack in
      if fvs <> [] then begin
      let mess = Format.(
            fprintf str_formatter "variables out of bound in eqns %a@." pp_eqns eqns;
            flush_str_formatter ()) in
      Misc.fail_invariant_break mess
    end

  let lift_exist us ?st:(eqns=[]) stack =
    let rec go stack = match stack with
      | KEmpty -> begin
          let idx = List.filter (fun x -> not (is_syntactic_sort (get_sort x))) us in 
          existentials := idx @ !existentials;
          model := eqns @ !model;
          KEmpty
        end
      | KAnd (cons, ctx, post) -> KAnd (cons, go ctx, post)
      | KCases (cons, ctx, post) -> KCases (cons, go ctx, post)
      | KLoc (loc, ctx) -> KLoc (loc, go ctx)
      | KDef (x,s,a,ctx) -> KDef (x,s,a,go ctx)
      | KUniv univ ->
        KUniv { univ with
                   exists = us @ univ.exists;
                   witness = eqns @ univ.witness;
                 }
      | KInferSchemeInner infer ->
        KInferSchemeInner {infer with
                           accumulated = us @ infer.accumulated;
                           eqns = eqns @ infer.eqns
                          }
      | KInferSchemeOuter infer ->
        KInferSchemeOuter {infer with outer = go infer.outer}

    in
    go stack

  let rec fail_unification info ctx u v =
    let mess = Printf.sprintf "unification failed between %d and %d: %s" u v info in
    match ctx with
    | KLoc (loc, _) -> raise (Type_error (mess, Some loc))
    | KEmpty -> raise (Type_error (mess, None))
    | KAnd (_, ctx, _) | KDef (_, _, _, ctx) | KCases (_, ctx, _)
    | KInferSchemeInner {inner=ctx;_} | KInferSchemeOuter {outer=ctx;_}
    | KUniv {inner=ctx;_} ->
      fail_unification info ctx u v

  let unify_or_fail ctx u v =
    try unify u v with
    | UnifySort (u,v) -> fail_unification "different number of arguments" ctx u v
    | Unify (u,v,uk,vk) ->
      let open Format in
      fprintf str_formatter "different constructors %a and %a" pp_node uk pp_node vk;
      let mess = flush_str_formatter () in
      fail_unification mess ctx u v

  type 'a elaboration = 'a -> con * (output_env -> 'a)

  let ( >>> ) con gen = con, gen


  type _binder = Bind_Univ | Bind_Exists

  let lift_idx_freevars post =

    let open UFOL in

    let rec advance r p ctx = match p with

      | PTrue | PFalse | PEqn _ -> backtrack r p ctx
      | PLoc (loc, p) -> advance r p (KLoc (loc, ctx))
      | PAnd ps -> advance r PTrue (KAnd ([], ctx, ps))
      | PCases ps -> advance r PTrue (KCases ([], ctx, ps))
      | PExists (xs, ys, eqns, p) ->
        let ctx = KExists ([], ys, eqns, ctx) in
        let ctx = List.fold_left (lift Bind_Exists (r+1)) ctx xs in
        advance (r+1) p ctx
      | PForall (xs, ys, eqns, eqns', p) ->
        let ctx = KForall ([], [], eqns, eqns', ctx) in
        let ctx = List.fold_left (lift Bind_Univ (r+1)) ctx xs in
        let ctx = List.fold_left (lift Bind_Exists (r+1)) ctx ys in
        advance (r+1) p ctx

    and backtrack r p ctx = match ctx with
      | KEmpty ->
        if r > 0 then
          fail_invariant_break "During typechecking, ranking is out of sync with constraint"
        else
          p
      | KLoc (loc ,ctx) -> backtrack r (PLoc (loc, p)) ctx
      | KAnd (dones, ctx, []) -> backtrack r (PAnd (p::dones)) ctx
      | KCases (dones, ctx, []) -> backtrack r (PCases (p::dones)) ctx
      | KAnd (dones, ctx, p'::todos) -> advance r p' (KAnd (p::dones, ctx, todos))
      | KCases (dones, ctx, p'::todos) -> advance r p' (KCases (p::dones, ctx, todos))
      | KForall (xs, ys, eqns, eqns', ctx) ->
        backtrack (r-1) (PForall (xs, ys, eqns, eqns', p)) ctx
      | KExists (xs, ys, eqns, ctx) ->
        backtrack (r-1) (PExists (xs, ys, eqns, p)) ctx

    and lift bind r ctx v =
      let r_goal = rank v in
      let rec go r ctx = match ctx with
        | KEmpty ->
          if r_goal = 0 then
            KExists ([v],[],[],KEmpty)
          else
            Misc.fail_invariant_break "Failed to lift type variable when creating logical constriant"
        | KLoc (loc, ctx) -> KLoc (loc, go r ctx)
        | KAnd (dones, ctx, todos) -> KAnd (dones, go r ctx, todos)
        | KCases (dones, ctx, todos) -> KCases (dones, go r ctx, todos)
        | KForall (xs, ys, eqns, eqns', ctx) ->
          if r <= r_goal then
            match bind with
            | Bind_Univ -> KForall (v :: xs, ys, eqns, eqns', ctx)
            | Bind_Exists -> KForall (xs, v::ys, eqns, eqns', ctx)
          else
            KForall (xs, ys, eqns, eqns', go (r-1) ctx)
        | KExists (xs, ys, eqns, ctx) ->
          if r <= r_goal then
            KExists (v :: xs, ys, eqns, ctx)
          else
            KExists (xs, ys, eqns, go (r-1) ctx) in
      go r ctx in
    advance 0 post UFOL.KEmpty


  let finalize_post_con env c =
    let finalize_uvar u = env.get u in
    let finalize_term u = env.u u in
    let finalize_eqn eq =
      let res = match eq with
        | Eq (a, b, so) -> Eq (finalize_term a, finalize_term b, so)
        | Rel (rel, args) -> Rel (rel, List.map finalize_term args) in
      res in
    let finalize_eqns = List.map finalize_eqn in
    let c = lift_idx_freevars c in
    let c = PExists (!existentials, [], !model, c) in
    let model_id x = Eq (deep_of_var (finalize_uvar x), finalize_term x , get_sort x) in

    let rec go = function
      | UFOL.PTrue -> FFOL.PTrue
      | PFalse -> PFalse
      | PLoc (loc, c) -> PLoc (loc, go c)
      | PEqn eqns -> PEqn (finalize_eqns eqns)
      | PAnd cs -> PAnd (List.map go cs)
      | PCases cs -> PCases (List.map go cs)
      | PExists (vars, duty, eqns, c) ->
        PExists (List.map finalize_uvar vars,
                 List.map finalize_uvar duty,
                 List.map model_id vars @ finalize_eqns eqns,
                 go c)
      | PForall (vars, duty, eqns, eqns', c) ->
        PForall (List.map finalize_uvar vars,
                 List.map finalize_uvar duty,
                 List.map model_id vars @ finalize_eqns eqns,
                 List.map model_id duty @ finalize_eqns eqns',
                 go c) in
    go c


  let solve ?trace:(do_trace=false) (elab : 'a elaboration) (x : 'a) =

    let rec entrypoint () =
      reset_unifier ();
      let con, gen = elab x in
      let con = compress_and con in
      if do_trace then _trace KEmpty con PTrue;
      let post = advance KEmpty con in
      let env = finalize_env () in
      let post = finalize_post_con env post in
      gen env, post

    and advance stack con =
      if do_trace then _trace stack con PTrue;
      match con with

      | CTrue -> backtrack stack PTrue
      | CFalse -> backtrack stack PFalse
      | CEq (a,b) ->
        let eqns = (List.map (fun (u,v) -> Eq (u,v, get_sort u)) (unify_or_fail stack a b)) in
        backtrack (lift_exist [] ~st:eqns stack) PTrue
      | CAnd [] -> backtrack stack PTrue
      | CAnd [con] -> advance stack con
      | CAnd (h::t) -> advance (KAnd ([], stack, t)) h
      | CCases [] -> backtrack stack PTrue
      | CCases [con] -> advance stack con
      | CCases (h::t) -> advance (KCases ([], stack, t)) h
      | CDef (x,s,u,con) -> advance (KDef (x,s,u,stack)) con
      | CLoc (loc, con) -> advance (KLoc (loc, stack)) con

      | CExists (us, eqns, con) -> advance (lift_exist us ~st:eqns stack) con

      | CVar (x,u) ->
        let (vs, v, eqns) = lookup_scheme stack x in
        let stack = lift_exist vs ~st:eqns stack in
        let eqs = unify_or_fail stack u v in
        backtrack stack (PEqn (List.map (fun (u,v) -> Eq (u,v,get_sort u)) eqs))

      | CUniv {duty; assume; exists; witness; inner; typs} ->
        enter ();
        advance (KUniv {duty; assume; exists; witness; typs; inner = stack}) inner
      | CInferScheme {var; name; accumulated; eqns; typ; inner; outer} ->
        enter ();
        advance (KInferSchemeInner {var; name; accumulated; eqns; typ; outer; inner = stack}) inner


    and backtrack stack post =
      if do_trace then _trace stack CTrue post;
      match stack with

      | KEmpty -> post
      | KAnd (posts, stack, []) -> backtrack stack (PAnd (post :: posts))
      | KCases (posts, stack, []) -> backtrack stack (PCases (post :: posts))
      | KLoc (loc, stack) -> backtrack stack (PLoc (loc, post))
      | KAnd (posts, stack, h::t) -> advance (KAnd (post :: posts, stack, t)) h
      | KCases (posts, stack, h::t) -> advance (KCases (post :: posts, stack, t)) h
      | KDef (x, _, u, stack) -> define x ([],[],u); backtrack stack post
      | KInferSchemeOuter {var; accumulated; eqns; typ; outer; _} ->
        define var (accumulated, eqns, typ); backtrack outer post

      | KUniv {duty; assume; exists; witness; inner; typs} ->
        if do_trace then begin
          Format.print_newline ();
          Format.printf "checking scheme: forall (%a). (%a) => exists (%a). (%a) & (%a)\n"
              pp_uvars duty
              pp_eqns assume
              pp_uvars exists
              pp_eqns witness
              pp_uvars typs
          end;

        (* Shorten paths we will take, normalize the variables *)
        let duty_typs, duty_idx =
          List.partition (fun v -> is_syntactic_sort (get_sort v)) duty in
        let exists_typs, exists_idx =
          List.partition (fun v -> is_syntactic_sort (get_sort v)) exists in
        let normalize_vars v =
          let v = List.map repr v in
          List.fold_left insert_nodup [] v in
        let typs = normalize_vars typs in
        let duty_typs, duty_idx = normalize_vars duty_typs, normalize_vars duty_idx in
        let exists_typs, exists_idx = normalize_vars exists_typs, normalize_vars exists_idx in

        (* lower each variable free type-level variable, lowest-ranked vars first *)
        let fvs_typs = List.concat
            ([duty_typs; exists_typs] @ List.map freevars_of_type typs) in
        Array.iteri lift_freevars (ranked_freevars fvs_typs !_rank);
        (* We now know which vars can be lifted in the surronding scope! *)
        let fvs_typs, old = extract_old_vars fvs_typs !_rank in
        let inner = lift_exist old inner in
        leave ();

        (* After ensuring we don't have leftovers, pass it all to the post-constraints *)
        assert (List.for_all (fun x -> List.mem x fvs_typs) duty_typs);

        backtrack inner (PForall (duty_idx, exists_idx, assume, witness, post))

      | KInferSchemeInner _ ->
        assert false (* TODO *)
    in

    (* entrypoint is defined at the top of elaborate *)
    entrypoint ()

end
