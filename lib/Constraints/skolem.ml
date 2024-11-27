open Polynomials
open Constraint_common
open Format

type 'a skolem_formula =
  | SFTrue
  | SFFalse
  | SFNull of 'a list
  | SFImplies of 'a list *'a list * 'a skolem_formula
  | SFAnd of 'a list * 'a skolem_formula list

type 'a skolem_ctx =
  | SCtxEmpty
  | SCtxImplies of 'a list * 'a list * 'a skolem_ctx
  | SCtxAnd of 'a list * 'a skolem_formula list * 'a skolem_ctx * 'a skolem_formula list


let pp fmt globals opt =

  let pp_list pp fmt l = pp_print_list ~pp_sep:pp_print_space pp fmt l in
  let pp_global fmt v = fprintf fmt "(:global %a)" (Vars.TyVar.pp ~debug:true) v in
  let pp_globals fmt vs = fprintf fmt "@[<v 0>%a@]" (pp_list pp_global) vs in
  let pp_null fmt p = fprintf fmt "@[<v 1>(%a)@]" Poly.pp p in
  let pp_nulls fmt nulls= fprintf fmt "@[<v 1>(:kernel %a)@]" (pp_list pp_null) nulls in
  pp_set_geometry fmt ~max_indent:100 ~margin:200;

  let rec pp_f fmt f = match f with
  | SFTrue -> fprintf fmt ":true"
  | SFFalse -> fprintf fmt ":false"
  | SFNull nulls -> pp_nulls fmt nulls
  | SFAnd (nulls,fs) ->
     fprintf fmt "@[<v 1>(:and@ %a@ %a)@]" pp_nulls nulls (pp_list pp_f) fs
  | SFImplies (assume, witn, f) ->
     fprintf fmt "@[<v 1>(:assume %a@ :witness %a@ :then %a)@]"
       pp_nulls assume
       pp_nulls witn
       pp_f f in

  fprintf fmt "%a@.%a@." pp_globals globals pp_f opt.formula


let pp_smt fmt opt =

  let rec pp_scalar fmt (s:Scalar.t) = match Scalar.simplify s with
    | Scalar.Param v -> Vars.TyVar.pp ~debug:true fmt v
    | Scalar.Cons c -> Vars.TyConsVar.pp ~debug:true fmt c
    | Scalar.Cst n -> pp_print_int fmt n
    | Scalar.Mult (a, b) -> fprintf fmt "(* %a %a)" pp_scalar a pp_scalar b
    | Scalar.Add (a, b) -> fprintf fmt "(+ %a %a)" pp_scalar a pp_scalar b
    | Scalar.Pow (_, _) -> assert false in

  let pp_list pp fmt l = pp_print_list ~pp_sep:pp_print_space pp fmt l in
  let pp_global fmt v = fprintf fmt "(declare-const %a Int) (assert (<= 0 %a))"
                          (Vars.TyVar.pp ~debug:true) v
                          (Vars.TyVar.pp ~debug:true) v in
  let pp_one_goal fmt x = fprintf fmt "(minimize %a)" pp_scalar x in
  let pp_goal = pp_list pp_one_goal in
  let pp_null_scalar fmt (_,x) = fprintf fmt "@[(= 0 %a)@]" pp_scalar x in
  let pp_null fmt p =
    let scals = Poly.P.bindings p in
    if scals = [] then fprintf fmt "true"
    else fprintf fmt "@[<v 1>(and@ %a)@]"(pp_list pp_null_scalar) scals in
  let pp_nulls fmt ps =
    if ps = [] then fprintf fmt "true"
    else fprintf fmt "@[<v 1>(and@ %a)@]" (pp_list pp_null) ps in

  let pp_doc fmt doc = pp_print_string fmt doc in

  let rec pp_f fmt f = match f with
  | SFTrue -> fprintf fmt "true"
  | SFFalse -> fprintf fmt "false"
  | SFNull nulls -> pp_nulls fmt nulls
  | SFAnd (nulls,fs) ->
     fprintf fmt "@[<v 1>(and@ %a@ %a)@]" pp_nulls nulls (pp_list pp_f) fs
  | SFImplies ([], [], f) -> pp_f fmt f
  | SFImplies ([], witn, f) ->
     fprintf fmt "@[<v 1>(and %a@ %a)@]" pp_nulls witn pp_f f
  | SFImplies (assume, witn, f) ->
     fprintf fmt "@[<v 1>(=> %a@ @[<v 1>(and %a@ %a)@])@]"
       pp_nulls assume
       pp_nulls witn
       pp_f f in

  pp_set_geometry fmt ~max_indent:100 ~margin:200;
  fprintf fmt "%a@.@[<v 1>(assert@ %a)@]@.%a@.(echo \"%a\")@.(check-sat)@.(get-objectives)"
    (pp_list pp_global) opt.globals
    pp_f opt.formula
    (pp_print_option (pp_goal)) opt.goals
    pp_doc opt.doc




let compress opt =

  let rec advance f ctx = match f with
    | SFFalse -> lift_false ctx
    | SFTrue -> lift_nulls [] ctx
    | SFNull nulls -> lift_nulls nulls ctx
    | SFImplies ([], witn, f) -> advance (SFAnd (witn, [f])) ctx
    | SFImplies (assume, witn, f) -> advance f (SCtxImplies (assume, witn, ctx))
    | SFAnd (nulls, []) -> lift_nulls nulls ctx
    | SFAnd (nulls, x::xs) -> advance x (SCtxAnd (nulls, [], ctx, xs))

  and backtrack f ctx = match ctx with
    | SCtxEmpty -> f
    | SCtxImplies (assume, witn, ctx) -> backtrack (SFImplies (assume, witn, f)) ctx
    | SCtxAnd (ps, [], ctx, []) -> lift_nulls ps ctx
    | SCtxAnd (ps,xs,ctx,[]) -> backtrack (SFAnd (ps,f::xs)) ctx
    | SCtxAnd (ps,xs,ctx,y::ys) -> advance y ((SCtxAnd (ps,xs,ctx,ys)))

  and lift_nulls nulls ctx = match ctx with
    | SCtxEmpty -> if nulls = [] then SFTrue else SFNull nulls
    | SCtxImplies (assume, witn, ctx) ->
       backtrack (SFImplies (assume, nulls@witn, SFTrue)) ctx
    | SCtxAnd (ps, [], ctx, []) -> lift_nulls (nulls@ps) ctx
    | SCtxAnd (ps, xs, ctx, []) -> backtrack (SFAnd (ps, xs)) ctx
    | SCtxAnd (ps, xs, ctx, y::ys) -> advance y (SCtxAnd (nulls@ps, xs, ctx, ys))

  and lift_false ctx = match ctx with
    | SCtxEmpty -> SFFalse
    | SCtxAnd (_,_,ctx,_) -> lift_false ctx
    | SCtxImplies (assume, witn, ctx) -> backtrack (SFImplies (assume, witn, SFFalse)) ctx

  in

  {opt with formula = advance opt.formula SCtxEmpty}
