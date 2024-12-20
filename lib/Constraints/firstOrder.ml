open Misc
open Format
open Constraint_common
open Prelude

module type FOL_Params = sig
  type sort
  type rel
  type var
  type term

  val pp_sort : formatter -> sort -> unit
  val pp_term : formatter -> term -> unit
  val pp_var : formatter -> var -> unit
  val pp_rel : formatter -> rel -> unit

end

module FOL (P : FOL_Params) = struct

  type eqn = (P.term, P.rel) _eqn

  include P
  type formula =
    | PTrue
    | PFalse
    | PLoc of position * formula
    | PEqn of eqn list
    | PAnd of formula list
    | PCases of formula list
    | PExists of var list * var list * eqn list * formula
    | PForall of var list * var list * eqn list * eqn list * formula

  type ctx =
    | KEmpty
    | KLoc of position * ctx
    | KAnd of formula list * ctx * formula list
    | KCases of formula list * ctx * formula list
    | KForall of var list * var list * eqn list * eqn list * ctx
    | KExists of var list * var list * eqn list * ctx

  let pp_list pp fmt l =
    fprintf fmt "%a" (pp_print_list ~pp_sep:pp_print_space pp) l

  let pp_eqn fmt = function
    | Eq (a,b,_) ->
      fprintf fmt "(:eq-idx %a %a)" pp_term a pp_term b
    | Rel (rel,args) -> fprintf fmt "@[<hov 1>(:rel \"%a\"@ %a)@]" pp_rel rel (pp_list pp_term) args

  let pp_eqns fmt eqns =
    fprintf fmt "@[<v 1>(%a)@]" (pp_list pp_eqn) eqns

  let pp_vars fmt vars = fprintf fmt "@[(%a)@]" (pp_list pp_var) vars

  let pp_formula ?with_loc:(with_loc=true) fmt f =
    let rec pp fmt f = match f with
      | PTrue -> fprintf fmt ":true"
      | PFalse -> fprintf fmt ":false"
      | PLoc (loc, f) ->
        if with_loc then
          fprintf fmt "@[<hv 1>(:loc \"%s\"@ %a)@]"
            (string_of_position ~with_filename:false loc) pp f
        else
          pp fmt f
      | PEqn eqns -> fprintf fmt "(:model %a)" pp_eqns eqns
      | PAnd fs -> fprintf fmt "@[<v 1>(:and@ %a)@]" (pp_list pp) fs
      | PCases fs -> fprintf fmt "@[<v 1>(:cases@ %a)@]" (pp_list pp) fs
      | PExists (vars, duty, eqns, rest) ->
        fprintf fmt "@[<v 1>(:exists %a@ :witness %a@ :with %a@ :then %a)@]"
          pp_vars vars
          pp_vars duty
          pp_eqns eqns
          pp rest
      | PForall (duty, exists, assume, witness, rest) ->
        fprintf fmt "@[<v 1>(:forall %a@ :exists %a@ :assume %a@ :witness %a@ :then %a)@]"
          pp_vars duty
          pp_vars exists
          pp_eqns assume
          pp_eqns witness
          pp rest

    in pp fmt f

  let string_of_formula ?with_loc:(with_loc=true) f =
    pp_formula ~with_loc str_formatter f; flush_str_formatter ()

  module S : Set.S with type elt = var =
    Set.Make (struct type t = var let compare = compare end)

  let freevars_of_eqn fv_term : eqn -> S.t = function
    | Eq (a,b,_) -> S.union (fv_term a) (fv_term b)
    | Rel (_, args) -> List.fold_left (fun vs t -> S.union vs (fv_term t)) S.empty args

  let freevars_of_eqns fv_term eqns =
    List.fold_left (fun vs eqn -> S.union vs (freevars_of_eqn fv_term eqn)) S.empty eqns

  let rec freevars_of_formula fv_term = function
    | PTrue | PFalse -> S.empty
    | PLoc (_, c) -> freevars_of_formula fv_term c
    | PEqn eqns -> freevars_of_eqns fv_term eqns
    | PAnd cs ->
      List.fold_left (fun vs c -> S.union vs (freevars_of_formula fv_term c)) S.empty cs
    | PCases cs ->
      List.fold_left (fun vs c -> S.union vs (freevars_of_formula fv_term c)) S.empty cs
    | PExists (xs,ys,eqns,c) ->
      let fvs = S.union (freevars_of_eqns fv_term eqns) (freevars_of_formula fv_term c) in
      S.diff fvs (S.union (S.of_list xs) (S.of_list ys))
    | PForall (xs,ys,eqns, eqns',c) ->
      let fvs =
        S.union (freevars_of_eqns fv_term eqns)
          (S.union (freevars_of_eqns fv_term eqns')
             (freevars_of_formula fv_term c)) in
      S.diff fvs (S.union (S.of_list xs) (S.of_list ys))

type compress_quantifiers_t =
  | Univ of var list * var list * eqn list * eqn list
  | Exist of var list * var list * eqn list
  | Neutral of eqn list

let rec compress_logic ?(remove_loc = true) opt =

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
    | PTrue | PEqn [] -> backtrack PTrue ctx
    | PFalse -> backtrack PFalse ctx
    | PEqn eqns ->
      let ctx = lift_quant (Neutral (compress_eqns eqns)) ctx in
      backtrack PTrue ctx
    | PLoc (loc, c) ->
      if remove_loc then advance c ctx else advance c (lift_loc loc ctx)
    | PAnd xs -> compress_and xs ctx
    | PCases cs -> advance PTrue (KCases ([], ctx, cs))
    | PExists ([], [], [], c) -> kill (); advance c ctx
    | PExists ([], [], eqns, c) -> kill (); advance (PAnd [PEqn (eqns); c]) ctx
    | PExists (xs, ys, eqns, c) ->
      advance c (lift_quant (Exist (xs, ys, compress_eqns eqns)) ctx)
    | PForall ([], [], [], [], x) -> kill (); advance x ctx
    | PForall (duty, exists, assume, witness, c) ->
      advance c (lift_quant (Univ (duty, exists, compress_eqns assume, compress_eqns witness)) ctx)

  and backtrack c ctx = match ctx with
    | KEmpty -> c
    | KLoc (loc, ctx) -> backtrack (PLoc (loc, c)) ctx
    | KAnd ([], ctx, []) -> kill (); backtrack c ctx
    | KAnd (xs, ctx, []) ->
      let xs = if c = PTrue then xs else c :: xs in
      backtrack (PAnd xs) ctx
    | KAnd (xs, ctx, y::ys) ->
      let xs = if c = PTrue then xs else c :: xs in
      advance y (KAnd (xs, ctx, ys))
    | KCases ([], ctx, []) -> kill (); backtrack c ctx
    | KCases (xs, ctx, []) ->
      let xs = if c = PTrue then xs else c :: xs in
      backtrack (PCases xs) ctx
    | KCases (xs, ctx, y::ys) ->
      let xs = if c = PTrue then xs else c :: xs in
      advance y (KCases (xs, ctx, ys))
    | KForall (xs, ys, eqns, eqns', ctx) ->
      let xs = List.fold_left insert_nodup [] xs in
      let ys = List.fold_left insert_nodup [] ys in
      backtrack (PForall (xs, ys, eqns, eqns', c)) ctx
    | KExists (xs, ys, eqns, ctx) ->
      let xs = List.fold_left insert_nodup [] xs in
      let ys = List.fold_left insert_nodup [] ys in
      backtrack (PExists (xs, ys, eqns, c)) ctx

  and compress_and cs ctx =
    let rec go acc eqns cs = match cs with
      | [] -> eqns, acc
      | PTrue :: cs -> go acc eqns cs
      | PLoc (_,c) :: cs when remove_loc -> go acc eqns (c :: cs)
      | PAnd (cs') :: cs -> kill (); go acc eqns (cs' @ cs)
      | PEqn eqns' :: cs -> go acc (eqns' @ eqns) cs
      | c :: cs -> go (c::acc) eqns cs in
    let eqns, cs = go [] [] cs in
    match eqns, cs with
    | [], [] -> backtrack PTrue ctx
    | [], cs -> advance PTrue (KAnd ([], ctx, cs))
    | eqns, [] -> advance (PEqn eqns) ctx
    | eqns, cs -> advance PTrue (KAnd ([], ctx, PEqn eqns :: cs))

  and lift_loc loc = function
    | KLoc (_, ctx) -> kill (); KLoc (loc, ctx)
    | ctx -> KLoc (loc, ctx)

  and lift_quant vs ctx =
    if vs = Univ ([], [], [], []) || vs = Exist ([], [], []) || vs = Neutral [] then
      ctx
    else match vs,ctx with
      | Exist (xs, ys, eqns), KExists (xs', ys', eqns', ctx') ->
        let xs = List.fold_left Misc.insert_nodup xs xs' in
        let ys = List.fold_left Misc.insert_nodup ys ys' in
        kill (); KExists (xs, ys, eqns@eqns', ctx')
      | Univ (us, vs, eqns, witn), KForall (us', vs', eqns', witn', ctx') ->
        let us = List.fold_left Misc.insert_nodup us us' in
        let vs = List.fold_left Misc.insert_nodup vs vs' in
        kill (); KForall (us, vs, eqns@eqns', witn@witn', ctx')

      | Exist (xs, ys, eqns), _ -> KExists (xs, ys, eqns, ctx)
      | Univ (us, vs, eqns, eqns'), _ -> KForall (us, vs, eqns, eqns', ctx)
      | Neutral eqns, KExists (us, vs, eqns', ctx) -> KExists (us, vs, eqns @ eqns', ctx)
      | Neutral eqns, KForall (us, vs, eqns', witn, ctx)
        -> KForall (us, vs, eqns',  witn @ eqns, ctx)
      | Neutral _, KLoc (loc, ctx) -> KLoc (loc, lift_quant vs ctx)
      | Neutral _, KAnd (cs, ctx, cs') -> KAnd (cs, lift_quant vs ctx, cs')
      | Neutral eqns, KEmpty -> KAnd ([PEqn eqns], KEmpty, [])
      | Neutral _, KCases _ ->
        Misc.fail_invariant_break
          "A logical constraint contains a case disjunction without the required quantifiers"
  in
  let c = opt.formula in
  let c = advance c KEmpty in
  if not !canary then
    compress_logic ~remove_loc {opt with formula = c}
  else
    let c = advance c KEmpty in
    {opt with formula = c}


end

module FullFOL =  struct

  open Vars
  open Types

  include FOL(struct
      type var = TyVar.t
      type sort = SortVar.t Types.sort
      type rel = RelVar.t
      type term = typ

      let pp_var = TyVar.pp ~debug:true
      let pp_rel = RelVar.pp ~debug:true
      let pp_sort = pp_sort SortVar.to_string
      let pp_term = pp_typ TyConsVar.pp TyVar.pp
    end)

  let freevars_of_typ t : var list =
    let rec go = function
      | TCons _ -> []
      | TInternal v | TVar {node=v;_} -> [v]
      | TApp {tfun;args;_} -> List.concat (List.map go (tfun::args)) in
    List.fold_left insert_nodup [] (go t)

  let freevars_of_eqn = freevars_of_eqn (fun x -> S.of_list (freevars_of_typ x))

  let freevars_of_eqns = freevars_of_eqns (fun x -> S.of_list (freevars_of_typ x))

  let freevars_of_formula = freevars_of_formula (fun x -> S.of_list (freevars_of_typ x))

end
