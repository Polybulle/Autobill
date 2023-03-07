open Misc
open Format

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

  include P

  type eqn =
    | Eq of term * term * sort
    | Rel of rel * term list

  type formula =
    | PTrue
    | PFalse
    | PLoc of position * formula
    | PEqn of eqn list
    | PAnd of formula list
    | PExists of var list * var list * eqn list * formula
    | PForall of var list * var list * eqn list * formula

  type ctx =
    | KEmpty
    | KLoc of position * ctx
    | KAnd of formula list * ctx * formula list
    | KForall of var list * var list * eqn list * ctx
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
      | PExists (vars, duty, eqns, rest) ->
        fprintf fmt "@[<v 1>(:exists %a@ :witness %a@ :with %a@ :then %a)@]"
          pp_vars vars
          pp_vars duty
          pp_eqns eqns
          pp rest
      | PForall (vars, duty, eqns, rest) ->
        fprintf fmt "@[<v 1>(:forall %a@ :keep-free %a@ :assume %a@ :then %a)@]"
          pp_vars vars
          pp_vars duty
          pp_eqns eqns
          pp rest

    in pp fmt f

  let string_of_formula ?with_loc:(with_loc=true) f =
    pp_formula ~with_loc str_formatter f; flush_str_formatter ()

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
  | PExists (xs, ys, eqns, c) ->
    PExists (List.map f_var xs,
             List.map f_var ys,
             map_eqns f_term eqns,
             map f_var f_term c)
  | PForall (xs, ys, eqns, c) ->
    PForall (List.map f_var xs,
             List.map f_var ys,
             map_eqns f_term eqns,
             map f_var f_term c)

  let visit_eqns f eqns =
    let go = function
      | Eq (a,b, _) -> f a; f b
      | Rel (_, args) -> List.iter f args in
    List.iter go eqns

  let visit f_var f_term =
    let rec go = function
    | PTrue
    | PFalse -> ()
    | PLoc (_, c) -> go c
    | PEqn eqns -> visit_eqns f_term eqns
    | PAnd cs -> List.iter go cs
    | PExists (xs, ys, eqns, c) ->
      List.iter f_var xs;
      List.iter f_var ys;
      visit_eqns f_term eqns;
      go c
    | PForall (xs, ys, eqns, c) ->
      List.iter f_var xs;
      List.iter f_var ys;
      visit_eqns f_term eqns;
      go c

  in go


type compress_quantifiers_t =
  | Univ of var list * var list * eqn list
  | Exist of var list * var list * eqn list

let rec compress_logic ?(remove_loc = false) c =

  let canary = ref true in

  let kill () = (canary := false) in

  let rec compress_eqns eqns =
    let rec remove_ids = function
    | [] -> []
    | eqn::eqns -> match eqn with
      | Eq (a,b,_) when a = b -> kill (); remove_ids eqns
      | Eq (a,b,so) when b > a -> (Eq (b,a,so)) :: (remove_ids eqns)
      | _ -> eqn :: (remove_ids eqns) in
    List.fold_left insert_nodup [] (remove_ids eqns)

  and advance c ctx = match c with
    | PTrue | PEqn [] -> backtrack PTrue ctx
    | PFalse -> backtrack PFalse ctx
    | PEqn eqns -> backtrack (PEqn (compress_eqns eqns)) ctx
    | PLoc (loc, c) ->
      if remove_loc then advance c ctx else advance c (lift_loc loc ctx)
    | PAnd xs -> compress_and xs ctx
    | PExists ([], [], [], c) -> kill (); advance c ctx
    | PExists ([], [], eqns, c) -> kill (); advance (PAnd [PEqn (eqns); c]) ctx
    | PExists (xs, ys, eqns, c) ->
      advance c (lift_quant (Exist (xs, ys, compress_eqns eqns)) ctx)
    | PForall ([], [], [], x) -> kill (); advance x ctx
    | PForall (vars, duty, eqns, c) ->
      advance c (lift_quant (Univ (vars, duty, compress_eqns eqns)) ctx)

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
    | KForall (vars, duty, eqns, ctx) -> backtrack (PForall (vars, duty, eqns, c)) ctx
    | KExists (xs, ys, eqns, ctx) -> backtrack (PExists (xs, ys, eqns, c)) ctx

  and compress_and cs ctx =
    let rec go acc vars duty eqns cs = match cs with
      | [] -> vars, duty, eqns, acc
      | PLoc (_,c) :: cs when remove_loc -> go acc vars duty eqns (c :: cs)
      | PAnd (cs') :: cs -> kill (); go acc duty vars eqns (cs' @ cs)
      | PEqn eqns' :: cs -> go acc vars duty (eqns' @ eqns) cs
      | PExists (vars', duty', eqns',c)::cs ->
        go acc (vars' @ vars) (duty' @ duty) (eqns' @ eqns) (c::cs)
      | c :: cs -> go (c::acc) vars duty eqns cs in
    let vars, duty, eqns, xs = go [] [] [] [] cs in
    match vars, duty, eqns with
    | [], [], [] -> advance PTrue (KAnd ([], ctx, xs))
    | [], [], eqns -> advance PTrue (KAnd ([PEqn eqns], ctx, xs))
    | vars, duty, eqns ->
      let ctx = lift_quant (Exist (vars, duty, eqns)) ctx in
      advance PTrue (KAnd ([], ctx, xs))

  and lift_loc loc = function
    | KLoc (_, ctx) -> kill (); KLoc (loc, ctx)
    | ctx -> KLoc (loc, ctx)

  and lift_quant vs ctx =
    if vs = Univ ([], [], []) || vs = Exist ([], [], []) then
      ctx
    else match vs,ctx with
    | Exist (xs, ys, eqns), KExists (xs', ys', eqns', ctx') ->
      let xs = List.fold_left Misc.insert_nodup xs xs' in
      let ys = List.fold_left Misc.insert_nodup ys ys' in
      kill (); KExists (xs, ys, eqns@eqns', ctx')
    | Univ (us, vs, eqns), KForall (us', vs', eqns', ctx') ->
      let us = List.fold_left Misc.insert_nodup us us' in
      let vs = List.fold_left Misc.insert_nodup vs vs' in
      kill (); KForall (us, vs, eqns@eqns', ctx')
    | Exist (xs, ys, eqns), _ -> KExists (xs, ys, eqns, ctx)
    | Univ (us, vs, eqns), _ -> KForall (us, vs, eqns, ctx) in

  let c = advance c KEmpty in
  if not !canary then
    compress_logic ~remove_loc c
  else
    advance c KEmpty

end

module FullFOL =  FOL(struct
    open Vars
    open Types
    type var = TyVar.t
    type sort = SortVar.t Types.sort
    type rel = RelVar.t
    type term = typ

    let pp_var = TyVar.pp
    let pp_rel = RelVar.pp
    let pp_sort = pp_sort SortVar.to_string
    let pp_term = pp_typ TyConsVar.pp TyVar.pp
  end)
