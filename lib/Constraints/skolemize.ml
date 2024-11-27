open Vars
open FirstOrder
open FullFOL
open Primitives
open Format
open Polynomials
open Skolem
open Constraint_common
open Prelude

exception Invariant_break_not_convertible_to_optimization of string

let fail_untranslatable mess = raise (Invariant_break_not_convertible_to_optimization mess)

let fail_bad_sorting () =
  Misc.fail_invariant_break "Infered polynomial cannot be evaluated due to bad sorting"


module type P = sig
  val opt : (formula, Vars.TyConsVar.t) optim_program
end

module type IGlobals = sig
  val fresh : unit -> TyVar.t
  val export : unit -> TyVar.t list
end

module Globals () : IGlobals = struct
  let existentials = ref []
  let fresh () =
    let p = TyVar.fresh () in
    existentials := p :: !existentials;
    p
  let export () =
    !existentials
end


module Scope (P:P) (G : IGlobals) = struct

  let env = ref TyVar.Env.empty

  let univs = ref []

  let _add x t = (env := TyVar.Env.add x t !env)

  let get x = TyVar.Env.find x !env

  let add_univ x =
    univs := (V x) :: !univs;
    _add x (Poly.of_mono (Mono.of_var (V x)))

  let add_exists x =
    let deg = match P.opt.max_degree with
      | None -> Misc.fatal_error "Generating complexity model" "The program defines no goal to infer"
      | Some d -> d in
    let p = free_poly ~callback:(fun _ -> G.fresh ()) ~base:!univs ~degree:deg in
    _add x p

end


module Conses (P : P) (G : IGlobals) = struct

  type info = {
      doc : string;
      params : TyVar.t list array;
      args : polyVar list;
      body : Poly.t
    }

  let _env = ref TyConsVar.Env.empty

  let _degree =
    let deg = match P.opt.max_degree with
      | None -> Misc.fatal_error "Generating complexity model" "The program defines no goal to infer"
      | Some d -> d in
    ref deg

  let _mk_formula name formals body =
    let pp_sep fmt () =  fprintf fmt ", " in
    fprintf str_formatter "@[%a(%a) = %a@]"
      (TyConsVar.pp ~debug:false) name
      (pp_print_list ~pp_sep (Polynomials.pp_var ~debug:false)) formals
      Poly.pp body;
    flush_str_formatter ()

  let _mk_args n_args =
    let names = ["X"; "Y"; "Z"; "T"; "S"; "R"; "N"; "M"; "P"; "Q"] in
    List.init n_args
      (fun i -> Polynomials.V (if i < List.length names
                               then TyVar.of_string (List.nth names i)
                               else TyVar.fresh ()))

  let fresh name n_args =
    let base = _mk_args n_args in
    let arr = Array.make (!_degree + 1) [] in
    let callback m =
      let v = G.fresh () in
      let d = Mono.degree_of m in
      arr.(d) <- v :: arr.(d);
      v in
    let body = free_poly ~base ~callback ~degree:!_degree in
    let doc = _mk_formula name base body in
    let res = {doc; args=base; body; params=arr} in
    _env := TyConsVar.Env.add name res !_env;
    res

  let get c =
    match TyConsVar.Env.find_opt c !_env with
    | Some res -> res
    | None -> match arity_of_polynomial P.opt c with
              | None -> fail_bad_sorting ()
              | Some n -> fresh c n

  let formal_args_of c = (get c).args

  let body_of c = (get c).body

end



module Converter (P:P) = struct

  module Globals = Globals ()
  module Scope = Scope (P) (Globals)
  module Conses = Conses (P) (Globals)

  let rec convert_term (t:term) = match t with
    | TVar {node=v;_} | TInternal v -> Scope.get v
    | TCons {node=Cons c;_} ->
       if c = nat_zero then Poly.zero
       else if c = nat_one then Poly.unit
       else if c = nat_large then Poly.scale (Scalar.of_int 1000) (Poly.of_mono Mono.unit)
       else convert_cons_const c
    | TApp {tfun = TCons {node = Cons c; _}; args; _} ->
       let accum f acc t = f acc (convert_term t) in
       if c = nat_add then List.fold_left (accum Poly.add) Poly.zero args
       else if c = nat_mult then List.fold_left (accum Poly.mult) Poly.unit args
       else convert_cons_call c args
    | TApp {tfun = TApp {tfun;args=args1;_}; args=args2; loc} ->
       convert_term (TApp {tfun; args = args1 @ args2; loc})
    | _ -> fail_untranslatable "Unskolemizable term"

  and convert_cons_const c =
    if List.length (Conses.formal_args_of c) > 0 then fail_bad_sorting ();
    Conses.body_of c

  and convert_cons_call c args =
    let formal_args = Conses.formal_args_of c in
    if List.compare_lengths args formal_args <> 0 then fail_bad_sorting ();
    let body = Conses.body_of c in
    let args = List.map convert_term args in
    let instanciate_args v =
      let rec find_idx i = function
        | w::t -> if v=w then i else find_idx (i+1) t
        | [] -> fail_bad_sorting () in
      List.nth args (find_idx 0 formal_args) in
    Poly.subst instanciate_args body

  let poly_of_eqn eqn = match eqn with
    | Eq (t,u,_) -> (Poly.sub (convert_term t) (convert_term u))
    | Rel _ -> fail_untranslatable "unsupported relation"

  let convert_eqns eqns = (List.map poly_of_eqn eqns)

  let rec convert_formula f = match f with
    | PTrue -> SFTrue
    | PFalse -> SFFalse
    | PLoc (_, f) -> convert_formula f
    | PEqn eqns -> SFNull (convert_eqns eqns)
    | PAnd fs | PCases fs -> SFAnd ([], List.map convert_formula fs)
    | PExists (xs, ys, eqns, f) ->
       List.iter Scope.add_exists xs;
       List.iter Scope.add_exists ys;
       SFAnd (convert_eqns eqns, [convert_formula f])
    | PForall (xs, ys, eqns, eqns', f) ->
       List.iter Scope.add_univ xs;
       List.iter Scope.add_exists ys;
       let eqns = List.map poly_of_eqn eqns in
       let eqns' = convert_eqns eqns' in
       SFImplies (eqns, eqns', convert_formula f)

  let convert_fol opt =
    let formula = convert_formula opt.formula in
    let globals = Globals.export () in
    let info = match Option.map Conses.get opt.goals with
      | None -> assert false
      | Some info -> info in
    let goals =
      let sum  = List.fold_left
                     (fun x v -> Scalar.add x (Scalar.Param v))
                     Scalar.zero in
      Array.fold_left (fun l vs -> (sum vs) :: l) [] info.params in
    let doc = info.doc in
    let opt = {opt with formula; globals; goals = Some goals; doc} in
    compress opt

end
