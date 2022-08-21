open Sexpr


type uvar = int

type ('a, 'b) _shallow = Var of uvar | Shallow of 'a * ('b list)

type 'a shallow = ('a, uvar) _shallow

type 'a folded = Fold of ('a, 'a folded) _shallow

module type Unifier_params = sig
  type node
  type deep
  val arity : node -> int
  val eq : node -> node -> bool
  val string_of_node : node -> string
  val folded_of_deep : (string -> node folded) -> deep -> node folded
  val mk_var : unit -> string
  val deep_of_var : string -> deep
  val deep_of_cons : deep list -> node -> deep
end

module Make (P : Unifier_params) = struct

  include P

  type rank = int

  exception Unify of int * int
  exception Incompatible_datatypes of string * string
  exception Cycle of int
  exception Occured
  exception Unbound of string
  exception BadArity of int * int

  let uvar_to_sexpr u = V (string_of_int u)

  let _fresh_uvar = Global_counter.fresh_int

  let _rank = ref 0

  let enter () = incr _rank

  let leave () = decr _rank

  type cell =
    | Redirect of uvar
    | Trivial of rank
    | Cell of P.node shallow * rank

  let shallow_to_sexpr = function
    | Var u -> uvar_to_sexpr u
    | Shallow (node, args) ->
      S (V (string_of_node node) :: List.map uvar_to_sexpr args)

  let cell_to_sexpr = function
    | Redirect u -> uvar_to_sexpr u
    | Trivial r -> V ("ρ" ^string_of_int r)
    | Cell (sh,r) -> S [V ("ρ" ^ string_of_int r); shallow_to_sexpr sh]

  module S = Map.Make(struct
      type t = uvar
      let compare = compare
    end)

  type subst = cell S.t

  let subst_to_sexpr s =
    let binds = S.bindings s in
    let aux (u, c) = S [uvar_to_sexpr u; V ":"; cell_to_sexpr c] in
    L (List.map aux binds)

  let _state = ref S.empty

  let _var_env = ref []

  let set k v = _state := S.add k v !_state

  let fresh_u () =
    let u = _fresh_uvar () in
    set u (Trivial !_rank);
    u

  let shallow ?rank:r sh  =
    let r = Option.value r ~default:!_rank in
    let u = _fresh_uvar () in
    set u (Cell (sh, r));
    u

  let rec of_folded rank = function
    | Fold (Var x) -> x, [x]
    | Fold (Shallow (k, xs)) ->
      let xs, fvs = List.split @@ List.map (of_folded rank) xs in
      let y = shallow ~rank (Shallow (k,xs)) in
      y, y :: List.concat fvs

  let of_user_var ?env:(env=_var_env) rank a =
    match List.assoc_opt a !env with
    | Some u -> u, []
    | None ->
      let u = _fresh_uvar () in
      _var_env := List.cons (a,u) !env;
      set u (Trivial rank);
      u, [u]

  let of_deep rank deep =
    let of_var v = Fold (Var (v |> of_user_var rank |> fst)) in
    let folded = folded_of_deep of_var deep in
    of_folded rank folded

  let of_rank1_typ = of_deep !_rank

  let of_tvar = of_user_var !_rank

  let of_prim p = fst (of_deep (-1) p)

  let compress u v = if u <> v then set u (Redirect v)

  let traverse u =
    let rec loop v old = try match S.find v !_state with
      | Redirect w -> loop w v
      | c -> v, Some c
      with _ -> old, None in
    let v,c = loop u u in
    compress u v; v,c

  let repr u =
    let v,_ = traverse u in v

  let cell u =
    let _, c = traverse u in c

  let rank u = match cell u with
    | None -> -1
    | Some (Trivial r) -> r
    | Some (Cell (_, r)) -> r
    | Some (Redirect _) -> assert false

  let lower_rank v k =
    match traverse v with
    | v, Some (Trivial k') -> set v (Trivial (min k k'))
    | v, Some (Cell (c,k')) -> set v (Cell (c, min k k'))
    | _, None -> ()
    | _, Some (Redirect _) -> assert false

  let rec unify u v =
    let (urep, uc), (vrep, vc) = traverse u, traverse v in
    if urep = vrep then urep
    else
      match uc, vc with
      | None, _ -> compress urep vrep; vrep
      | _, None -> compress vrep urep; urep
      | Some uc, Some vc ->

        let cell = match uc, vc with

          | Trivial ur, Trivial vr -> Trivial (min ur vr)

          | Trivial tr, Cell (sh, cr)
          | Cell (sh,cr), Trivial tr -> Cell (sh, min cr tr)

          | Cell (Shallow (uk, uxs),ur), Cell (Shallow (vk,vxs),vr) ->
            if not (eq uk vk) then fail u v;
            if List.compare_lengths uxs vxs != 0 then raise (BadArity (u,v));
            let xs = List.map2 unify uxs vxs in
            Cell (Shallow (uk, xs), min ur vr)

          | Redirect _,_ | _, Redirect _
          | Cell (Var _, _), _ | _, Cell (Var _, _) -> assert false in

        set urep cell; set vrep (Redirect urep); urep

  and fail u v =
    print_sexpr (subst_to_sexpr !_state);
    raise (Unify (u,v))


  type scheme = uvar list * uvar

  let scheme_to_sexpr pp (svars, typ) =
    S [K "∀" ;
       S (List.map pp svars);
       pp typ]

  let freevars_of_scheme (us, u) =
    let rec go fvs u = match cell u with
      | Some (Cell (Var _ ,_))
      | Some (Redirect _) -> assert false
      | None -> raise (Failure "Invariant break: variable in scheme in dangling")
      | Some (Trivial _) -> u::fvs
      | Some (Cell (Shallow (_, xs), _)) ->
        List.concat (fvs :: List.map (go []) xs) in
    us @ go [] u

  let ranked_freevars_of_scheme s r =
    let fvs = freevars_of_scheme s in
    let a = Array.make (r+1) [] in
    let aux v =
      lower_rank v r;
      let r' = rank v in
      if r' > -1 then (* ignoring primitive types who have rank = -1 *)
        a.(r') <- v :: a.(r') in
    List.iter aux fvs;
    a

  let refresh_scheme (us, u) =
    let env = ref [] in
    let us = List.map repr us in
    let copy old c =
      let young = fresh_u () in
      env := (repr old,young) :: !env;
      set young c;
      young in
    let rec go v =
      let vrep, cell = traverse v in
      match List.assoc_opt vrep !env with
      | Some w -> w
      | None -> match cell with
         | Some (Cell (Shallow (k, xs),r)) ->
           copy vrep (Cell (Shallow (k, List.map go xs), r))
         | Some (Trivial r) ->
           if not (List.mem vrep us) then vrep
           else copy vrep (Trivial r)
         | Some (Redirect _)
         | Some (Cell (Var _, _))
         | None -> assert false in
    let u' = go u in
    let us' = List.map go us in
    (us', u')

  (* Assuming all relevant freevars of rank less than [r] are maximally
   * lifted, lift the freevars [vs], of rank [r], at their lowest rank *)
  let lift_freevars r vs : unit =
    let rec aux children =
      List.fold_left (fun acc child -> go child; max acc (rank child))
        0 (children)
    and go v =
      lower_rank v r;
      match cell v with
      | None -> ()
      | Some c -> match c with
        | Redirect _
        | Trivial _
        | Cell (Var _, _) -> ()
        | Cell (Shallow (_, args), _) ->
          if rank v = r then
            lower_rank v (aux args)
    in
    List.iter go vs

  (* filters the quantifiers of a scheme that that under a given rank,
   * returns the filtered scheme and the high-ranked variables separately *)
  let extract_old_vars (us,u) r  =
    let test u = match cell u with
      (* | Some (Cell (ShArrow _,_)) -> false *)
      | Some (Trivial r') -> r <= r'
      | _ -> false in
    let young, old = List.partition test us in
    (young, u), old

  let occurs_check (_, u) =
    let rec go parents u =
      if List.mem (repr u) parents then raise Occured;
      let parents = (repr u) :: parents in
      match cell u with
      | None ->
        raise (Failure
                 ("Invariant break: variable in scheme is dangling: " ^ string_of_int u))
      | Some (Redirect _) -> assert false
      | Some (Trivial _) -> ()
      | Some (Cell (sh,_)) -> match sh with
        | Var u -> go parents u
        | Shallow (_, xs) -> List.iter (go parents) xs
            in
    try go [] u; true
    with Occured -> false


  (* NOTE: Exporting stops registering new vars from type annotations *)
  type exporter = (uvar -> string) * (uvar -> deep)

  let _make_exporter ()  =
    let ven = ref (List.map (fun (a, u) -> (repr u, a)) !_var_env) in
    let add u =
      let a = mk_var () in
      if Option.is_none (cell u) then
        set u (Trivial (-2));
      ven := (repr u, a) :: !ven; a in
    let get u = match List.assoc_opt (repr u) !ven with
      | Some a -> a
      | None -> add u in
    let rec aux u =
      let urep, cell = traverse u in
      match cell with
      | None ->
        raise (Failure "export")
      | Some (Trivial _) ->  deep_of_var (get urep)
      | Some (Redirect _) | Some (Cell (Var _, _))-> assert false
      | Some (Cell (Shallow (k, args), _)) ->
        deep_of_cons (List.map aux args) k
    in
    ((get, aux) : exporter)



  let _nvar_env : (string * scheme) list ref = ref []

  let define x (us, u) =
    let s = (List.map repr us, repr u) in
    _nvar_env := (x, s) :: !_nvar_env

  let _env_to_sexpr () =
    let aux (v,s) = S [V v; scheme_to_sexpr uvar_to_sexpr s] in
    S (List.map aux !_nvar_env)


  type specializer = deep list ref
  let _specializers : (specializer * uvar list) list ref = ref []
  let new_spec () = ref []
  let specialize spec vs : unit =
    _specializers := (spec, vs) :: !_specializers

  type generalizer = (string list * deep) ref
  let _gens :
    (generalizer * uvar list * uvar) list ref = ref []
  let new_gen () = ref ([], deep_of_var (mk_var ()))
  let generalize _ gen (us, u) =
    _gens := (gen, us, u) :: !_gens


  let finalize_env () : (uvar -> deep) * (string -> (string list * deep)) =
    let get, env = _make_exporter () in
    let env_scheme (us,u) = (List.map get us, env u) in
    let spec_aux (spec, vs) = spec := List.map env vs in
    let gen_quant_aux (gen, us, u) =
      gen := env_scheme (us, u)
    in
    List.iter spec_aux !_specializers;
    List.iter gen_quant_aux !_gens;
    env, fun x -> match List.assoc_opt x !_nvar_env with
      | None -> raise (Unbound x)
      | Some s -> env_scheme s

  let reset_unifier () =
    _rank := 0;
    _state := S.empty;
    _var_env := [];
    _var_env := [];
    _specializers := [];
    _gens := []


end
