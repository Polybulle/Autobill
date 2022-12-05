open Sexpr
open FirstOrder

exception Unify of int * int
exception UnifySort of int * int
exception Cycle of int
exception Occured
exception Unbound of string
exception BadArity of int * int
exception Undefined of int
exception SortConflict of string * string
exception InvalidSort of string

type uvar = int

type ('a, 'b) _shallow = Var of uvar | Shallow of 'a * ('b list)

type 'a shallow = ('a, uvar) _shallow

type 'a folded = Fold of ('a, 'a folded) _shallow

module type Unifier_params = sig
  type node
  type deep
  type sort
  type var
  type rel
  val string_of_rel : rel -> string
  val sort_of_rel : rel -> sort list
  val is_valid_sort : sort -> bool
  val is_syntactic_sort : sort -> bool
  val sort_of_cons : node -> (sort list * sort)
  val eq : node -> node -> bool
  val string_of_sort : sort -> string
  val string_of_node : node -> string
  val folded_of_deep : (var -> sort -> node folded) -> deep -> node folded
  val mk_var : unit -> var
  val string_of_var : var -> string
  val deep_of_var : var -> deep
  val deep_of_cons : deep list -> node -> deep
end

module Make (P : Unifier_params) = struct

  include P

  module S = Map.Make(struct
      type t = uvar
      let compare = compare
    end)

  type rank = int

  type cell =
    | Redirect of uvar
    | Trivial of rank
    | Cell of P.node shallow * rank

  type subst = cell S.t

  type scheme = uvar list * uvar

  let _fresh_uvar () = Global_counter.fresh_int ()

  let _state = ref S.empty

  let _var_env : (var * rank) list ref = ref []


  let _sorts = ref S.empty

  let add_sort u so =
    if not (is_valid_sort so) then
      raise (InvalidSort (string_of_sort so));
    if S.mem u !_sorts then
      raise (Failure ("double sort declaration for " ^ string_of_int u));
    _sorts := S.add u so ! _sorts

  let get_sort u =
    try S.find u !_sorts with _ -> raise (Undefined u)

  let sort_check u v =
    if get_sort u <> get_sort u then raise (UnifySort (u,v))


  let _rank = ref 0

  let enter () = incr _rank

  let leave () = decr _rank


  let uvar_to_sexpr u = V (string_of_int u)

  let shallow_to_sexpr = function
    | Var u -> uvar_to_sexpr u
    | Shallow (node, args) ->
      S (V (string_of_node node) :: List.map uvar_to_sexpr args)

  let rec folded_to_sexpr (Fold sh) = match sh with
    | Var u -> uvar_to_sexpr u
    | Shallow (node, args) ->
      let args = List.map folded_to_sexpr args in
      S (K (string_of_node node) :: args)

  let cell_to_sexpr = function
    | Redirect u -> uvar_to_sexpr u
    | Trivial r -> V ("ρ" ^string_of_int r)
    | Cell (sh,r) -> S [V ("ρ" ^ string_of_int r); shallow_to_sexpr sh]

  let subst_to_sexpr s =
    let binds = S.bindings s in
    let sort v = V (string_of_sort (get_sort v)) in
    let aux (u, c) = S [uvar_to_sexpr u; V "->"; cell_to_sexpr c; K ":"; sort u] in
    L (List.map aux binds)

  let scheme_to_sexpr pp (svars, typ) =
    S [K "∀" ;
       S (List.map pp svars);
       pp typ]

  let fail_bad_cons_sort folded sort =
    let sort = string_of_sort sort in
    let folded = Sexpr.to_string (folded_to_sexpr folded) in
    raise (SortConflict (folded, sort))


  let set k v = _state := S.add k v !_state

  let fresh_u so =
    let u = _fresh_uvar () in
    set u (Trivial !_rank);
    add_sort u so;
    u

  let shallow ?rank:r ~sort:so sh =
    let r = Option.value r ~default:!_rank in
    let u = _fresh_uvar () in
    add_sort u so;
    set u (Cell (sh, r));
    u

  let rec of_folded ~sort rank sh = match sh with
    | Fold (Var x) -> x, [x]
    | Fold (Shallow (k, xs)) ->
      let (sos, rets) = sort_of_cons k in
      if sort <> rets then fail_bad_cons_sort sh sort;
      let xs, fvs =
        List.split @@ List.map2 (fun sort x -> of_folded rank ~sort x) sos xs in
      let y = shallow ~rank ~sort:rets (Shallow (k,xs)) in
      y, y::List.concat fvs

  let of_user_var ?env:(env=_var_env) ~sort ~rank a =
    match List.assoc_opt a !env with
    | Some u ->
      if get_sort u <> sort then
        raise (SortConflict (string_of_var a, string_of_sort sort))
      else
        u, [u]
    | None ->
      let u = _fresh_uvar () in
      add_sort u sort;
      env := List.cons (a,u) !env;
      set u (Trivial rank);
      u, [u]

  let of_rank1_typ ?rank:(rank=(!_rank)) ~sort deep =
    let of_var v sort = Fold (Var (v |> of_user_var ~sort ~rank |> fst)) in
    let sh = folded_of_deep of_var deep in
    of_folded ~sort rank sh

  let of_tvars vs =
    let of_tvar (v, sort) = of_user_var ~rank:!_rank ~sort v in
    let us, fvss = List.split (List.map of_tvar vs) in
    us, List.concat fvss

  let of_eqns eqns =
    let of_eqn = function
      | Eq (a,b,sort) ->
        let a,fvsa = of_rank1_typ ~sort a in
        let b,fvsb = of_rank1_typ ~sort b in
        Eq (a, b, sort), fvsa @ fvsb
      | Rel (rel, args) ->
        let aux = List.map2 (fun sort t -> of_rank1_typ ~sort t) (sort_of_rel rel) args in
        let args, fvss = List.split aux in
        let fvs = List.concat fvss in
        Rel (rel, args), fvs in
    List.fold_left_map
      (fun fvs eqn -> let eqn, fvs' = of_eqn eqn in (fvs@fvs',eqn))
      []
      eqns

  let compress u v =
    sort_check u v;
    if u <> v then set u (Redirect v)

  let traverse u =
    let rec loop v old = try match S.find v !_state with
      | Redirect w | Cell (Var w, _)-> loop w v
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
    | Some (Trivial r) | Some (Cell (_, r)) -> r
    | Some (Redirect _) -> assert false

  let lower_rank v k =
    match traverse v with
    | v, Some (Trivial k') -> set v (Trivial (min k k'))
    | v, Some (Cell (c,k')) -> set v (Cell (c, min k k'))
    | _, None -> ()
    | _, Some (Redirect _) -> assert false


  let unify u v =

    let non_syntactic_unifications = ref [] in

    let add u v =
      non_syntactic_unifications := (u,v)::!non_syntactic_unifications in

    let redirect urep vrep cell =
      set urep cell;
      set vrep (Redirect urep) in

    let rec go u v =
      sort_check u v;
      let (urep, uc), (vrep, vc) = traverse u, traverse v in
      if is_syntactic_sort (get_sort urep) then
        if urep = vrep then ()
        else
          match uc, vc with
          | None, _ -> compress urep vrep
          | _, None -> compress vrep urep
          | Some uc, Some vc -> go_cell uc urep vc vrep
      else if equal_syntax u v then
        redirect u vrep (Option.get (cell u))
      else
        add u v

    and go_cell uc urep vc vrep =
      match uc, vc with
      | Trivial ur, Trivial vr -> redirect urep vrep (Trivial (min ur vr))
      | Trivial tr, Cell (sh, cr)
      | Cell (sh,cr), Trivial tr ->  redirect urep vrep (Cell (sh, min cr tr))
      | Redirect _, _ | _, Redirect _
      | Cell (Var _, _), _ | _, Cell (Var _, _) -> assert false
      | Cell (Shallow (uk, uxs),ur), Cell (Shallow (vk,vxs),vr) ->
        go_sh (min ur vr) urep uk uxs vrep vk vxs

    and go_sh rank urep uk uxs vrep vk vxs =
      if eq uk vk then
        try
          List.iter2 go uxs vxs;
          redirect urep vrep (Cell (Shallow (uk, uxs), rank))
        with Invalid_argument _ -> raise (BadArity (u,v))
      else begin
        print_sexpr (subst_to_sexpr !_state);
        raise (Unify (u,v))
      end

    and equal_syntax u v =
      let rec aux u v =
        let (urep, uc), (vrep, vc) = traverse u, traverse v in
        if urep = vrep then ()
        else match uc, vc with
          | Some (Cell (Shallow (uk, uxs),_)), Some (Cell (Shallow (vk,vxs),_)) ->
            if eq uk vk then
              try
                List.iter2 aux uxs vxs
              with
                Invalid_argument _ -> raise (BadArity (u,v))
            else
              raise (Failure "")
          | _ -> raise (Failure "") in
      try aux u v; true with Failure _ -> false in

    go u v;
    !non_syntactic_unifications


  let freevars_of_scheme (us, u) =
    let rec go fvs u = match cell u with
      | Some (Cell (Var _ ,_))
      | Some (Redirect _) -> assert false
      | None -> raise (Failure "Invariant break: variable in scheme in dangling")
      | Some (Trivial _) -> u::fvs
      | Some (Cell (Shallow (_, xs), _)) ->
        let fvs = if is_syntactic_sort (get_sort u) then fvs else u::fvs in
        List.fold_left go fvs xs
    in
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

  (* HACK: First-Order shouldn't have anything to do here *)
  let refresh_scheme (us, u, eqns) =
    let env = ref [] in
    let us = List.map repr us in
    let copy old c =
      let young = fresh_u (get_sort old) in
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
    let go_eq =
      let open FirstOrder in function
      | Eq (u,v,so) -> Eq (go u, go v, so)
      | Rel (r, us) -> Rel (r, List.map go us) in
    (List.map go us, go u, List.map go_eq eqns)

  (* Assuming all relevant freevars of rank less than [r] are maximally
   * lifted, lift the freevars [vs], of rank [r], at their lowest rank *)
  let lift_freevars r vs : unit =
    let rec aux children =
      List.fold_left (fun acc child -> go child; max acc (rank child))
        0 children
    and go v =
      if not (is_syntactic_sort (get_sort v)) then
        (* We never lift indices since ranking isn't correct for them *)
        ()
      else begin
        lower_rank v r;
        match cell v with
        | None -> ()
        | Some c -> match c with
          | Redirect _
          | Trivial _ -> ()
          | Cell (sh, vr) -> go_sh v vr sh
      end
    and go_sh v vr = function
      | Var _ -> ()
      | Shallow (_, args) -> if vr = r then lower_rank v (aux args)
    in
    List.iter go vs

  (* filters the quantifiers of a scheme that that under a given rank,
   * returns the filtered scheme and the low-ranked variables separately *)
  let extract_old_vars (us,u) r  =
    let rec test u =
      not (is_syntactic_sort (get_sort u)) || match cell u with
      | Some (Trivial r') -> r <= r'
      | Some (Cell (Var u, _)) -> test u
      | Some (Cell (Shallow _, _))
      | Some (Redirect _)
      | None -> false in
    let young, old = List.partition test us in
    (young, u), old

  let occurs_check (_, u) =
    let rec go old u =
      if List.mem (repr u) old then
        raise Occured
      else if is_syntactic_sort (get_sort u) then
        let old = u :: old in
        match cell u with
        | None ->
          raise (Failure ("Invariant break: variable in scheme is dangling: "
                          ^ string_of_int u))
        | Some (Redirect _) -> assert false
        | Some (Trivial _) -> ()
        | Some (Cell (sh,_)) -> go_sh old sh

    and go_sh old sh = match sh with
      | Var u -> go old u
      | Shallow (_, xs) -> List.iter (go old) xs
    in
    try go [] u; true
    with Occured -> false


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

  type generalizer = (var list * deep) option ref
  let _gens : (generalizer * uvar list * uvar) list ref = ref []
  let new_gen () = ref None
  let generalize _ gen (us, u) =
    _gens := (gen, us, u) :: !_gens

  type output_env = {
    u : uvar -> deep;
    var : string -> deep;
    get : uvar -> var
  }

  let finalize_env () : output_env =

    (* NOTE: Exporting stops registering new vars from type annotations *)

    let ven = ref (List.map (fun (a, u) -> (repr u, a)) !_var_env) in
    let add u =
      let a = mk_var () in
      if Option.is_none (cell u) then
        set u (Trivial (-2));
      ven := (repr u, a) :: !ven;
      a in
    let get u : var =
      match List.assoc_opt (repr u) !ven with
      | Some a -> a
      | None -> add u in
    let rec aux u =
      let urep, cell = traverse u in
      match cell with
      | None ->
        raise (Failure "export")
      | Some (Trivial _) ->  deep_of_var (get urep)
      | Some (Redirect _) | Some (Cell (Var _, _)) -> assert false
      | Some (Cell (Shallow (k, args), _)) ->
        deep_of_cons (List.map aux args) k
    in


    let env_scheme (us,u) = (List.map get us, aux u) in
    let spec_aux (spec, vs) = spec := List.map aux vs in
    let gen_aux (gen, us, u) = gen := Some (env_scheme (us, u)) in
    List.iter spec_aux !_specializers;
    List.iter gen_aux !_gens;
    {
      u = aux;
      (* TODO we can't generalise variables yet *)
      var = (fun x -> match List.assoc_opt x !_nvar_env with
          | None -> raise (Unbound x)
          | Some (_,u) -> aux u);
      get
    }

  let reset_unifier () =
    _rank := 0;
    _sorts := S.empty;
    _state := S.empty;
    _var_env := [];
    _specializers := [];
    _gens := []


end
