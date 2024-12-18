open Vars
open Format


type polyVar = C of TyConsVar.t | V of TyVar.t

module PolyVarEnv = Map.Make (struct type t = polyVar let compare = compare end)

let pp_var ?debug fmt v = match v with
  | C c -> TyConsVar.pp ?debug fmt c
  | V v -> TyVar.pp ?debug fmt v

module Scalar = struct

  type t =
    | Param of TyVar.t
    | Cons of TyConsVar.t
    | Cst of int
    | Mult of t * t
    | Add of t * t
    | Pow of t * int

  let of_int n = Cst n
  let of_param v = Param v
  let unit = of_int 1
  let zero = of_int 0
  let mult a b = Mult (a,b)
  let add a b = Add (a,b)
  let pow a n = Pow (a,n)

  let rec int_pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
    let b = int_pow a (n / 2) in
    b * b * (if n mod 2 = 0 then 1 else a)

  let rec simplify = function
    | Param v -> Param v
    | Cons c -> Cons c
    | Cst n -> Cst n
    | Mult (a,b) ->
      begin match simplify a, simplify b with
        | Cst a, Cst b -> Cst (a*b)
        | Cst 1, a | a, Cst 1 -> a
        | a,b -> Mult (a,b)
      end
    | Add (a,b) ->
      begin match simplify a, simplify b with
        | Cst a, Cst b -> Cst (a+b)
        | Cst 0, a | a, Cst 0 -> a
        | a,b -> Add (a,b)
      end
    | Pow (a,n) -> begin match simplify a with
        | Cst a -> Cst (int_pow a n)
        | a -> Pow (a,n)
      end

  let pp fmt x =
    let rec go fmt = function
    | Param v ->
      (* if for_mzn then *)
      (*   fprintf fmt "\\(%a)" (TyVar.pp ~debug:true) v *)
      (* else *)
        fprintf fmt "%a" (TyVar.pp ~debug:true) v
    | Cons c -> TyConsVar.pp fmt c
    | Cst n -> pp_print_int fmt n
    | Mult (a,b) -> fprintf fmt "(%a * %a)" go a go b
    | Add (a,b) -> fprintf fmt "(%a + %a)" go a go b
    | Pow (a,n) -> fprintf fmt "(%a ^ %d)" go a n in
    go fmt (simplify x)

end

module Mono = struct

  module M = PolyVarEnv

  type t =
    | Unit
    | Mono of {
      degree : int;
      powers : int M.t
    }

  let unit = Unit
  let of_var v = Mono {degree = 1; powers = M.singleton v 1}
  let mult a b = match a,b with
    | Unit, a | a, Unit -> a
    | (Mono {degree = da; powers = pa}), (Mono {degree = db; powers = pb}) ->
      Mono {
          degree = da + db;
          powers = M.union (fun _ a b -> Some (a+b)) pa pb
        }
  let pow a n =
    if n = 0 then Unit else
    match a with
    | Unit -> Unit
    | (Mono {degree; powers}) -> Mono {
      powers = M.map (( * ) n) powers;
      degree = degree * n
    }

  let degree_of = function
    | Unit -> 0
    | (Mono {degree; _}) -> degree

  let pp_one fmt (x,k) =
    if k = 1 then pp_var ~debug:false fmt x
    else fprintf fmt "%a^%d" (pp_var ~debug:false) x k

  let pp fmt = function
    | Unit -> pp_print_string fmt "1"
    |Mono {powers; _} ->
      if powers = M.empty then
        pp_print_string fmt "1"
      else begin
        pp_open_hbox fmt ();
        pp_print_seq
          ~pp_sep:(fun fmt () -> fprintf fmt " * ")
          pp_one
          fmt
          (M.to_seq powers);
        pp_close_box fmt ()
        end

  let eval f = function
    | Unit -> Scalar.Cst 1
    | (Mono {powers; _}) -> begin
      let acc = ref Scalar.unit in
      M.iter (fun m k ->
          acc := Scalar.(mult !acc (pow (f m) k));
        ) powers;
      !acc
      end
end

module Poly = struct

  module P = Map.Make (struct type t = Mono.t let compare = compare end)

  type t = Scalar.t P.t

  let zero = P.empty
  let of_mono m = P.singleton m (Scalar.of_int 1)
  let unit = of_mono Mono.unit
  let scale a p = P.map (fun b -> Scalar.mult a b) p
  let add p q = P.union (fun _ a b -> Some (Scalar.add a b)) p q
  let sub p q = add p (scale (Scalar.of_int (-1)) q)
  let mult p q =
    let res = ref zero in
    let add_term p = (res := add !res p) in
    P.iter (fun p a ->
        P.iter (fun q b ->
            add_term (scale (Scalar.mult a b) (of_mono (Mono.mult p q)))
          ) q
      ) p;
    !res
   let rec pow p n =
    if n = 0 then unit
    else if n = 1 then p
    else if n mod 2 = 0 then let q = pow p (n/2) in mult q q
    else let q = pow p (n/2) in (mult q (mult q p))

  let pp fmt p =
    if p = P.empty then pp_print_string fmt "0" else
      let pp_one fmt (m,a) =
        if m = Mono.Unit then
          Scalar.pp fmt a
        else
          fprintf fmt "@[<h 0>%a * [%a]@]" Scalar.pp a Mono.pp m in
      let pp_sep fmt () = fprintf fmt "@ + " in
      (pp_print_seq ~pp_sep pp_one) fmt (P.to_seq p)

  let simplify p = P.map Scalar.simplify p

  let subst_mono (f : polyVar -> t) m : t =
    match m with
    | Mono.Unit -> unit
    | (Mono {powers;_} : Mono.t) ->
      let acc = ref unit in
      Mono.M.iter (fun x k -> acc := mult !acc (pow (f x) k)) powers;
      !acc

  let subst f p =
    let acc = ref zero in
    P.iter (fun m a -> acc := add !acc (scale a (subst_mono f m))) p;
    !acc

   let eval f p =
    let acc = ref Scalar.zero in
    P.iter (fun x k -> acc := Scalar.(add !acc (mult k (Mono.eval f x)))) p;
    !acc

end


let rec all_monomial_iterator ~max_degree ~base = match max_degree, base with
  | 0, _ | _, [] -> Seq.return Mono.unit
  | 1, base -> Seq.cons Mono.unit (List.to_seq (List.map Mono.of_var base))
  | n, x::base -> Seq.concat @@ Seq.init (n+1) (fun i ->
      Seq.map
        (Mono.mult (Mono.pow (Mono.of_var x) i))
        (all_monomial_iterator ~max_degree:(n-i) ~base))

let free_poly
    ~base:(base : polyVar list)
    ~degree:(degree : int)
    ~callback:(f : Mono.t -> TyVar.t) : Poly.t =
  let base = all_monomial_iterator ~max_degree:degree ~base in
  let acc = ref Poly.zero in
  Seq.iter (fun m ->
      acc := Poly.add !acc (Poly.scale (Scalar.of_param (f m)) (Poly.of_mono m))
    ) base;
  !acc
