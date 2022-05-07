open Vars

module type Constructors = sig
  type 'x constructor
  type 't pos_type_cons
  type 't neg_type_cons
  type ('x, 'a) destructor

  val string_of_constructor : ('x -> string) -> ('x constructor) -> string
  val string_of_pos_type_cons : ('t -> string) -> ('t pos_type_cons) -> string
  val string_of_neg_type_cons : ('t -> string) -> ('t neg_type_cons) -> string
  val string_of_destructor : ('x -> string) -> ('a -> string) -> ('x, 'a) destructor -> string
end


module Empty = struct
    type 'x constructor = |
    type 't pos_type_cons = |
    type 't neg_type_cons = |
    type ('x, 'a) destructor = |

    let fail _ = failwith "Impossible"

    let string_of_constructor = fail
    let string_of_destructor = fail
    let string_of_pos_type_cons = fail
    let string_of_neg_type_cons = fail
  end

module TextConstructors = struct

  type 'x constructor = string * 'x list
  type 't pos_type_cons = string * 't list
  type 't neg_type_cons = string * 't list
  type ('x, 'a) destructor = string * 'a * 'x list

  let string_of_constructor k (cons,args) = pp_sexp cons (List.map k args)
  let string_of_destructor kx ka (cons,kont,args) =
    pp_sexp cons (ka kont :: List.map kx args)
  let string_of_pos_type_cons k (cons, args) = pp_texp cons (List.map k args)
  let string_of_neg_type_cons = string_of_pos_type_cons
end

module ILL = struct

  type 't pos_type_cons =
    | Unit
    | Zero
    | Prod of 't * 't
    | Sum of 't * 't
    (*let unit = Unit*)
  let zero = Zero
  let prod a b = Prod (a,b)
  let sum a b = Sum (a,b)
  let string_of_pos_type_cons k = function
    | Unit -> "unit"
    | Zero -> "zero"
    | Prod (a,b) -> pp_texp "prod" [k a; k b]
    | Sum (a,b) -> pp_texp "sum" [k a; k b]

  type 't neg_type_cons =
    | Top
    | Bottom
    | Fun of 't * 't
    | Choice of 't * 't
  let top = Top
  let bottom = Bottom
  let func a b = Fun (a,b)
  let choice a b = Choice (a,b)
  let string_of_neg_type_cons k = function
    | Top -> "top"
    | Bottom -> "bottom"
    | Fun (a,b) -> pp_texp "fun" [k a; k b]
    | Choice (a,b) -> pp_texp "choice" [k a; k b]

  type 'x constructor =
    | Unit
    | Pair of 'x * 'x
    | Fst of 'x
    | Snd of 'x
  let unit = Unit
  let pair a b = Pair (a,b)
  let fst a = Fst a
  let snd b = Snd b
  let string_of_constructor k = function
    | Unit -> ":unit"
    | Pair (x,y) -> pp_sexp ":pair" [k x; k y]
    | Fst x -> pp_sexp ":fst" [k x]
    | Snd y -> pp_sexp ":snd" [k y]

  type ('x ,'a) destructor =
    | Call of 'x * 'a
    | Yes of 'a
    | No of 'a
  let call x a = Call (x,a)
  let yes a = Yes a
  let no a = No a
  let string_of_destructor kx ka = function
    | Call (x,a) -> pp_sexp ":call" [kx x; ka a]
    | Yes a -> pp_sexp ":yes" [ka a]
    | No a -> pp_sexp ":no" [ka a]

end

module LAME (Vars : AllVars) = struct

  type 't pos_type_cons =
    | Unit
    | Zero
    | Prod of 't * 't
    | Sum of 't * 't
    | Cons of Vars.TyVar.t * 't list
    (*let unit = Unit*)
  let unit_t = Unit
  let zero = Zero
  let prod a b = Prod (a,b)
  let sum a b = Sum (a,b)
  let posvar v = Cons (v, [])
  let postype v args = Cons (v,args)
  let string_of_pos_type_cons k = function
    | Unit -> "unit"
    | Zero -> "zero"
    | Prod (a,b) -> pp_texp "prod" [k a; k b]
    | Sum (a,b) -> pp_texp "sum" [k a; k b]
    | Cons (var,args) -> pp_texp (Vars.TyVar.to_string var) (List.map k args)

  type 't neg_type_cons =
    | Top
    | Bottom
    | Fun of 't * 't
    | Choice of 't * 't
    | Cons of Vars.TyVar.t * 't list
  let top = Top
  let bottom = Bottom
  let func a b = Fun (a,b)
  let choice a b = Choice (a,b)
  let negvar v = Cons (v, [])
  let negtype v args = Cons (v,args)
  let string_of_neg_type_cons k = function
    | Top -> "top"
    | Bottom -> "bottom"
    | Fun (a,b) -> pp_texp "fun" [k a; k b]
    | Choice (a,b) -> pp_texp "choice" [k a; k b]
    | Cons (var,args) -> pp_texp (Vars.TyVar.to_string var) (List.map k args)

  type 'x constructor =
    | Unit
    | Pair of 'x * 'x
    | Fst of 'x
    | Snd of 'x
    | PosCons of Vars.ConsVar.t * 'x list
  let unit = Unit
  let pair a b = Pair (a,b)
  let fst a = Fst a
  let snd b = Snd b
  let poscons c args = PosCons (c,args)
  let string_of_constructor k = function
    | Unit -> ":unit"
    | Pair (x,y) -> pp_sexp ":pair" [k x; k y]
    | Fst x -> pp_sexp ":fst" [k x]
    | Snd y -> pp_sexp ":snd" [k y]
    | PosCons (name, args) -> pp_sexp (Vars.ConsVar.to_string name) (List.map k args)


  type ('x ,'a) destructor =
    | Call of 'x * 'a
    | Yes of 'a
    | No of 'a
    | NegCons of Vars.ConsVar.t * 'x list * 'a
  let call x a = Call (x,a)
  let yes a = Yes a
  let no a = No a
  let negcons c args cont = NegCons (c,args,cont)
  let string_of_destructor kx ka = function
    | Call (x,a) -> pp_sexp ":call" [kx x; ka a]
    | Yes a -> pp_sexp ":yes" [ka a]
    | No a -> pp_sexp ":no" [ka a]
    | NegCons (name, args, a) -> pp_sexp (Vars.ConsVar.to_string name) (ka a :: List.map kx args)

end
