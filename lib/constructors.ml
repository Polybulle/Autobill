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


module ILL = struct

  type 't pos_type_cons =
    | Unit
    | Zero
    | Prod of 't * 't
    | Sum of 't * 't
  let prod a b = Prod (a,b)
  let sum a b = Sum (a,b)
  let unit = Unit
  let zero = Zero

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
  let func a b = Fun (a,b)
  let choice a b = Choice (a,b)
  let top = Top
  let bottom = Bottom

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
    | Unit -> "unit"
    | Pair (x,y) -> pp_sexp "pair" [k x; k y]
    | Fst x -> pp_sexp "fst" [k x]
    | Snd y -> pp_sexp "snd" [k y]

  type ('x ,'a) destructor =
    | Call of 'x * 'a
    | Yes of 'a
    | No of 'a
  let call x a = Call (x,a)
  let yes a = Yes a
  let no a = No a

  let string_of_destructor kx ka = function
    | Call (x,a) -> pp_sexp "call" [kx x; ka a]
    | Yes a -> pp_sexp "yes" [ka a]
    | No a -> pp_sexp "no" [ka a]

end
