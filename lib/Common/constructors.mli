(** Definition of all type constructors. All extensions to the IR are
    implemented as new constructors. Remember to extend types are well ! *)

val type_cons_names : string list

type ('var, 't) type_cons =
    Unit
  | Zero
  | Top
  | Bottom
  | ShiftPos of 't
  | ShiftNeg of 't
  | Prod of 't list
  | Sum of 't list
  | Fun of 't list * 't
  | Choice of 't list
  | Cons of 'var * 't list

val unit_t : ('var, 'a) type_cons
val zero : ('var, 'a) type_cons
val top : ('var, 'a) type_cons
val bottom : ('var, 'a) type_cons
val shift_pos_t : 'a -> ('var, 'a) type_cons
val shift_neg_t : 'a -> ('var, 'a) type_cons
val prod : 'a -> 'a -> ('var, 'a) type_cons
val sum : 'a -> 'a -> ('var, 'a) type_cons
val func : 'a -> 'a -> ('var, 'a) type_cons
val choice : 'a -> 'a -> ('var, 'a) type_cons
val typecons : 'var -> 'a list -> ('var, 'a) type_cons

val string_of_type_cons : ('var -> string) -> ('a -> string) -> ('var, 'a) type_cons -> string


val cons_names : string list

type ('var, 'x) constructor =
  | Unit
  | ShiftPos of 'x
  | Tupple of 'x list
  | Inj of int * int * 'x
  | PosCons of 'var * 'x list

val unit : ('var, 'x) constructor
val shift_pos : 'x -> ('var, 'x) constructor
val pair : 'x -> 'x -> ('var, 'x) constructor
val left : 'x -> ('var, 'x) constructor
val right : 'x -> ('var, 'x) constructor
val poscons : 'var -> 'x list -> ('var, 'x) constructor

val string_of_constructor : ('var -> string) -> ('x -> string) -> ('var, 'x) constructor -> string
val consvar_of_constructor : ('var, 'x) constructor -> 'var option


val destr_names : string list

type ('var, 'x ,'a) destructor =
  | Call of 'x list * 'a
  | Proj of int * int * 'a
  | ShiftNeg of 'a
  | NegCons of 'var * 'x list * 'a

val call : 'x -> 'a -> ('var, 'x, 'a) destructor
val yes : 'a -> ('var, 'x, 'a) destructor
val no : 'a -> ('var, 'x, 'a) destructor
val shift_neg : 'a -> ('var, 'x, 'a) destructor
val negcons : 'var -> 'x list -> 'a -> ('var, 'x, 'a) destructor

val string_of_destructor :
  ('var -> string) ->
  ('x -> string) ->
  ('a -> string) ->
  ('var, 'x, 'a) destructor ->
  string
val destrvar_of_destructor : ('var, 'x, 'a) destructor -> 'var option
(* val definition_of_destructor : ('a -> string) -> ('a, 'a) destructor -> string *)
