(** Definition of all type constructors. All extensions to the IR are
    implemented as new constructors. Remember to extend types are well ! *)

type ('var, 't) type_cons =
    Unit
  | Zero
  | Top
  | Bottom
  | Prod of 't * 't
  | Sum of 't * 't
  | Fun of 't * 't
  | Choice of 't * 't
  | Cons of 'var * 't list

val unit_t : ('var, 'a) type_cons
val zero : ('var, 'a) type_cons
val top : ('var, 'a) type_cons
val bottom : ('var, 'a) type_cons
val prod : 'a -> 'a -> ('var, 'a) type_cons
val sum : 'a -> 'a -> ('var, 'a) type_cons
val func : 'a -> 'a -> ('var, 'a) type_cons
val choice : 'a -> 'a -> ('var, 'a) type_cons
val typecons : 'var -> 'a list -> ('var, 'a) type_cons

val string_of_type_cons : ('var -> string) -> ('a -> string) -> ('var, 'a) type_cons -> string


type ('var, 'x) constructor =
  | Unit
  | Pair of 'x * 'x
  | Left of 'x
  | Right of 'x
  | PosCons of 'var * 'x list

val unit : ('var, 'x) constructor
val pair : 'x -> 'x -> ('var, 'x) constructor
val left : 'x -> ('var, 'x) constructor
val right : 'x -> ('var, 'x) constructor
val poscons : 'var -> 'x list -> ('var, 'x) constructor

val string_of_constructor : ('var -> string) -> ('x -> string) -> ('var, 'x) constructor -> string
val consvar_of_constructor : ('var, 'x) constructor -> 'var option

type ('var, 'x ,'a) destructor =
  | Call of 'x * 'a
  | Yes of 'a
  | No of 'a
  | NegCons of 'var * 'x list * 'a

val call : 'x -> 'a -> ('var, 'x, 'a) destructor
val yes : 'a -> ('var, 'x, 'a) destructor
val no : 'a -> ('var, 'x, 'a) destructor
val negcons : 'var -> 'x list -> 'a -> ('var, 'x, 'a) destructor

val string_of_destructor :
  ('var -> string) ->
  ('x -> string) ->
  ('a -> string) ->
  ('var, 'x, 'a) destructor ->
  string
val destrvar_of_destructor : ('var, 'x, 'a) destructor -> 'var option
(* val definition_of_destructor : ('a -> string) -> ('a, 'a) destructor -> string *)
