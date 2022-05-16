open Vars

type 'x constructor
type ('x, 'a) destructor

val unit : 'a constructor
val pair : 'a -> 'a -> 'a constructor
val fst : 'a -> 'a constructor
val snd : 'a -> 'a constructor
val poscons : ConsVar.t -> 'a list -> 'a constructor
val call : 'a -> 'b -> ('a, 'b) destructor
val yes : 'a -> ('b, 'a) destructor
val no : 'a -> ('b, 'a) destructor
val negcons : ConsVar.t -> 'a list -> 'b -> ('a, 'b) destructor

val string_of_constructor : ('a -> string) -> 'a constructor -> string
val definition_of_constructor : ('a -> string) -> 'a constructor -> string
val string_of_destructor : ('a -> string) -> ('b -> string) -> ('a, 'b) destructor -> string
val definition_of_destructor : ('a -> string) -> ('a, 'a) destructor -> string
