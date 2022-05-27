val paren : string -> string
val string_of_tupple : ('a -> string) -> 'a list -> string
type position = {
  start_pos : Lexing.position;
  end_pos : Lexing.position;
  is_dummy : bool;
}
val position : Lexing.position -> Lexing.position -> position
val dummy_pos : position
val string_of_position : position -> string

module type SubstParam = sig
  type key
  type valu
  val compare_key : key -> key -> int
end

module Subst (Param : SubstParam) : sig

  type t
  val empty : t
  val get : Param.key -> t -> Param.valu
  val push : Param.key -> Param.valu -> t -> t
  val push_list : (Param.key * Param.valu) list -> t -> t
  val pop : Param.key -> t -> t
  val pop_list : (Param.key * Param.valu) list -> t -> t

end
