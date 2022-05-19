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
