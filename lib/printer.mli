open Ast

val string_of_value : value -> string
val string_of_stack : stack -> string
val string_of_command : command -> string
val string_of_prog_item : program_item -> string
val string_of_program : program_item list -> string
val string_of_program_with_debug : program_item list -> string
