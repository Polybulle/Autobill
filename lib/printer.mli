open Vars
open Types
open Constructors
open Ast

val string_of_binding : Var.t -> typ option -> string
val string_of_cobinding : typ option -> string
val string_of_pattern : (Var.t * typ option) constructor -> string
val string_of_copattern : (Var.t * typ option, typ option) destructor -> string
val string_of_value : value -> string
val string_of_stack : stack -> string
val string_of_command : command -> string
val string_of_prog_item : program_item -> string
val string_of_program : program_item list -> string
