open Constructors
open Cst
open Format


val pp_var : formatter -> var -> unit
val pp_tyvar : formatter -> tyvar -> unit
val pp_consvar : formatter -> consvar -> unit
val pp_destrvar : formatter -> destrvar -> unit
val pp_sort : formatter -> sort -> unit
val pp_typ : formatter -> typ -> unit

val pp_constructor :
  (formatter -> 'var -> unit)
  -> (formatter -> 'a -> unit)
  -> formatter
  -> ('var, 'a) constructor
  -> unit
val pp_destructor :
  (formatter -> 'var -> unit)
  -> (formatter -> 'a -> unit)
  -> (formatter -> 'b -> unit)
  -> formatter -> ('var, 'a, 'b) destructor
  -> unit

val pp_value : formatter -> value -> unit
val pp_stack : formatter -> stack -> unit
val pp_stack_trail : formatter -> stack -> unit
val pp_cmd : formatter -> command -> unit
val pp_program : formatter -> program_item list -> unit
