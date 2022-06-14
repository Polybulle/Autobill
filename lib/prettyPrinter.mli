open Vars
open Ast
open Format
open FullAst

val pp_typ : formatter -> (TyConsVar.t, TyVar.t) Types.pre_typ -> unit
val pp_value : formatter -> meta_value -> unit
val pp_pre_value : formatter -> pre_value -> unit
val pp_stack : formatter -> meta_stack -> unit
val pp_pre_stack : formatter -> pre_stack -> unit
val pp_stack_trail : formatter -> meta_stack -> unit
val pp_pre_stack_trail : formatter -> pre_stack -> unit
val pp_cmd : formatter -> command -> unit
val pp_tycons_def : formatter -> TyConsVar.t * tycons_definition -> unit
val pp_definition : formatter -> prog_item -> unit
val pp_program : formatter -> prelude * prog_item list -> unit
