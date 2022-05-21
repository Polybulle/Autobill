open Vars
open Types
open Constructors
open Ast
open Format


val pp_var : formatter -> Var.t -> unit
val pp_tvar : formatter -> TyVar.t -> unit
val pp_cvar : formatter -> ConsVar.t -> unit
val pp_sort : formatter -> sort -> unit
val pp_typ : formatter -> typ -> unit
val pp_constructor : (formatter -> 'a -> unit) -> formatter -> 'a constructor -> unit
val pp_destructor :
  (formatter -> 'a -> unit)
  -> (formatter -> 'b -> unit)
  -> formatter -> ('a, 'b) destructor
  -> unit
val pp_custom_binding :
  prefix:string ->
  suffix:string ->
  formatter ->
  (formatter -> 'a -> unit) ->
  'a -> (formatter -> 'b -> unit) -> 'b option -> unit
val pp_bind : formatter -> Var.t * typ option -> unit
val pp_bind_paren :  formatter -> Var.t * typ option -> unit
val pp_bind_copatt : formatter -> typ option -> unit
val pp_bind_bindcc : formatter -> typ option -> unit
val pp_bind_typ : formatter -> TyVar.t * sort option -> unit
val pp_bind_typ_paren : formatter -> TyVar.t * sort option -> unit
val pp_pattern : formatter -> (Var.t * typ option) constructor -> unit
val pp_copattern : formatter -> (Var.t * typ option, typ option) destructor -> unit
val pp_pol_annot : formatter -> extended_polarity -> unit
val pp_value : formatter -> value -> unit
val pp_stack : formatter -> stack -> unit
val pp_stack_trail : formatter -> stack -> unit
val pp_cmd : formatter -> command -> unit
val pp_typ_lhs :
  formatter -> TyVar.t * (TyVar.t * sort option) list * sort option -> unit
val pp_comma_sep : formatter -> unit -> unit
val pp_data_decl_item : formatter -> typ constructor -> unit
val pp_codata_decl_item : formatter -> (typ, typ) destructor -> unit
val pp_prog_item : formatter -> program_item -> unit
val pp_program : formatter -> program_item list -> unit
