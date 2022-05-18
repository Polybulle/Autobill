module type ISorted =
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

module Var : ISorted
module TyVar : ISorted
module ConsVar : ISorted
module DestrVar : ISorted
module PolVar : ISorted

val hack_destrvar_to_string : ConsVar.t -> string
