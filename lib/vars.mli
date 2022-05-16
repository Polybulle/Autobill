module type ISorted =
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

module Var : ISorted
module CoVar : ISorted
module TyVar : ISorted
module ConsVar : ISorted
module DestrVar : ISorted
module PolVar : ISorted
