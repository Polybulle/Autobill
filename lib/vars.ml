module type ISorted = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val fresh : unit -> t
end

module type Parameters = sig
  val default_name : string
  val prefix : string
  val suffix : string
end

module Sorted (P : Parameters) : ISorted = struct
    type t = string
    let _hacky_global_counter = ref 0
    let of_string s = s
    let to_string s = P.prefix ^ s ^ P.suffix
    let fresh () =
      let name = P.default_name ^ (string_of_int !_hacky_global_counter) in name
  end

module Var = Sorted (struct
    let default_name = "x"
    let prefix = ""
    let suffix = ""
  end)

module CoVar = Sorted (struct
    let default_name = "x"
    let prefix = "$"
    let suffix = ""
  end)

module TyVar = Sorted (struct
    let default_name = "t"
    let prefix = ""
    let suffix = ""
  end)

module ConsVar = Sorted (struct
    let default_name = "cons"
    let prefix = ":"
    let suffix = ""
  end)

module DestrVar = Sorted (struct
    let default_name = "destr"
    let prefix = "it."
    let suffix = ""
  end)

module PolVar = Sorted (struct
    let default_name = "p"
    let prefix = "["
    let suffix = "]"
  end)
