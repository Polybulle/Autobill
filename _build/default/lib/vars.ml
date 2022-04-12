let pp_sexp first args =
  let rest = List.fold_left (fun acc s -> acc ^ " " ^ s) "" args in
  Printf.sprintf "(%s%s)" first rest

let pp_texp first args =
  let rest = List.fold_left (fun acc s -> acc ^ " " ^ s) "" args in
  Printf.sprintf "{%s%s}" first rest


module type Sorted = sig
  type t
  val of_string : string -> t
  val to_string : t -> string
  val fresh : unit -> t
end

module type Var = sig
 module Var : Sorted
 module CoVar : Sorted
 module TyVar : Sorted
 module ConsVar : Sorted
end

module StringVar :
  (Var with type Var.t = string
        and type CoVar.t = string
        and type TyVar.t = string
        and type ConsVar.t = string)
= struct
  let _hacky_global_counter = ref 0
  module StringSorted : Sorted with type t = string = struct
    type t = string
    let of_string s = s
    let to_string s = s
    let fresh () =
      let name = "x" ^ (string_of_int !_hacky_global_counter) in name
  end
  module Var = StringSorted
  module CoVar = StringSorted
  module TyVar = StringSorted
  module ConsVar = StringSorted
end
