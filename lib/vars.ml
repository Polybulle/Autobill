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

let _hacky_global_counter = ref 0

module type Prefix = sig
  val it : string
end

module StringSorted (Prefix : Prefix) : (Sorted with type t = string) = struct
    type t = string
    let of_string s = s
    let to_string s =Prefix.it ^ s
    let fresh () =
      let name = "x" ^ (string_of_int !_hacky_global_counter) in name
  end

module Var = StringSorted (struct let it = "" end)
module CoVar = StringSorted (struct let it = "!" end)
module TyVar = StringSorted (struct let it = "" end)
module ConsVar = StringSorted (struct let it = ":" end)
module SoVar = StringSorted (struct let it = "#" end)
