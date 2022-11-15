exception Undefined_variable of string

module IntM = Map.Make (struct
    type t = int
    let compare = compare
  end)

  module StrM = Map.Make (struct
    type t = string
    let compare = compare
  end)

module type LocalVarParam = sig
  val default_name :  string
end

module type LocalVar = sig
  type t
  module Env : Map.S with type key = t
  val of_string : string -> t
  val fresh : unit -> t
  val to_string : t -> string
end

module LocalVar (Param : LocalVarParam) : LocalVar = struct

  open Param
  type t = int

  module Env = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let names = ref IntM.empty
  let of_string s =
    let s =
      try Str.replace_first (Str.regexp {|\([a-zA-Z0-9_]+\)__[0-9]+|}) {|\1|} s
      with _ -> s in
    let v = Global_counter.fresh_int () in
    let s = s ^ "__" ^ (string_of_int v)  in
    names := IntM.add v s !names;
    v

  let fresh () = of_string default_name

  let to_string v =
    try IntM.find v !names with
    | Not_found -> raise (Undefined_variable (string_of_int v))

end

module Var = LocalVar (struct
    let default_name = "x"
  end)

module CoVar = LocalVar (struct
    let default_name = "a"
  end)

module TyVar = LocalVar (struct
    let default_name = "t"
  end)

module DefVar = LocalVar (struct
    let default_name = "def"
  end)

module ConsVar = LocalVar (struct
    let default_name = "cons"
  end)

module DestrVar = LocalVar (struct
    let default_name = "destr"
  end)

module SortVar = LocalVar (struct
    let default_name = "sort"
  end)

module TyConsVar = LocalVar (struct
    let default_name = "tycons"
  end)
