exception Undefined_variable of string

module IntM = Map.Make (struct
    type t = int
    let compare = compare
  end)

  module StrM = Map.Make (struct
    type t = string
    let compare = compare
  end)

(* TODO ajouter un préfixe distinctif *)
module type LocalVarParam = sig
  val default_name : int -> string
end

module LocalVar (Param : LocalVarParam) = struct

  open Param

  type var = int

  type t = var

  module Env = Map.Make (struct
    type t = var
    let compare = compare
  end)

  let names = ref IntM.empty

  let of_string s =
    let v = Global_counter.fresh_int () in
    let s = s ^ "<" ^ (string_of_int v) ^ ">" in
    names := IntM.add v s !names;
    v

  let fresh () =
    let v = Global_counter.fresh_int () in
    of_string (default_name v)

  let to_string v =
    try IntM.find v !names with
    | Not_found -> raise (Undefined_variable (string_of_int v))

end

module Var = LocalVar (struct
    let default_name v = "x" ^ string_of_int v
  end)

module TyVar = LocalVar (struct
    let default_name v = "t" ^ string_of_int v
  end)

module DefVar = LocalVar (struct
    let default_name v = "t" ^ string_of_int v
  end)

module ConsVar = LocalVar (struct
    let default_name v = "cons" ^ string_of_int v
  end)

module DestrVar = LocalVar (struct
    let default_name v = "destr" ^ string_of_int v
  end)

module PolVar = LocalVar (struct
    let default_name v = "pol" ^ string_of_int v
  end)

module SortVar = LocalVar (struct
    let default_name v = "sort" ^ string_of_int v
  end)

module TyConsVar = LocalVar (struct
    let default_name v = "tycons" ^ (string_of_int v)
  end)
