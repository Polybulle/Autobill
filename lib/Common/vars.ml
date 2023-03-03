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
  val of_primitive : string -> t
  val is_primitive : t -> bool
  val fresh : unit -> t
  val _debug_of_int : int -> t
  val _debug_to_int : t -> int
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end

module LocalVar (Param : LocalVarParam) : LocalVar = struct

  (* Creating variables with of_string is idempotent, and is the identity with
     of_primitive, as in: *)
  (* to_string (of_string s) != s *)
  (* to_string (of_primitive s) == s *)
  (* to_string (of_string s) == to_string (of_string (to_string (of_string s))) *)

  open Param
  type t = int

  module Env = Map.Make (struct
    type t = int
    let compare = compare
  end)

  let names : string IntM.t ref = ref IntM.empty
  let prims : int StrM.t ref = ref StrM.empty


  let to_string v =
    match IntM.find_opt v !names with
    | None -> raise (Undefined_variable (string_of_int v))
    | Some s -> s

  let _of_string s =
    let v = Global_counter.fresh_int () in
    let s = s ^ "__" ^ (string_of_int v) in
    names := IntM.add v s !names;
    v

  let of_string s = match StrM.find_opt s !prims with
    | None -> _of_string s
    | Some v -> v

  let of_primitive s =
    let v = Global_counter.fresh_int () in
    names := IntM.add v s !names;
    prims := StrM.add s v !prims;
    v

  and is_primitive v = StrM.mem (to_string v) !prims

  let fresh () = _of_string default_name

  let pp fmt v = Format.pp_print_string fmt (to_string v)

  let _debug_of_int n =
    if not (IntM.mem n !names) then
      names := IntM.add n default_name !names;
    n

  let _debug_to_int v = v

  end

module Var = LocalVar (struct
    let default_name = "x"
  end)

module CoVar = LocalVar (struct
    let default_name = "a"
  end)

module TyVar = LocalVar (struct
    let default_name = "T"
  end)

module DefVar = LocalVar (struct
    let default_name = "def"
  end)

module ConsVar = LocalVar (struct
    let default_name = "cons"
  end)

module DestrVar = ConsVar

module SortVar = LocalVar (struct
    let default_name = "sort"
  end)

module TyConsVar = LocalVar (struct
    let default_name = "Tycons"
  end)

module RelVar = LocalVar (struct
    let default_name = "rel"
  end)
