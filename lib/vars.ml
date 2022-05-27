exception Unregistered_variable of string

exception Undefined_variable of string

exception Redefined_variable of string

module IntM = Map.Make (struct
    type t = int
    let compare = compare
  end)

  module StrM = Map.Make (struct
    type t = string
    let compare = compare
  end)


module type LocalVarParam = sig
  val default_name : int -> string
  val print_var : string -> string
end

module LocalVar (Param : LocalVarParam) = struct

  open Param

  type t = int

  let hacky_global_counter = ref 0
  let names = ref IntM.empty

  let fresh () =
    let v = !hacky_global_counter in
    let s = default_name v in
    names := IntM.add v s !names;
    incr hacky_global_counter;
    v

  let of_string s =
    let v = !hacky_global_counter in
    names := IntM.add v s !names;
    incr hacky_global_counter;
    v

  let to_string v =
    try print_var (IntM.find v !names) with
    | Not_found -> raise (Undefined_variable (string_of_int v))

end


module type RegisterVarParam = sig
  val default_name : int -> string
  val print_var : string -> string
end

module RegisterVar (Param : RegisterVarParam) = struct

  open Param

  type t = int

  let hacky_global_counter = ref 0

  let conses : string IntM.t ref = ref IntM.empty
  let registered : int StrM.t ref = ref StrM.empty

  let register s =
    match StrM.find_opt s !registered with
    | Some _ -> raise (Redefined_variable s)
    | None ->
      let v = !hacky_global_counter in
      incr hacky_global_counter;
      conses := IntM.add v s !conses;
      registered := StrM.add s v !registered;
      v

  let fresh () =
    let v = !hacky_global_counter in
    let s = default_name v in
    incr hacky_global_counter;
    conses := IntM.add v s !conses;
    registered := StrM.add s v !registered;
    v

  let of_string s =
    try StrM.find s !registered with
    | Not_found -> raise (Unregistered_variable s)

  let to_string s =
    try print_var (IntM.find s !conses) with
    | Not_found -> raise (Undefined_variable (string_of_int s))

end


module Var = LocalVar (struct
    let default_name v = "x" ^ string_of_int v
    let print_var s = s
  end)

module TyVar = LocalVar (struct
    let default_name v = "t" ^ string_of_int v
    let print_var s = s
  end)

module DefVar = LocalVar (struct
    let default_name v = "t" ^ string_of_int v
    let print_var s = s
  end)

module ConsVar = RegisterVar (struct
    let default_name v = "cons" ^ string_of_int v
    let print_var s = s
  end)

module DestrVar = RegisterVar (struct
    let default_name v = "destr" ^ string_of_int v
    let print_var s = s
  end)

module PolVar = struct
  type t = int
  let _hacky_global_counter = ref 0
  let fresh () =
    let v = !_hacky_global_counter in
    incr _hacky_global_counter;
    v
  let to_string n =
    "pol" ^ (string_of_int n)
end

module SortVar = struct
  type t = int
  let _hacky_global_counter = ref 0
  let fresh () =
    let v = !_hacky_global_counter in
    incr _hacky_global_counter;
    v
  let to_string n =
    "sort" ^ (string_of_int n)
end
