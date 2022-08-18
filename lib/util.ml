let paren s = Printf.sprintf "(%s)" s

let string_of_tupple k = function
  | [] -> "()"
  | [e] -> paren (k e)
  | e::rest ->
      paren (List.fold_left (fun acc x -> acc ^ ", " ^ k x) (k e) rest)

let rec insert_nodup l x = match l with
  | [] -> [x]
  | h::t ->
    if x = h then l else
    if x < h then (x::h::t)
    else h::(insert_nodup t x)

type position = {
  start_pos : Lexing.position;
  end_pos : Lexing.position;
  is_dummy : bool
}

let position start endd = {
  start_pos = start;
  end_pos = endd;
  is_dummy = false
}

let dummy_pos = {
  start_pos = Lexing.dummy_pos;
  end_pos = Lexing.dummy_pos;
  is_dummy = true
}

let string_of_position p =
  let fname = p.start_pos.pos_fname in
  let lnum = p.start_pos.pos_lnum in
  let st = p.start_pos.pos_cnum - p.start_pos.pos_bol in
  let endd = p.end_pos.pos_cnum - p.end_pos.pos_bol in
  let dummy = if p.is_dummy then "(dummy)" else "" in
  Printf.sprintf "%s:%d:%d-%d%s" fname lnum st endd dummy

module type SubstParam = sig
  type key
  type valu
  val compare_key : key -> key -> int
end

module Subst (Param : SubstParam)= struct

  module M = Map.Make (struct
      type t = Param.key
      let compare = Param.compare_key
    end)

  type t = Param.valu list M.t

  let empty = M.empty

  let get k sub =
    try List.hd (M.find k sub)
    with _ -> raise Not_found

  let push k v sub =
    match M.find_opt k sub with
    | None ->  M.add k [v] sub
    | Some vs -> M.add k (v::vs) sub

  let pop k sub =
    match M.find_opt k sub with
    | None -> raise Not_found
    | Some (_::vs) -> M.add k vs sub
    | Some [] -> raise Not_found

  let push_list kvs sub =
    let aux sub' (k,v) =
      M.update k
            (function
              | None -> Some [v]
              | Some vs -> Some (v::vs))
            sub' in
    List.fold_left aux sub kvs

  let pop_list kvs sub =
    let aux sub' (k,_) = M.update k (fun _ -> None) sub' in
    List.fold_left aux sub kvs

end
