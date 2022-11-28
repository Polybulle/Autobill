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

let have_same_elems xs ys =

  let rec test_and_remove x ys occured acc =
    match ys with
    | [] -> if occured then acc else raise (Failure "")
    | y::ys ->
      if x = y then
        test_and_remove x ys true acc
      else
        test_and_remove x ys occured (y::acc) in

  try
    [] = List.fold_left (fun acc x -> test_and_remove x acc false []) ys xs
  with
  | Failure _ -> false


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