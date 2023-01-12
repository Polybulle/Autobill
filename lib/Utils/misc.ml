open Format

let pp_tupple_or_item pp_item fmt items = match items with
  | [] -> pp_print_string fmt ""
  | [x] -> pp_item fmt x
  | _ ->
    fprintf fmt "(%a)"
    (pp_print_list ~pp_sep:(fun fmt () -> pp_print_string fmt ", ") pp_item) items

let rec insert_nodup l x = match l with
  | [] -> [x]
  | h::t ->
    if x = h then l else
    if x < h then (x::h::t)
    else h::(insert_nodup t x)

let list_take l n =
  let rec go l acc n = match l, n with
    | l, 0 -> List.rev acc, l
    | [], _ -> raise (Failure "list too short")
    | x::xs, m -> go xs (x::acc) (m-1) in
  go l [] n

let is_sublist xs ys =

  let rec test_and_remove x ys occured acc =
    match ys with
    | [] -> if occured then acc else raise (Failure "")
    | y::ys ->
      if x = y then
        test_and_remove x ys true acc
      else
        test_and_remove x ys occured (y::acc) in

  try
    ignore (List.fold_left (fun acc x -> test_and_remove x acc false []) ys xs);
    true
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
  if p.is_dummy then
    "(no-position)"
  else
    Printf.sprintf "%s:%d:%d-%d" fname lnum st endd
