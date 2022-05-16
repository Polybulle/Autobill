let paren s = Printf.sprintf "(%s)" s

let string_of_tupple k = function
  | [] -> "()"
  | [e] -> paren (k e)
  | e::rest ->
      paren (List.fold_left (fun acc x -> acc ^ ", " ^ k x) (k e) rest)
