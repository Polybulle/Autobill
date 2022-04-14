
(* Jetons: *)
(* - mots clés: (  ) {  } jump call force let box unbox lin aff exp match *)
(* - variables: !a x :k Tvar *)

type expr =
  | Parens of expr list
  | Type of pretype
  | Keyword of string
  | Var of string
  | CoVar of string
  | Cons of string
and pretype =
  | Curlies of pretype list
  | Omitted
  | Tvar of string

let concat_args xs = List.fold_left (fun acc x -> acc ^ " " ^ x) "" xs

let rec to_string = function
  | Parens [] -> "()"
  | Parens (h::t) ->
    Printf.sprintf "(%s%s)" (to_string h) (concat_args (List.map to_string t))
  | Type Omitted -> ""
  | Type t -> type_to_string t
  | Keyword s -> s
  | Var s -> s
  | CoVar s -> "!" ^ s
  | Cons s -> ":" ^ s
and type_to_string = function
  | Curlies [] -> "{}"
  | Curlies (h::t) ->
    Printf.sprintf "{%s%s}" (type_to_string h) (concat_args (List.map type_to_string t))
  | Tvar s -> s
  | Omitted -> "_"
