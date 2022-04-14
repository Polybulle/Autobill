type expr =
    Parens of expr list
  | Type of pretype
  | Keyword of string
  | Var of string
  | CoVar of string
  | Cons of string

and pretype =
    Curlies of pretype list
  | Omitted
  | Tvar of string

val concat_args : string list -> string
val to_string : expr -> string
val type_to_string : pretype -> string
