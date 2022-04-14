open Autobill

let cst_of_string s = Parser.prog Lexer.token (Lexing.from_string s)

let string_of_cst x = Cst.to_string x

let tests = ["()"]

let%test _ =
  "(jump foo)" = string_of_cst (cst_of_string "(jump foo)")
