open Lexing
open Autobill
open Intern_prelude

let pos_of_error lexbuf =
  Printf.sprintf "%d:%d"
      lexbuf.lex_curr_p.pos_lnum
      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let parse lexbuf =
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^  msg))
  | Parser.Error ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^ " syntax error"))

let parse_string str =
  let lexbuf = Lexing.from_string ~with_positions:true str in
  Lexing.set_filename lexbuf "<internal>";
  parse lexbuf

let string_of_program prog =
  PrettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let dotest prog =
  let iter s = s
    |> parse_string
    |> internalize_prelude
    |> fun (_,prelude,_) -> string_of_program (prelude, []) in
  print_string (iter prog)

let%expect_test "intern" =
  dotest
{|
decl type test1 : (-) -> +
type test2 : + = unit
type test3 (a : +) (b : -) : - = b
type test4 : - = (test3 unit top)
type test5 (a : -) : - = test4
type test6 : + = (test1 test4)
data test7 =
  case :cons1
  case :cons2(test2, test6)
codata test8 =
  case this.destr1().ret() : (shift- unit)
|};
  [%expect{|
    decl type test1 : (-) -> +

    type test2 : + = unit

    type test3 (a : +) (b : -) : - = b

    type test4 : - = (test3 unit top)

    type test5 (a : -) : - = test4

    type test6 : + = (test1 test4)

    data test7 : + =
      case :cons1()
      case :cons2(test2, test6)

    codata test8 : - =
      case this.destr1().ret() : (shift- unit)

    //constructor "cons1" is :cons1() : test7

    //constructor "cons2" is :cons2(test2, test6) : test7

    //destructor "destr1" is .destr1().ret((shift- unit)) : test8 |}]