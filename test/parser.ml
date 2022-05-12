open Autobill
open Calculi.PreLAMECalc

let parse file =
  try
    let lexbuf = Lexing.from_channel ~with_positions:true file in
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg -> raise (Failure msg)
  | Parser.Error -> raise (Failure "Internal parser error")

let parse_string str =
  try
    let lexbuf = Lexing.from_string ~with_positions:true str in
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg -> raise (Failure msg)
  | Parser.Error -> raise (Failure "Internal parser error")

let loop ?show:(show=false) str =
  let str2 = program_to_string (parse_string str) in
  if show then Printf.printf "<<<<<<<<\n%s\n========\n%s\n>>>>>>>>\n" str str2;
  str2


let%test_module "Parser roundtrips" = (module struct

  let%expect_test "parsing of positive type declaration" =
    let prog = "decl type test : typ+" in
    print_string (program_to_string (parse_string prog));
    [%expect{| decl type test : typ+ |}]

 let%expect_test "parsing of negative type declaration" =
    let prog = "decl type test : typ-" in
    print_string (program_to_string (parse_string prog));
    [%expect{| decl type test : typ- |}]

 let%expect_test "parsing of type declaration" =
    let prog = "decl type test : typ~" in
    print_string (program_to_string (parse_string prog));
    [%expect{| decl type test : typ~ |}]

end)
