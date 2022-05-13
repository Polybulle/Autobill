open Autobill
open Calculi.PreLAMECalc

let parse lexbuf =
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg -> raise (Failure msg)
  | Parser.Error -> raise (Failure "Internal parser error")

let parse_string str =
  let lexbuf = Lexing.from_string ~with_positions:true str in
  parse lexbuf

let loop ?show:(show=false) str =
  let str2 = program_to_string (parse_string str) in
  if show then Printf.printf "<<<<<<<<\n%s\n========\n%s\n>>>>>>>>\n" str str2;
  str2


let%test_module "Parser roundtrips" = (module struct

  let dotest prog = print_string (program_to_string (parse_string prog))

  let%expect_test "parsing of positive type declaration" =

    let prog = "decl type test : typ+" in
    dotest prog;
    [%expect{| decl type test : typ+ |}]

 let%expect_test "parsing of negative type declaration" =

    let prog = "decl type test : typ-" in
    dotest prog;
    [%expect{| decl type test : typ- |}]

 let%expect_test "parsing of type declaration" =

    let prog = "decl type test : typ~" in
    dotest prog;
    [%expect{| decl type test : typ~ |}]

 let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = +tvar" in
     dotest prog;
     [%expect{| type test : typ~ = +tvar |}]

 let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = -tvar" in
     dotest prog;
     [%expect{| type test : typ~ = -tvar |}]

let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = tvar" in
     dotest prog;
     [%expect{| type test : typ~ = tvar |}]

let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = +{box lin tvar}" in
     dotest prog;
     [%expect{| type test : typ~ = +{box lin tvar} |}]

let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = +{box aff tvar}" in
     dotest prog;
     [%expect{| type test : typ~ = +{box aff tvar} |}]

let%expect_test "parsing of type definition" =

   let prog = "type test : typ~ = +{box exp tvar}" in
     dotest prog;
     [%expect{| type test : typ~ = +{box exp tvar} |}]


end)
