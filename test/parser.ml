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

let%test_module "Parser roundtrips" = (module struct

  let dotest prog = print_string (program_to_string (parse_string prog))

  let%expect_test "parsing of type definition" =

    let prog =
      {|
        decl type test : [+];
        decl type test : [-];
        decl type test : [~];
        type test : [+] = +tvar;
        type {test +a -b ~c {d [+]}} : [+] = +tvar;
        type test : [~] = {+box lin {+box aff {+box exp ~tvar}}};
        type {~test +a} = {+prod +unit {+sum +zero {-choice -top {-fun -bottom +a}}}};
      |} in
    dotest prog;
    [%expect{|
      decl type test : [+];

      decl type test : [-];

      decl type test : [~];

      type test : [+] = +tvar;

      type {test {a [+]} {b [-]} {c [~]} {d [+]}} : [+] = +tvar;

      type test : [~] = {+box lin {+box aff {+box exp ~tvar}}};

      type {test {a [+]}} : [~] = {+prod +unit {+sum +zero {-choice -top {-fun -bottom +a}}}}
    |}]

  let%expect_test "parsing data definition" =

     let prog =
      {|

        data {+list +a} =
          | :nil
          | :cons of +a * {+list +a};
      |} in
    dotest prog;
    [%expect{|

    |}]

end)
