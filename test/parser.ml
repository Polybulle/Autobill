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
        type {test +a -b ~c d [+]} : [+] = +tvar;
        type test : [~] = {+box lin {+box aff {+box exp ~tvar}}};
        type {~test +a} = {+prod +unit {+sum +zero {-choice -top {-fun -bottom +a}}}};
      |} in
    dotest prog;
    [%expect{|
      decl type test : [+];

      decl type test : [-];

      decl type test : [~];

      type test : [+] = +tvar;

      type {test a [+] b [-] c [~] d [+]} : [+] = +tvar;

      type test : [~] = {+box lin {+box aff {+box exp ~tvar}}};

      type {test a [+]} : [~] = {+prod +unit {+sum +zero {-choice -top {-fun -bottom +a}}}}
    |}]

  let%expect_test "parsing data definition" =

    let prog = {|
      data +test =
        | :test of +unit ;
      codata {-myfun ~a ~b} =
        | :mycall of ~a cont ~b;
      codata {-mychoice ~a ~b} =
        | :myyes cont ~a
        | :myno cont ~b;
    |} in
    dotest prog;
    [%expect{|
      data test =
        | :test of +unit;

      codata {myfun a [~] b [~]} =
        | :mycall of ~a cont ~b;

      codata {mychoice a [~] b [~]} =
        | :myyes cont ~a
        | :myno cont ~b

    |}]

  let%expect_test "parsing commands" =

    let prog = {|
     cmd test = (jump x !a);
     cmd test = (enter x (let y +unit (jump y !a)))
    |} in
    dotest prog;
    [%expect{|
      cmd test = (jump x !a);

      cmd test = (enter x (let y {+unit} (jump y !a))) |}]

  let%expect_test "parsing co-matches" =

    let prog = {|
     term id = (match
       (:call !a x) (jump x !a)
     );
    |} in
    dotest prog;
    [%expect{|
      term id : _omitted = (match (:call x {_omitted} !a {_omitted}) (jump x !a))


    |}]

let%expect_test "parsing matches" =

    let prog = {|
     env !test =
      (let x +test (jump (let !a +test (jump test !a))
       (match
         (:left x) (jump x !a)
         (:right y) (jump y !a)
     )));
    |} in
    dotest prog;
    [%expect{|
      env test : _omitted = (let x {+test} (jump (let !a {+test} (jump test !a)) (match (:fst x {_omitted}) (jump x !a) (:snd y {_omitted}) (jump y !a))))
    |}]

let%expect_test "parsing values" =

    let prog = {|
      term test = x;
      term test = :unit;
      term test = (:pair (:left x) (:right y));
      term test = :mycons;
      term test = (:mycons);
      term test = (:mycons2 x y z);
      term test = (box lin !a +unit (jump :unit !a));
      term test = (let !a -top (jump x !a));
      term test = (force !a +unit (jump :unit !a))
    |} in
    dotest prog;
    [%expect {|
      term test : _omitted = x;

      term test : _omitted = :unit;

      term test : _omitted = (:pair (:fst x) (:snd y));

      term test : _omitted = (:mycons);

      term test : _omitted = (:mycons);

      term test : _omitted = (:mycons2 x y z);

      term test : _omitted = (box lin !a {+unit} (jump :unit !a));

      term test : _omitted = (let !a {-top} (jump x !a));

      term test : _omitted = (force !a {+unit} (jump :unit !a)) |}]


let%expect_test "parsing stacks" =

    let prog = {|
      env !test = !x;
      env !test = (:yes (:no !a));
      env !test = (:mycons !a);
      env !test = (:mycons2 !a x y z);
      env !test = (unbox lin !a);
      env !test = (let x +unit (jump x !a));
      env !test = (force x -top (jump x !a))
    |} in
    dotest prog;
    [%expect {|
      env test : _omitted = !x;

      env test : _omitted = (:yes (:no !a));

      env test : _omitted = (:mycons !a);

      env test : _omitted = (:mycons2 !a x y z);

      env test : _omitted = (unbox lin !a);

      env test : _omitted = (let x {+unit} (jump x !a));

      env test : _omitted = (force x {-top} (jump x !a)) |}]



end)
