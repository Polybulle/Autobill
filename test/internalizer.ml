open Lexing
open Autobill
open Intern_prog

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
  Intern_prettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let dotest prog =
  let iter s = s
    |> parse_string
    |> internalize
    |> fun (prelude, prog, _) -> string_of_program (prelude, prog) in
  print_string (iter (iter prog))

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
term test9 = unit()
//term test10 = :cons1()
term test9 = bind/cc ->
  term x = unit() in
  term y = bind/cc ->
    term x = unit() in
    x.ret() in
  x.ret()
|};
  [%expect{|
    decl type test1<8> : (-) -> +

    type test2<9> : + = unit

    type test3<10> (a<24> : +) (b<25> : -) : - = b<25>

    type test4<11> : - = (test3<10> unit top)

    type test5<12> (a<26> : -) : - = test4<11>

    type test6<13> : + = (test1<8> test4<11>)

    data test7<14> =
      case :cons1<2>()
      case :cons2<3>(test2<9>, test6<13>)

    codata test8<15> =
      case this.destr1<1>().ret() : (shift- unit)

    /* constructor "cons1<2>" is :cons1<2>() : test7<14> */

    /* constructor "cons2<3>" is :cons2<3>(test2<9>, test6<13>) : test7<14> */

    /* destructor "destr1<1>" for test8<15> is
      this.destr1<1>().ret() : (shift- unit) */

    term<pol14> test9<2> : <t28> = unit()

    term<pol25> test9<3> : <t43> =
      bind/cc<pol15> (ret() : <t29>) -> unit()
        .bind<pol23> (x<3> : <t32>) ->
          step<pol22>
            bind/cc<pol16> (ret() : <t34>) -> unit()
              .bind<pol18> (x<4> : <t37>) -> x<4>.ret()
          : <t33>
          into
            this.bind<pol21> (y<5> : <t40>) -> x<3>.ret()
          end
     |}]



let%expect_test "intern" =
  dotest
{|
data list (a : +) =
  case :nil
  case :cons(a, (list a))

decl type int : +
|};
  [%expect{|
    data list<17> (a<45> : +) =
      case :nil<6>()
      case :cons<7>(a<45>, (list<17> a<45>))

    /* constructor "nil<6>" is
      forall (a<45> : +). :nil<6>() : (list<17> a<45>) */

    /* constructor "cons<7>" is
      forall (a<45> : +). :cons<7>(a<45>, (list<17> a<45>)) : (list<17> a<45>) */
     |}]
