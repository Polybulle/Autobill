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

let dotest prog =
  print_string (string_of_program (parse_string (string_of_program (parse_string prog))))


let%expect_test "Parser roundtrips" =

  let prog = {|
      decl type test : +
      decl type test : -
      type test : + = tvar
      type test a b c (d : +) = (d e (f g h))
      type test = (exp (aff (lin a)))
      type test a = (prod unit (sum zero (choice top (fun bottom a))))
      data test =
        | :test(unit)
      codata test a b =
        | this.mycall(a).ret() : b
      codata test a b =
        | this.myyes().ret() : a
        | this.myno().ret() : b
      cmd test = step+ v into this.ret()
      cmd test = v.ret()
      cmd test = step into this.ret() with v
      term test : t = x
      term test = :mycons()
      term test = :mycons(x, y, z)
      term test = pair(left(unit()), right(unit()))
      term test = box(exp) (ret() : t) -> v.ret()
      term test = box(exp) -> v.ret()
      term test = bind/cc+ (ret() : t) -> v.ret()
      term test = bind/cc -> v.ret()
      term test = match this.cons(x, y, z).ret()-> v.ret()
      term test = match this.cons(x : t, y : u, z : v).ret() : w -> v.ret()
      term test = match
        | this.cons1(x : t, y : u, z : v).ret() : w -> v.ret()
        | this.cons2(x : t, y : u, z : v).ret() : w -> v.ret()
      end
      env test : t = this.ret()
      env test = this.call(x).yes().no().ret()
      env test = this.mycons().ret()
      env test = this.mycons2(x, y, z).ret()
      env test = this.unbox(lin).ret()
      env test = this.bind x -> v.ret()
      env test = this.bind+ (x : t) -> v.ret()
      env test = this.match :cons(x, y, z) -> v.ret()
      env test = this.match
        | :cons1(x : t, y : u, z : v) -> v.ret()
        | :cons2(x : t, y : u, z : v) -> v.ret()
      end
    |}

  in
  dotest prog;
  [%expect

    {|
      decl type test : +
      decl type test : -
      type test : + = tvar
      type test a b c (d : +) = (d e (f g h))
      type test = (exp (aff (lin a)))
      type test a = (prod unit (sum zero (choice top (fun bottom a))))
      data test =
        | :test(unit)
      codata test a b =
        | this.mycall(a).ret() : b
      codata test a b =
        | this.myyes().ret() : a
        | this.myno().ret() : b
      cmd test = step+ v into this.ret()
      cmd test = step v into this.ret()
      cmd test = step v into this.ret()
      term test : t = x
      term test = :mycons()
      term test = :mycons(x, y, z)
      term test = pair(left(unit()), right(unit()))
      term test = box(exp) (ret() : t) -> step v into this.ret()
      term test = box(exp) -> step v into this.ret()
      term test = bind/cc+ (ret() : t) -> step v into this.ret()
      term test = bind/cc -> step v into this.ret()
      term test = match this.cons(x, y, z).ret() -> step v into this.ret()
      term test = match this.cons(x : t, y : u, z : v).ret() : w -> step v into this.ret()
      term test = match | this.cons1(x : t, y : u, z : v).ret() : w -> step v into this.ret() | this.cons2(x : t, y : u, z : v).ret() : w -> step v into this.ret()  end
      env test : t = this.ret()
      env test = this.call(x).yes().no().ret()
      env test = this.mycons().ret()
      env test = this.mycons2(x, y, z).ret()
      env test = this.unbox(lin).ret()
      env test = this.bind x -> step v into this.ret()
      env test = this.bind+ (x : t) -> step v into this.ret()
      env test = this.match :cons(x, y, z) -> step v into this.ret()
      env test = this.match | :cons1(x : t, y : u, z : v) -> step v into this.ret() | :cons2(x : t, y : u, z : v) -> step v into this.ret()  end
 |}

]
