open Autobill
open Lexing

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
  CstPrettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let dotest prog =
  let iter s = string_of_program (parse_string s) in
  print_string (iter (iter prog))

let%expect_test "Parser roundtrips" =

  let prog = {|
      decl type test : +
      decl type test : -
      type test : + = tvar
      type test a b c (d : +) = (d e (f g h))
      type test = (exp (aff (lin a)))
      type test a = (prod unit (sum zero (choice top (fun bottom a))))
      data test =
        case :test(unit)
      codata test a b =
        case this.mycall(a).ret() : b
      codata test a b =
        case this.myyes().ret() : a
        case this.myno().ret() : b
      cmd test = step+ v into this.ret() end
      cmd test = v.ret()
      cmd test = step into this.ret() with v end
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
        case this.cons1(x : t, y : u, z : v).ret() : w -> v.ret()
        case this.cons2(x : t, y : u, z : v).ret() : w -> v.ret()
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
        case :cons1(x : t, y : u, z : v) -> v.ret()
        case :cons2(x : t, y : u, z : v) -> v.ret()
      end
      term test = fun (x : t) -> v
      term test = box(lin) v
      cmd test = match :cons(x,y,z) = v in v.ret()
      cmd test = match env this.cons(x,y,z).ret() in v.ret()
      cmd test = term x = v in v.ret()
      cmd test = env this.ret() in v.ret()
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
        case :test(unit)

      codata test a b =
        case this.mycall(a).ret() : b

      codata test a b =
        case this.myyes().ret() : a
        case this.myno().ret() : b

      cmd test = v.ret()

      cmd test = v.ret()

      cmd test = v.ret()

      term test : t = x

      term test = :mycons()

      term test = :mycons(x, y, z)

      term test = pair(left(unit()), right(unit()))

      term test = box(exp) (ret() : t) -> v.ret()

      term test = box(exp) -> v.ret()

      term test = bind/cc+ (ret() : t) -> v.ret()

      term test = bind/cc -> v.ret()

      term test = match
                    case this.cons(x, y, z).ret() -> v.ret()
                  end

      term test = match
                    case this.cons(x : t, y : u, z : v).ret() : w -> v.ret()
                  end

      term test =
        match
          case this.cons1(x : t, y : u, z : v).ret() : w -> v.ret()
          case this.cons2(x : t, y : u, z : v).ret() : w -> v.ret()
        end

      env test : t = this.ret()

      env test = this.call(x).yes().no().ret()

      env test = this.mycons().ret()

      env test = this.mycons2(x, y, z).ret()

      env test = this.unbox(lin).ret()

      env test = this.bind x -> v.ret()

      env test = this.bind+ (x : t) -> v.ret()

      env test = this.match
                       case :cons(x, y, z) -> v.ret()
                     end

      env test =
        this.match
              case :cons1(x : t, y : u, z : v) -> v.ret()
              case :cons2(x : t, y : u, z : v) -> v.ret()
            end

      term test = match
                    case this.call(x : t).ret() -> v.ret()
                  end

      term test = box(lin) -> v.ret()

      cmd test = v.match
                    case :cons(x, y, z) -> v.ret()
                  end

      cmd test =
        step-
          match
            case this.cons(x, y, z).ret() -> v.ret()
          end
        into
          this.ret()
        end

      cmd test = v.bind x -> v.ret()

      cmd test = step
                   bind/cc -> v.ret()
                 into
                   this.ret()
                 end
 |}

]
