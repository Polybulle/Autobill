open Autobill
open Calculi
open PreLAMECalc
open Util

let var x = (Var.of_string x)
let tvar x = tvar (TyVar.of_string x)

let test1 =
    V.var (var "x") |+|
    S.bind positive (Some(tvar "a")) (var "y") (V.var (var "y") |+| S.ret ())


let test2 =
    V.box linear (Some(tvar "a")) (V.var (var "x") |~| S.ret ())
    |+|
    S.bind positive  (Some(boxed linear (tvar "a"))) (var "y")
      (V.var (var "y") |+| S.box linear (S.ret ()))


let test3 =
    V.bindcc positive (Some(cons (prod (tvar "a") (tvar "b"))))
      ((V.var (var "input"))
       |+|
       (S.case [ pair (var "x", Some (tvar "a")) (var "y", None) |=> (
            (V.cons (pair (V.var (var "y")) (V.var (var "x")))) |+| S.ret ())]))

let test4 =
    V.case [
      call
        (var "x", Some ((cons (sum (tvar "a") (tvar "a")))))
        (Some (tvar "a"))
      |=>
      (V.var (var "x") |+| S.case [
          (fst (var "y", Some (tvar "a"))) |=> ((V.var (var "y")) |+| S.ret ());
          (snd (var "y", Some (tvar "a"))) |=> ((V.var (var "y")) |+| S.ret ())
        ])]

let prog = [
  Cmd_definition {
    name = var "test1";
    typ = None;
    content = test1;
    loc = dummy_pos
  };
  Cmd_definition {
    name = var "test2";
    typ = None;
    content = test2;
    loc = dummy_pos
  };
  Term_definition {
    name = var "test3";
    typ = None;
    content = test3;
    loc = dummy_pos
  };
  Term_definition {
    name = var "test4";
    typ = None;
    content = test4;
    loc = dummy_pos
  }
]

let%expect_test "Printing of programs" =
  let s =
    PrettyPrinter.pp_program Format.str_formatter prog;
    Format.flush_str_formatter () in
  print_string s;
  [%expect{|
    cmd test1 = x.bind+ (y : a) -> y.ret()

    cmd test2 =
      step+
        box(lin) (ret() : a) -> x.ret()
      into
        this.bind+ (y : (lin a)) -> y.unbox(lin).ret()
      end

    term test3 =
      bind/cc+ (ret() : (prod a b)) ->
        input.match
               case pair(x : a, y) -> pair(y, x).ret()
             end

    term test4 =
      match
        case this.call(x : (sum a a)).ret() : a ->
          x.match
             case left(y : a) -> y.ret()
             case right(y : a) -> y.ret()
           end
      end |}]
