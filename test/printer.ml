open Autobill
open Calculi
open PreLAMECalc

let var x = (Var.of_string x)
let tvar x = tvar (TyVar.of_string x)

let test1 =
    V.var (var "x") |+|
    S.bind positive (Some(tvar "a")) (var "y") (V.var (var "y") |+| S.ret)


let test2 =
    V.box linear (Some(tvar "a")) (V.var (var "x") |~| S.ret)
    |+|
    S.bind positive  (Some(boxed linear (tvar "a"))) (var "y")
      (V.var (var "y") |+| S.box linear S.ret)


let test3 =
    V.bindcc positive (Some(cons (prod (tvar "a") (tvar "b"))))
      ((V.var (var "input"))
       |+|
       (S.case [ pair (var "x", Some (tvar "a")) (var "y", None) |=> (
            (V.cons (pair (V.var (var "y")) (V.var (var "x")))) |+| S.ret)]))

let test4 =
    V.case [
      call
        (var "x", Some ((cons (sum (tvar "a") (tvar "a")))))
        (Some (tvar "a"))
      |=>
      (V.var (var "x") |+| S.case [
          (fst (var "y", Some (tvar "a"))) |=> ((V.var (var "y")) |+| S.ret);
          (snd (var "y", Some (tvar "a"))) |=> ((V.var (var "y")) |+| S.ret)
        ])]

let prog = [
  Cmd_definition {
    name = var "test1";
    typ = None;
    content = test1
  };
  Cmd_definition {
    name = var "test2";
    typ = None;
    content = test2
  };
  Term_definition {
    name = var "test3";
    typ = None;
    content = test3
  };
  Term_definition {
    name = var "test4";
    typ = None;
    content = test4
  }
]

let%expect_test "Printing of programs" =
  print_string (Printer.string_of_program prog);
  [%expect{|
    cmd test1 = step+ x into this.bind+ (y : a) -> step+ y into this.ret()
    cmd test2 = step+ box(lin) (ret() : a) -> step x into this.ret() into this.bind+ (y : (lin a)) -> step+ y into this.unbox(lin).ret()
    term test3 = bind/cc+ (ret() : (prod a b)) -> step+ input into this.match pair(x : a, y) -> step+ pair(y, x) into this.ret()
    term test4 = match this.call(x : (sum a a)).ret() : a -> step+ x into this.match | left(y : a) -> step+ y into this.ret() | right(y : a) -> step+ y into this.ret()  end |}]
