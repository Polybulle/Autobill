open Autobill
open Calculi
open PreLAMECalc


(*  let%test _ =
  "(jump foo)" = string_of_cst (cst_of_string "(jump foo)")
*)

let var x = (Var.of_string x)
let covar x = (CoVar.of_string x)
let tvar x = tvar (TyVar.of_string x)

let test1 =
    V.var (var "x") |+|
    S.bind (var "y") (tvar "a") (V.var (var "y") |+| S.var (covar "alpha"))


let test2 =
    V.box linear (covar "alpha") (tvar "a") (V.var (var "x") |?| S.var (covar "alpha"))
    |+|
    S.bind (var "y") (boxed linear (tvar "a"))
      (V.var (var "y") |+| S.box linear (S.var (covar "beta")))


let test3 =
    V.str_bind (covar "output") (data (prod (tvar "a") (tvar "b")))
      ((V.var (var "input"))
       |+|
       (S.case [ pair ("x", tvar "a") ("y", tvar "b") |=> (
            (V.cons (pair (V.var (var "y")) (V.var (var "x")))) |+| S.var (covar "output"))]))

let test4 =
    V.case [
      call
        ("x", pos (data (sum (tvar "a") (tvar "a"))))
        ("alpha", tvar "a")
      |=>
      (V.var "x" |+| S.case [
          (fst ("y", tvar "a")) |=> ((V.var "y") |+| (S.var "alpha"));
          (snd ("y", tvar "a")) |=> ((V.var "y") |+| (S.var "alpha"))
        ])]

let prog = [
  Cmd_definition {
    name = "test1";
    content = test1
  };
  Cmd_definition {
    name = "test2";
    content = test2
  };
  Term_definition {
    name = "test3";
    typ = omitted;
    content = test3
  };
  Term_definition {
    name = "test4";
    typ = omitted;
    content = test4
  }
]
