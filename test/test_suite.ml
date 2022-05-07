open Autobill
open Calculi

let cst_of_string s = Parser.prog Lexer.token (Lexing.from_string s)

let string_of_cst x = Cst.to_string x

let tests = ["()"]

(*  let%test _ =
  "(jump foo)" = string_of_cst (cst_of_string "(jump foo)")
*)

let test1 = EmptyCalc.(
    let var x = V.var (Var.of_string x) in
    var "x" |+| S.bind "y" (posvar "a") (var "y" |+| S.var "alpha")
  )

let test2 = EmptyCalc.(
    V.box Linear "alpha" (tvar "a") (V.var "x" |?| S.var "alpha")
    |+|
    S.bind "y" (boxed Linear (tvar "a")) (V.var "y" |+| S.box Linear (S.var "beta"))
  )


let test3 = ILLCalc.(
    V.str_bind "output" (data (prod (tvar "a") (tvar "b")))
      ((V.var "input")
       |+|
       (S.case [ pair ("x", tvar "a") ("y", tvar "b") |=> (
            V.(cons (pair (var "y") (var "x"))) |+| S.var "output")])))

let test4 = ILLCalc.(
    V.case [
      call
        ("x", pos (data (sum (tvar "a") (tvar "a"))))
        ("alpha", tvar "a")
      |=>
      (V.var "x" |+| S.case [
          (fst ("y", tvar "a")) |=> ((V.var "y") |+| (S.var "alpha"));
          (snd ("y", tvar "a")) |=> ((V.var "y") |+| (S.var "alpha"))
        ])])
