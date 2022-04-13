open Ast
open Vars
open Constructors

module EmptyCalc = LCalc (StringVar) (Empty)

module ILLCalc = struct
  include LCalc (StringVar) (ILL)
  include ILL
end


let test1 = EmptyCalc.(
    V.var "x" |>| S.bind "y" (posvar "a") (V.var "y" |<| S.var "alpha")
  )

let test2 = EmptyCalc.(
    V.box "alpha" (tvar "a") (V.var "x" |<| S.var "alpha")
    |>|
    S.bind "y" (exp (tvar "a")) (V.var "y" |>| S.box (S.var "beta"))
  )


let test3 = ILLCalc.(
    T.str_bind "output" (data (prod (tvar "a") (tvar "b"))) (
      (V.var "input")
      |>|
      (S.case [ pair ("x", tvar "a") ("y", tvar "b") |=> (
           V.(cons (pair (var "y") (var "x"))) |>| S.var "output")])))

let test4 = ILLCalc.(
    V.cocase [
      call
        ("x", pos (data (sum (tvar "a") (tvar "a"))))
        ("alpha", tvar "a")
      |=>
      (V.var "x" |>| S.case [
          (fst ("y", tvar "a")) |=> ((V.var "y") |>| (S.var "alpha"));
          (snd ("y", tvar "a")) |=> ((V.var "y") |>| (S.var "alpha"))
        ])])

