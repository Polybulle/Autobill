open Ast
open Vars
open Types
open Constructors

module EmptyCalc = struct
  include Empty
  include StringVar
  module Types = FullTypes (StringVar) (Empty)
  include Types
  include LCalc (StringVar) (Empty) (Types)
end

module ILLCalc = struct
  include ILL
  include StringVar
  module Types = FullTypes (StringVar) (ILL)
  include Types
  include LCalc (StringVar) (ILL) (Types)
end


let test1 = EmptyCalc.(
    V.var "x" |>| S.bind "y" (posvar "a") (V.var "y" |<| S.var "alpha")
  )

let test2 = EmptyCalc.(
    V.box Linear "alpha" (tvar "a") (V.var "x" |<| S.var "alpha")
    |>|
    S.bind "y" (boxed Linear (tvar "a")) (V.var "y" |>| S.box Linear (S.var "beta"))
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

