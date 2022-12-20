Test the sort inference
  $ autobill -s test.bill
  decl type int : +
  decl type char : +
  data list (c : +) =
    | Nil
    | Cons(c, (list c))
  decl val+ z : int
  decl val- succ : (fun (int) -> (thunk int))
  val+ lz : t__32 = Cons(z, Cons(z, Nil))
  decl val- map : (fun ((closure (fun (int) -> (thunk char)))
              , (list int)) -> (thunk (list char)))
