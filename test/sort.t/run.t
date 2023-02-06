Test the sort inference
  $ autobill -s test.bill
  decl type Char : +
  data List (C : +) =
    | nil()
    | cons(C, (List C))
  decl val<<->> succ : (Fun (Int) -> (Thunk Int))
  val<<+>> lz : T__29 = cons(int(0), cons(int(0), nil()))
  decl val<<->> map : (Fun ((Closure (Fun (Int) -> (Thunk Char)))
                  , (List Int)) -> (Thunk (List Char)))
