Test the sort inference
  $ autobill -s test.bill
  decl type Char : +
  data List (C : +) =
    | nil()
    | cons(C, (List C))
  decl val- succ : (Fun Int -> (Thunk Int))
  val+ lz : T__17 =
    bind/cc+ a__33 : T__17 ->
      cons(int{0}(), cons(int{0}(), nil())).ret(a__33)
  decl val- map : (Fun (Lin (Fun Int -> (Thunk Char))) (List Int) -> (Thunk (List Char)))
