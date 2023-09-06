Test the sort inference
  $ autobill -L -s test.bill
  decl type Char__21 : +
  data List__22 (C__25 : +) =
    | nil__26()
    | cons__27(C__25, (List__22 C__25))
  decl val- succ__40 : (Fun Int -> (Thunk Int))
  val+ lz__42 : T__52 =
    cons__27(int{0}(), cons__27(int{0}(), nil__26()))
  decl val- map__53 : (Fun (Closure Lin (Fun Int -> (Thunk Char__21))) (List__22 Int) -> (Thunk (List__22 Char__21)))
