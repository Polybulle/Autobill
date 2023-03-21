Test the sort inference
  $ autobill -M -s test.bill
  decl type Char__21 : +
  data List__22 (C__23 : +) =
    | nil__24()
    | cons__25(C__23, (List__22 C__23))
  decl val- succ__38 : (Fun Int -> (Thunk Int))
  val+ lz__40 : T__50 =
    cons__25(int{0}(), cons__25(int{0}(), nil__24()))
  decl val- map__51 : (Fun (Closure Lin (Fun Int -> (Thunk Char__21))) (List__22 Int) -> (Thunk (List__22 Char__21)))
