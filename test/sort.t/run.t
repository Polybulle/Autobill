Test the sort inference
  $ autobill -M -s test.bill
  decl type Char__19 : +
  data List__20 (C__21 : +) =
    | nil__22()
    | cons__23(C__21, (List__20 C__21))
  decl val- succ__36 : (Fun Int -> (Thunk Int))
  val+ lz__38 : T__48 =
    cons__23(int{0}(), cons__23(int{0}(), nil__22()))
  decl val- map__49 : (Fun (Closure Lin (Fun Int -> (Thunk Char__19))) (List__20 Int) -> (Thunk (List__20 Char__19)))
