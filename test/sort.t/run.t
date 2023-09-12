Test the sort inference
  $ autobill -L -s test.bill
  decl type Char$21 : +
  data List$22 (C$25 : +) =
    | nil$26()
    | cons$27(C$25, (List$22 C$25))
  decl val- succ$40 : (Fun Int -> (Thunk Int))
  val+ lz$42 : T$52 =
    cons$27(int(0), cons$27(int(0), nil$26()))
  decl val- map$53 : (Fun (Closure Lin (Fun Int -> (Thunk Char$21))) (List$22 Int) -> (Thunk
                                                                                  (List$22 Char$21)))
