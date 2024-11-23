Test the sort inference
  $ autobill -L -s test.bill
  decl type Char_21 : +
  data List_22 (C_25 : +) =
    | nil_26()
    | cons_27(C_25, (List_22 C_25))
  decl val- succ_40 : (Fun Int -> (Thunk Int))
  val+ lz_42 : T_52 =
    cons_27(int(0), cons_27(int(0), nil_26()))
  decl val- map_53 : (Fun (Closure Lin (Fun Int -> (Thunk Char_21))) (List_22 Int) -> (Thunk (List_22 Char_21)))
