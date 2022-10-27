Test the sort inference
  $ autobill -s test.bill
  decl type int__6 : +
  decl type char__7 : +
  data list__8 (c__9 : +) =
    case :nil__10()
    case :cons__11(c__9, (list__8 c__9))
  /* constructor "nil__10" is forall (c__9 : +). nil__10() : (list__8 c__9)*/
  /* constructor "cons__11" is forall (c__9 : +). cons__11(c__9,
      (list__8 c__9)) : (list__8 c__9)*/
  decl val<+> z__12 : int__6
  decl val<-> succ__14 : (fun int__6 -> (shift- int__6))
  val<+> lz__16 : t__23 = :cons__11(z__12, :cons__11(z__12, :nil__10()))
  decl val<-> map__24
                : (fun (shift+ (fun int__6 -> (shift- char__7)))
                    (list__8 int__6) -> (shift- (list__8 char__7)))
