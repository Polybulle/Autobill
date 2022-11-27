Test the sort inference
  $ autobill -s test.bill
  decl type int__12 : +
  decl type char__13 : +
  data list__14 (c__15 : +) =
    case nil__16()
    case cons__17(c__15, (list__14 c__15))
  /* constructor "nil__16" is
      forall (c__15 : +). nil__16() : (list__14 c__15)*/
  /* constructor "cons__17" is forall (c__15 : +). cons__17(c__15,
      (list__14 c__15)) : (list__14 c__15)*/
  /* tyvar t__23 : + */
  /* tyvar t__24 : + */
  /* tyvar t__25 : + */
  /* tyvar t__26 : + */
  /* tyvar t__27 : + */
  /* tyvar t__29 : + */
  decl val+ z__18 : int__12
  decl val- succ__20 : (fun (thunk int__12) int__12)
  val+ lz__22 : t__29 = :cons__17(z__18, :cons__17(z__18, :nil__16()))
  decl val- map__30
              : (fun (thunk (list__14 char__13))
                  (closure (fun (thunk char__13) int__12)) (list__14 int__12))
