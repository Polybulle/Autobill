Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  /* tyvar t__9 : + */
  /* tyvar t__11 : - */
  /* var y__8 used 1 : t__9 */
  /* cont a__10 used 1 : t__11 */
  val<-> f__6 : (fun t__9 -> (thunk t__9)) =
    match
      case this.call(y__8 : t__9).ret(a__10 : (thunk t__9)) -> thunk(y__8)
        .ret(a__10)
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  /* tyvar t__9 : + */
  /* tyvar t__10 : - */
  /* var x__8 used * : t__9 */
  /* cont a__7 used 1 : t__10 */
  val<-> f__6 : (fix a__66) =
    match this.fix(x__8 : (exp (fix a__66))).ret(a__7 : (fix a__66)) -> x__8
      .unbox(exp).ret(a__7)

Test with user sorts
  $ autobill -t sorts.bill
