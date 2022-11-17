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
  decl type n_to_r__8 : (nat__6 -> res__7)
  decl type r_to_n__9 : (res__7 -> nat__6)
  decl type r0__10 : res__7
  type n0__11 : nat__6 = (r_to_n__9 r0__10)
  type r1__12 : res__7 = (n_to_r__8 n0__11)
  type r2__13 : res__7 = (n_to_r__8 (r_to_n__9 r0__10))
  decl type r_to_pos__14 : (res__7 -> +)
  decl val<+> x__15 : (r_to_pos__14 (n_to_r__8 (r_to_n__9 r0__10)))
  val<+> y__17 : (r_to_pos__14 (n_to_r__8 (r_to_n__9 r0__10))) = x__15
