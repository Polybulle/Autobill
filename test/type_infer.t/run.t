Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  /* tyvar t__13 : - */
  /* tyvar t__15 : + */
  /* tyvar t__17 : - */
  /* tyvar t__18 : - */
  /* tyvar t__19 : - */
  /* tyvar t__20 : - */
  /* tyvar t__21 : + */
  /* tyvar t__22 : - */
  /* tyvar t__25 : - */
  /* var y__14 used 1 : t__15 */
  /* cont a__16 used 1 : t__17 */
  val- f__12 : (fun (thunk t__15) t__15) =
    match
      case this.call(y__14 : t__15)a__16 : (thunk t__15) -> thunk(y__14)
        .ret(a__16)
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  /* tyvar t__15 : - */
  /* tyvar t__16 : - */
  /* tyvar t__17 : + */
  /* tyvar t__18 : - */
  /* tyvar t__19 : + */
  /* tyvar t__20 : - */
  /* tyvar t__21 : + */
  /* tyvar t__22 : - */
  /* tyvar t__25 : - */
  /* var x__14 used * : t__15 */
  /* cont a__13 used 1 : t__16 */
  val- f__12 : (fix a__57) =
    match this.fix(x__14 : (exp (fix a__57)))a__13 : (fix a__57) -> x__14
      .unbox(exp).ret(a__13)

Test with user sorts
  $ autobill -t sorts.bill
  decl type n_to_r__14 : (nat__12 -> res__13)
  decl type r_to_n__15 : (res__13 -> nat__12)
  decl type r0__16 : res__13
  type n0__17 : nat__12 = (r_to_n__15 r0__16)
  type r1__18 : res__13 = (n_to_r__14 n0__17)
  type r2__19 : res__13 = (n_to_r__14 (r_to_n__15 r0__16))
  decl type r_to_pos__20 : (res__13 -> +)
  /* tyvar t__24 : + */
  decl val+ x__21 : (r_to_pos__20 (n_to_r__14 (r_to_n__15 r0__16)))
  val+ y__23 : (r_to_pos__20 (n_to_r__14 (r_to_n__15 r0__16))) = x__21
