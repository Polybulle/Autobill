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
  /* var f used 0 : (fun (thunk t__15) t__15) */
  /* var y used 1 : t__15 */
  /* cont a used 1 : (thunk t__15) */
  val- f : (fun (thunk t__15) t__15) =
    match
      case this.call(y : t__15)a : (thunk t__15) -> thunk(y).ret(a)
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
  /* var f used 0 : (fix a__56) */
  /* var x used * : (fix a__56) */
  /* cont a used 1 : (fix a__56) */
  val- f : (fix a__56) =
    match this.fix(x : (exp (fix a__56)))a : (fix a__56) -> x.unbox(exp).ret(a)

Test with user sorts
  $ autobill -t sorts.bill
  decl type n_to_r : (nat -> res)
  decl type r_to_n : (res -> nat)
  decl type r0 : res
  type n0 : nat = (r_to_n r0)
  type r1 : res = (n_to_r n0)
  type r2 : res = (n_to_r (r_to_n r0))
  decl type r_to_pos : (res -> +)
  /* tyvar t__24 : + */
  /* var x used 1 : (r_to_pos (n_to_r (r_to_n r0))) */
  /* var y used 0 : (r_to_pos (n_to_r (r_to_n r0))) */
  decl val+ x : (r_to_pos (n_to_r (r_to_n r0)))
  val+ y : (r_to_pos (n_to_r (r_to_n r0))) = x
