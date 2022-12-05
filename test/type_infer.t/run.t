Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  val- f : (fun (thunk t__15) t__15) =
    match
      case this.call(y : t__15)a : (thunk t__15) -> thunk(y).ret(a)
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
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
  decl val+ x : (r_to_pos (n_to_r (r_to_n r0)))
  val+ y : (r_to_pos (n_to_r (r_to_n r0))) = x
