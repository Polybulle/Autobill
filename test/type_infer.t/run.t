Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  val- f : (fun (t__15) -> (thunk t__15)) =
    match
      | this.call(y : t__15).ret(a : (thunk t__15)) -> thunk(y).ret(a)
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  val- f : (fix t__16) =
    match this.fix(x : (exp (fix t__16))).ret(a : t__16) -> x.unbox(exp).fix()
      .ret(a)

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

Test on the swap function f(x,y) = (y,x):
  $ autobill -t swap.bill
  val- swap : (fun ((t__23 * t__25)) -> (thunk (t__25 * t__23))) =
    match
      | this.call(t : (t__23 * t__25)).ret(a : (thunk (t__25 * t__23))) ->
        t.match
           | tupple(x : t__23, y : t__25) -> thunk(tupple(y, x)).ret(a)
         end
    end
