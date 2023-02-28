Test type inference on the identity : (fun t (shift- t))
  $ autobill -M -t id.bill
  val- f : (Fun T__15 -> (Thunk T__15)) =
    match
      | this.call(y : T__15).ret(a : (Thunk T__15)) -> thunk(y).ret(a)
    end

Test on the trivial fixpoint
  $ autobill -M -t fixpoint.bill
  val- f : (Fix T__16) =
    match this.fix(x : (Closure Exp (Fix T__16))).ret(a : T__16) ->
      x.unbox(Exp).fix().ret(a)

Test with user sorts
  $ autobill -M -t sorts.bill
  decl sort nat
  decl sort res
  
  decl type N_to_r : res
  decl type R_to_n : nat
  decl type R0 : res
  type N0 : nat = (R_to_n R0)
  type R1 : res = (N_to_r N0)
  type R2 : res = (N_to_r (R_to_n R0))
  decl type R_to_pos : +
  decl val+ x : (R_to_pos (N_to_r (R_to_n R0)))
  val+ y : (R_to_pos (N_to_r (R_to_n R0))) =
    x

Test on the swap function f(x,y) = (y,x):
  $ autobill -M -t swap.bill
  val- swap : (Fun (T__21 * T__23) -> (Thunk (T__23 * T__21))) =
    match
      | this.call(t : (T__21 * T__23)).ret(a : (Thunk (T__23 * T__21))) -> t.match
                                                                             | tuple(x : T__21, y : T__23) -> thunk(tuple(y, x)).ret(a),end
    end
