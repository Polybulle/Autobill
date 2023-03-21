Test type inference on the identity : (fun t (shift- t))
  $ autobill -M -t id.bill
  val- f__33 : (Fun T__43 -> (Thunk T__43)) =
    match
      | this.call(y__35 : T__43).ret(a__37 : (Thunk T__43)) -> thunk(y__35).ret(a__37)
    end

Test on the trivial fixpoint
  $ autobill -M -t fixpoint.bill
  val- f__33 : (Fix T__37) =
    match this.fix(x__35 : (Closure Exp (Fix T__37))).ret(a__34 : T__37) ->
      x__35.unbox(Exp).fix().ret(a__34)

Test with user sorts
  $ autobill -M -t sorts.bill
  decl sort res__21
  decl type N_to_r__22 : res__21
  decl type R_to_n__23 : nat
  decl type R0__24 : res__21
  type N0__25 : nat = (R_to_n__23 R0__24)
  type R1__26 : res__21 = (N_to_r__22 N0__25)
  type R2__27 : res__21 = (N_to_r__22 (R_to_n__23 R0__24))
  decl type R_to_pos__28 : +
  decl val+ x__41 : (R_to_pos__28 (N_to_r__22 (R_to_n__23 R0__24)))
  val+ y__43 : (R_to_pos__28 (N_to_r__22 (R_to_n__23 R0__24))) =
    x__41

Test on the swap function f(x,y) = (y,x):
  $ autobill -M -t swap.bill
  val- swap__33 : (Fun (T__54 * T__53) -> (Thunk (T__53 * T__54))) =
    match
      | this.call(t__35 : (T__54 * T__53)).ret(a__37 : (Thunk (T__53 * T__54))) ->
        t__35.match
          | tuple(x__45 : T__54, y__47 : T__53) -> thunk(tuple(y__47, x__45)).ret(a__37)
          end
    end
