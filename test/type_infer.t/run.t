Test type inference on the identity : (fun t (shift- t))
  $ autobill -M -t id.bill
  val- f__35 : (Fun T__45 -> (Thunk T__45)) =
    match
      | this.call(y__37 : T__45).ret(a__39 : (Thunk T__45)) -> thunk(y__37).ret(a__39)
    end

Test on the trivial fixpoint
  $ autobill -M -t fixpoint.bill
  val- f__35 : (Fix T__39) =
    match this.fix(x__37 : (Closure Exp (Fix T__39))).ret(a__36 : T__39) ->
      x__37.unbox(Exp).fix().ret(a__36)

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
  decl val+ x__43 : (R_to_pos__28 (N_to_r__22 (R_to_n__23 R0__24)))
  val+ y__45 : (R_to_pos__28 (N_to_r__22 (R_to_n__23 R0__24))) =
    x__43

Test on the swap function f(x,y) = (y,x):
  $ autobill -M -t swap.bill
  val- swap__35 : (Fun (T__56 * T__55) -> (Thunk (T__55 * T__56))) =
    match
      | this.call(t__37 : (T__56 * T__55)).ret(a__39 : (Thunk (T__55 * T__56))) ->
        t__37.match
          | tuple(x__47 : T__56, y__49 : T__55) -> thunk(tuple(y__49, x__47)).ret(a__39)
          end
    end
