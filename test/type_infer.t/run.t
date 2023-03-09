Test type inference on the identity : (fun t (shift- t))
  $ autobill -M -t id.bill
  val- f__31 : (Fun T__41 -> (Thunk T__41)) =
    match
      | this.call(y__33 : T__41).ret(a__35 : (Thunk T__41)) -> thunk(y__33).ret(a__35)
    end

Test on the trivial fixpoint
  $ autobill -M -t fixpoint.bill
  val- f__31 : (Fix T__35) =
    match this.fix(x__33 : (Closure Exp (Fix T__35))).ret(a__32 : T__35) ->
      x__33.unbox(Exp).fix().ret(a__32)

Test with user sorts
  $ autobill -M -t sorts.bill
  decl sort res__19
  decl type N_to_r__20 : res__19
  decl type R_to_n__21 : nat
  decl type R0__22 : res__19
  type N0__23 : nat = (R_to_n__21 R0__22)
  type R1__24 : res__19 = (N_to_r__20 N0__23)
  type R2__25 : res__19 = (N_to_r__20 (R_to_n__21 R0__22))
  decl type R_to_pos__26 : +
  decl val+ x__39 : (R_to_pos__26 (N_to_r__20 (R_to_n__21 R0__22)))
  val+ y__41 : (R_to_pos__26 (N_to_r__20 (R_to_n__21 R0__22))) =
    x__39

Test on the swap function f(x,y) = (y,x):
  $ autobill -M -t swap.bill
  val- swap__31 : (Fun (T__52 * T__51) -> (Thunk (T__51 * T__52))) =
    match
      | this.call(t__33 : (T__52 * T__51)).ret(a__35 : (Thunk (T__51 * T__52))) ->
        t__33.match
          | tuple(x__43 : T__52, y__45 : T__51) -> thunk(tuple(y__45, x__43)).ret(a__35)
          end
    end
