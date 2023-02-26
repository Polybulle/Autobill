Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  val<<->> f : (Fun (T__15) -> (Thunk T__15)) =
    match
      | this.call(y : T__15).ret(a : (Thunk T__15)) -> thunk(y).ret(a)
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  val<<->> f : (fix T__16) =
    match this.fix(x : (Exp (fix T__16))).ret(a : T__16) -> x.unbox(Exp).fix().ret(a)

Test with user sorts
  $ autobill -t sorts.bill
  decl sort nat
  decl sort res
  
  decl type N_to_r : (nat -> res)
  decl type R_to_n : (res -> nat)
  decl type R0 : res
  type N0 : nat = (R_to_n R0)
  type R1 : res = (N_to_r N0)
  type R2 : res = (N_to_r (R_to_n R0))
  decl type R_to_pos : (res -> +)
  decl val<<+>> x : (R_to_pos (N_to_r (R_to_n R0)))
  val<<+>> y : (R_to_pos (N_to_r (R_to_n R0))) =
    x

Test on the swap function f(x,y) = (y,x):
  $ autobill -t swap.bill
  val<<->> swap : (Fun ((T__23 * T__25)) -> (Thunk (T__25 * T__23))) =
    match
      | this.call(t : (T__23 * T__25)).ret(a : (Thunk (T__25 * T__23))) ->
        t.match
           | tuple(x : T__23, y : T__25) -> thunk(tuple(y, x)).ret(a)
         end
    end
