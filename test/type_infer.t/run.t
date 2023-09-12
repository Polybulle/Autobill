Test type inference on the identity : (fun t (shift- t))
  $ autobill -L -t id.bill
  val- f$35 : (Fun T$48 -> (Thunk T$48)) =
    match this.call(y$37 : T$48).ret(a$39 : (Thunk T$48)) ->
      cmd- : (Thunk T$48) val =
        match this.thunk().ret(b$45 : T$48) -> y$37.ret(b$45)
      stk =
        this.ret(a$39)

Test on the trivial fixpoint
  $ autobill -L -t fixpoint.bill
  val+ f$35 : (Fix T$37) =
    match this.fix().ret(a$36 : T$37) -> self.this
      .fix()
      .ret(a$36)

Test with user sorts
  $ autobill -L -t sorts.bill
  decl sort res$21
  decl type N_to_r$22 : res$21
  decl type R_to_n$23 : nat
  decl type R0$24 : res$21
  type N0$25 : nat = (R_to_n$23 R0$24)
  type R1$26 : res$21 = (N_to_r$22 N0$25)
  type R2$27 : res$21 = (N_to_r$22 (R_to_n$23 R0$24))
  decl type R_to_pos$28 : +
  decl val+ x$43 : (R_to_pos$28 (N_to_r$22 (R_to_n$23 R0$24)))
  val+ y$45 : (R_to_pos$28 (N_to_r$22 (R_to_n$23 R0$24))) =
    x$43

Test on the swap function f(x,y) = (y,x):
  $ autobill -L -t swap.bill
  val- swap$35 : (Fun (T$60 * T$59) -> (Thunk (T$59 * T$60))) =
    match this.call(t$37 : (T$60 * T$59)).ret(a$39 : (Thunk (T$59 * T$60))) ->
      t$37.match tuple(x$47 : T$60, y$49 : T$59) ->
        cmd- : (Thunk (T$59 * T$60)) val =
          match this.thunk().ret(b$54 : (T$59 * T$60)) -> tuple(y$49, x$47).ret(b$54)
        stk =
          this.ret(a$39)
