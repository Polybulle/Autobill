Test type inference on the identity : (fun t (shift- t))
  $ autobill -L -t id.bill
  val- f_35 : (Fun T_48 -> (Thunk T_48)) =
    match this.call(y_37 : T_48).ret(a_39 : (Thunk T_48)) ->
    cmd- : (Thunk T_48) val =
      match this.thunk().ret(b_45 : T_48) ->
      cmd+ : T_48 val =
        y_37
      stk =
        this.ret(b_45)
    stk =
      this.ret(a_39)

Test on the trivial fixpoint
  $ autobill -L -t fixpoint.bill
  val+ f_35 : (Fix T_37) =
    match this.fix().ret(a_36 : T_37) -> self.this
      .fix()
      .ret(a_36)

Test with user sorts
  $ autobill -L -t sorts.bill
  decl sort res_21
  decl type N_to_r_22 : res_21
  decl type R_to_n_23 : nat
  decl type R0_24 : res_21
  type N0_25 : nat = (R_to_n_23 R0_24)
  type R1_26 : res_21 = (N_to_r_22 N0_25)
  type R2_27 : res_21 = (N_to_r_22 (R_to_n_23 R0_24))
  decl type R_to_pos_28 : +
  decl val+ x_43 : (R_to_pos_28 (N_to_r_22 (R_to_n_23 R0_24)))
  val+ y_45 : (R_to_pos_28 (N_to_r_22 (R_to_n_23 R0_24))) =
    x_43

Test on the swap function f(x,y) = (y,x):
  $ autobill -L -t swap.bill
  val- swap_35 : (Fun (T_60 * T_59) -> (Thunk (T_59 * T_60))) =
    match this.call(t_37 : (T_60 * T_59)).ret(a_39 : (Thunk (T_59 * T_60))) ->
    cmd+ : (T_60 * T_59) val =
      t_37
    stk =
      this.match tuple(x_47 : T_60, y_49 : T_59) ->
    cmd- : (Thunk (T_59 * T_60)) val =
      match this.thunk().ret(b_54 : (T_59 * T_60)) ->
      cmd+ : (T_59 * T_60) val =
        tuple(y_49, x_47)
      stk =
        this.ret(b_54)
    stk =
      this.ret(a_39)
