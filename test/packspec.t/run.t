Test a simple pack/spec program.

  $ autobill -L -s sorts.bill
  data T_21 (A_27 : +) =
    | c_28()
  comput U_22 (A_29 : -) =
    | this.d_30().ret(A_29)
  decl type C_23 : -
  decl type D_24 : +
  decl val- x_43 : C_23
  decl val+ z_45 : D_24
  val+ test_47 : T_51 =
    c_28()
  val- test0_52 : T_63 =
    match this.d_30().ret(a_54 : T_55) ->
    cmd- : T_57 val =
      x_43
    stk =
      this.ret(a_54)
  val- test1_64 : T_76 =
    bind/cc- a_67 : T_66 ->
    cmd- : T_68 val =
      x_43
    stk =
      this.d_30().ret(a_67)


  $ autobill -L -t types.bill
  comput Id_21 =
    | this.inst_25<A_24 : +>().ret((Fun A_24 -> (Thunk A_24)))
  val- id2_38 : Id_21 =
    match this.inst_25<B_40 : +>().ret(a_41 : (Fun T_57 -> (Thunk T_57))) ->
    cmd- : (Fun T_57 -> (Thunk T_57)) val =
      match this.call(x_46 : T_57).ret(b_48 : (Thunk T_57)) ->
      cmd- : (Thunk T_57) val =
        match this.thunk().ret(c_54 : T_57) ->
        cmd+ : T_57 val =
          x_46
        stk =
          this.ret(c_54)
      stk =
        this.ret(b_48)
    stk =
      this.ret(a_41)
