Test the prelude internalizer
  $ autobill -L -i test_prelude.bill
  decl type Test_so_21 : nat
  decl type Test1_22 : +
  type Test2_23 : + = Unit
  type Test3_24 (A_31 : +) (B_32 : -) : - = B_32
  type Test4_25 : - = (Test3_24 Unit Top)
  type Test5_26 (A_33 : -) : - = Test4_25
  data Test7_27 =
    | cons1_34()
    | cons2_35(Test2_23, Test1_22)
  comput Test8_28 =
    | this.destr1_36().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -L -i test_prog.bill
  val<<pol_37>> test9_35 : T_38 =
    unit()
  val<<pol_74>> test9_39 : T_75 =
    bind/cc<<pol_40>> a_42 : <<T_41>> ->
    cmd<<pol_73>> : T_43 val =
      unit()
    stk =
      this.bind<<pol_72>> x_47 : T_46 ->
      cmd<<pol_71>> : T_48 val =
        bind/cc<<pol_49>> b_51 : <<T_50>> ->
        cmd<<pol_62>> : T_52 val =
          unit()
        stk =
          this.bind<<pol_61>> x_56 : T_55 ->
          cmd<<pol_60>> : T_57 val =
            x_56
          stk =
            this.ret(b_51)
      stk =
        this.bind<<pol_70>> y_65 : T_64 ->
        cmd<<pol_69>> : T_66 val =
          x_47
        stk =
          this.ret(a_42)
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -L -i | autobill -L -p
  decl type Test_so_21 : nat
  decl type Test1_22 : +
  type Test2_23 : + = Unit
  type Test3_24 (A_31 : +) (B_32 : -) : - = B_32
  type Test4_25 : - = (Test3_24 Unit Top)
  type Test5_26 (A_33 : -) : - = Test4_25
  data Test7_27 =
    | cons1_34()
    | cons2_35(Test2_23, Test1_22)
  comput Test8_28 =
    | this.destr1_36().ret((Thunk Unit))
  val test9_49 : T_52 = unit()
  val test9_53 : T_89 =
    bind/cc a_56 ->
      unit()
      .bind (x_61 : T_60) ->
        cmd
        : T_62 val =
          bind/cc b_65 -> unit().bind (x_70 : T_69) -> x_70.ret(b_65)
        stk =
          this.bind (y_79 : T_78) -> x_61.ret(a_56)
