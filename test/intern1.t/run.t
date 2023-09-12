Test the prelude internalizer
  $ autobill -L -i test_prelude.bill
  decl type Test_so$21 : nat
  decl type Test1$22 : +
  type Test2$23 : + = Unit
  type Test3$24 (A$31 : +) (B$32 : -) : - = B$32
  type Test4$25 : - = (Test3$24 Unit Top)
  type Test5$26 (A$33 : -) : - = Test4$25
  data Test7$27 =
    | cons1$34()
    | cons2$35(Test2$23, Test1$22)
  comput Test8$28 =
    | this.destr1$36().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -L -i test_prog.bill
  val<<pol$37>> test9$35 : T$38 =
    unit()
  val<<pol$74>> test9$39 : T$75 =
    bind/cc<<pol$40>> a$42 : <<T$41>> ->
      unit().bind<<pol$72>> x$47 : T$46 ->
        cmd<<pol$71>> : T$48 val =
          bind/cc<<pol$49>> b$51 : <<T$50>> ->
            unit().bind<<pol$61>> x$56 : T$55 -> x$56.ret(b$51)
        stk =
          this.bind<<pol$70>> y$65 : T$64 -> x$47.ret(a$42)
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -L -i | autobill -L -p
  decl type Test_so$21 : nat
  decl type Test1$22 : +
  type Test2$23 : + = Unit
  type Test3$24 (A$31 : +) (B$32 : -) : - = B$32
  type Test4$25 : - = (Test3$24 Unit Top)
  type Test5$26 (A$33 : -) : - = Test4$25
  data Test7$27 =
    | cons1$34()
    | cons2$35(Test2$23, Test1$22)
  comput Test8$28 =
    | this.destr1$36().ret((Thunk Unit))
  val test9$49 : T$52 = unit()
  val test9$53 : T$89 =
    bind/cc a$56 ->
      unit()
      .bind (x$61 : T$60) ->
        cmd
        : T$62 val =
          bind/cc b$65 -> unit().bind (x$70 : T$69) -> x$70.ret(b$65)
        stk =
          this.bind (y$79 : T$78) -> x$61.ret(a$56)
