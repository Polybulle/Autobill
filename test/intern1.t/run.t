Test the prelude internalizer
  $ autobill -M -i test_prelude.bill
  decl type Test_so__19 : nat
  decl type Test1__20 : +
  type Test2__21 : + = Unit
  type Test3__22 (A__27 : +) (B__28 : -) : - = B__28
  type Test4__23 : - = (Test3__22 Unit Top)
  type Test5__24 (A__29 : -) : - = Test4__23
  data Test7__25 =
    | cons1__30()
    | cons2__31(Test2__21, Test1__20)
  comput Test8__26 =
    | this.destr1__32().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -M -i test_prog.bill
  val<<pol__33>> test9__31 : T__34 =
    unit()
  val<<pol__70>> test9__35 : T__71 =
    bind/cc<<pol__36>> (a__38 : <<T__37>>) ->
      unit().bind<<pol__68>> (x__43 : <<T__42>>) ->
        cmd<<pol__67>> : T__44 val =
          bind/cc<<pol__45>> (b__47 : <<T__46>>) ->
            unit().bind<<pol__57>> (x__52 : <<T__51>>) ->
              x__52.ret(b__47)
        stk =
          this.bind<<pol__66>> (y__61 : <<T__60>>) ->
            x__43.ret(a__38)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -M -i | autobill -M -p
  decl type Test_so__19 : nat
  decl type Test1__20 : +
  type Test2__21 : + = Unit
  type Test3__22 (A__27 : +) (B__28 : -) : - = B__28
  type Test4__23 : - = (Test3__22 Unit Top)
  type Test5__24 (A__29 : -) : - = Test4__23
  data Test7__25 =
    | cons1__30()
    | cons2__31(Test2__21, Test1__20)
  comput Test8__26 =
    | this.destr1__32().ret((Thunk Unit))
  val test9__45 : T__48 = unit()
  val test9__49 : T__85 =
    bind/cc a__52 -> unit()
      .bind (x__57) ->
        cmd
        : T__58 val =
          bind/cc b__61 -> unit().bind (x__66) -> x__66.ret(b__61)
        stk =
          this.bind (y__75) -> x__57.ret(a__52)
        end
