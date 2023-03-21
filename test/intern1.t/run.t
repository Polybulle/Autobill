Test the prelude internalizer
  $ autobill -M -i test_prelude.bill
  decl type Test_so__21 : nat
  decl type Test1__22 : +
  type Test2__23 : + = Unit
  type Test3__24 (A__29 : +) (B__30 : -) : - = B__30
  type Test4__25 : - = (Test3__24 Unit Top)
  type Test5__26 (A__31 : -) : - = Test4__25
  data Test7__27 =
    | cons1__32()
    | cons2__33(Test2__23, Test1__22)
  comput Test8__28 =
    | this.destr1__34().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -M -i test_prog.bill
  val<<pol__35>> test9__33 : T__36 =
    unit()
  val<<pol__72>> test9__37 : T__73 =
    bind/cc<<pol__38>> (a__40 : <<T__39>>) ->
      unit().bind<<pol__70>> (x__45 : <<T__44>>) ->
        cmd<<pol__69>> : T__46 val =
          bind/cc<<pol__47>> (b__49 : <<T__48>>) ->
            unit().bind<<pol__59>> (x__54 : <<T__53>>) ->
              x__54.ret(b__49)
        stk =
          this.bind<<pol__68>> (y__63 : <<T__62>>) ->
            x__45.ret(a__40)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -M -i | autobill -M -p
  decl type Test_so__21 : nat
  decl type Test1__22 : +
  type Test2__23 : + = Unit
  type Test3__24 (A__29 : +) (B__30 : -) : - = B__30
  type Test4__25 : - = (Test3__24 Unit Top)
  type Test5__26 (A__31 : -) : - = Test4__25
  data Test7__27 =
    | cons1__32()
    | cons2__33(Test2__23, Test1__22)
  comput Test8__28 =
    | this.destr1__34().ret((Thunk Unit))
  val test9__47 : T__50 = unit()
  val test9__51 : T__87 =
    bind/cc a__54 -> unit()
      .bind (x__59) ->
        cmd
        : T__60 val =
          bind/cc b__63 -> unit().bind (x__68) -> x__68.ret(b__63)
        stk =
          this.bind (y__77) -> x__59.ret(a__54)
        end
