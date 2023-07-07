Test the prelude internalizer
  $ autobill -M -i test_prelude.bill
  decl type Test_so__21 : nat
  decl type Test1__22 : +
  type Test2__23 : + = Unit
  type Test3__24 (A__31 : +) (B__32 : -) : - = B__32
  type Test4__25 : - = (Test3__24 Unit Top)
  type Test5__26 (A__33 : -) : - = Test4__25
  data Test7__27 =
    | cons1__34()
    | cons2__35(Test2__23, Test1__22)
  comput Test8__28 =
    | this.destr1__36().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -M -i test_prog.bill
  val<<pol__37>> test9__35 : T__38 =
    unit()
  val<<pol__74>> test9__39 : T__75 =
    bind/cc<<pol__40>> (a__42 : <<T__41>>) ->
      unit().bind<<pol__72>> (x__47 : <<T__46>>) ->
        cmd<<pol__71>> : T__48 val =
         bind/cc<<pol__49>> (b__51 : <<T__50>>) ->
           unit().bind<<pol__61>> (x__56 : <<T__55>>) ->
             x__56.ret(b__51)
        stk =
         this.bind<<pol__70>> (y__65 : <<T__64>>) ->
           x__47.ret(a__42)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -M -i | autobill -M -p
  decl type Test_so__21 : nat
  decl type Test1__22 : +
  type Test2__23 : + = Unit
  type Test3__24 (A__31 : +) (B__32 : -) : - = B__32
  type Test4__25 : - = (Test3__24 Unit Top)
  type Test5__26 (A__33 : -) : - = Test4__25
  data Test7__27 =
    | cons1__34()
    | cons2__35(Test2__23, Test1__22)
  comput Test8__28 =
    | this.destr1__36().ret((Thunk Unit))
  val test9__49 : T__52 = unit()
  val test9__53 : T__89 =
    bind/cc a__56 -> unit()
      .bind (x__61) ->
        cmd
        : T__62 val =
          bind/cc b__65 -> unit().bind (x__70) -> x__70.ret(b__65)
        stk =
          this.bind (y__79) -> x__61.ret(a__56)
        end
