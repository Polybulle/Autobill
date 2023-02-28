Test the prelude internalizer
  $ autobill -M -i test_prelude.bill
  decl sort nat
  
  decl type Test_so : nat
  decl type Test1 : +
  type Test2 : + = Unit
  type Test3 (A : +) (B : -) : - = B
  type Test4 : - = (Test3 Unit Top)
  type Test5 (A : -) : - = Test4
  data Test7 =
    | cons1()
    | cons2(Test2, Test1)
  comput Test8 =
    | this.destr1().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -M -i test_prog.bill
  val<<pol__14>> test9 : T__15 =
    unit()
  val<<pol__51>> test9 : T__52 =
    bind/cc<<pol__17>> (a : <<T__18>>) ->
      unit().bind<<pol__49>> (x : <<T__23>>) ->
        cmd<<pol__48>> : T__25 val =
          bind/cc<<pol__26>> (b : <<T__27>>) ->
            unit().bind<<pol__38>> (x : <<T__32>>) ->
              x.ret(b)
        stk =
          this.bind<<pol__47>> (y : <<T__41>>) ->
            x.ret(a)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -M -i | autobill -M -p
  decl sort nat
  decl type Test_so : nat
  decl type Test1 : +
  type Test2 : + = Unit
  type Test3 (A : +) (B : -) : - = B
  type Test4 : - = (Test3 Unit Top)
  type Test5 (A : -) : - = Test4
  data Test7 =
    | cons1()
    | cons2(Test2, Test1)
  comput Test8 =
    | this.destr1().ret((Thunk Unit))
  val test9 : T__30 = unit()
  val test9 : T__67 =
    bind/cc a -> unit()
      .bind (x) ->
        cmd
        : T__40 val =
          bind/cc b -> unit().bind (x) -> x.ret(b)
        stk =
          this.bind (y) -> x.ret(a)
        end
