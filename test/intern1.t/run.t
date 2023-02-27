Test the prelude internalizer
  $ autobill -i test_prelude.bill
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
  $ autobill -i test_prog.bill
  val<<pol__2>> test9 : T__3 =
    unit()
  val<<pol__39>> test9 : T__40 =
    bind/cc<<pol__5>> (a : <<T__6>>) ->
      unit().bind<<pol__37>> (x : <<T__11>>) ->
        cmd<<pol__36>> : T__13 val =
          bind/cc<<pol__14>> (b : <<T__15>>) ->
            unit().bind<<pol__26>> (x : <<T__20>>) ->
              x.ret(b)
        stk =
          this.bind<<pol__35>> (y : <<T__29>>) ->
            x.ret(a)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
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
  val test9 : T__18 = unit()
  val test9 : T__55 =
    bind/cc a -> unit()
      .bind (x) ->
        cmd
        : T__28 val =
          bind/cc b -> unit().bind (x) -> x.ret(b)
        stk =
          this.bind (y) -> x.ret(a)
        end
