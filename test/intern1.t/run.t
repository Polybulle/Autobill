Test the prelude internalizer
  $ autobill -i test_prelude.bill
  decl type Test_so : nat
  decl type Test1 : +
  type Test2 : + = Unit
  type Test3 (A : +) (B : -) : (+ -> (- -> -)) = B
  type Test4 : - = (Test3 Unit Top)
  type Test5 (A : -) : (- -> -) = Test4
  data Test7 =
    | cons1()
    | cons2(Test2, Test1)
  comput Test8 =
    | this.destr1().ret((Thunk Unit))

Test the program internalizer on name shadowing:
  $ autobill -i test_prog.bill
  val<<pol__14>> test9 : T__15 = unit()
  val<<pol__56>> test9 : T__57 =
    bind/cc<<pol__17>> a : T__18 -> unit()
      .bind<<pol__54>> (x : T__24) ->
        cmd<<pol__53>> : T__26
        val =
          bind/cc<<pol__28>> b : T__29 -> unit()
            .bind<<pol__42>> (x : T__35) -> x.ret(b)
        stk =
          this.bind<<pol__52>> (y : T__45) -> x.ret(a)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
  decl type Test_so : nat
  decl type Test1 : +
  type Test2 : + = Unit
  type Test3 (A : +) (B : -) : (+ -> (- -> -)) = B
  type Test4 : - = (Test3 Unit Top)
  type Test5 (A : -) : (- -> -) = Test4
  data Test7 =
    | cons1()
    | cons2(Test2, Test1)
  comput Test8 =
    | this.destr1().ret((Thunk Unit))
  val test9 : T__30 = unit()
  val test9 : T__72 =
    bind/cc a : T__33 -> unit()
      .bind (x : T__39) ->
        cmd
        : T__41 val =
          bind/cc b : T__44 -> unit().bind (x : T__50) -> x.ret(b)
        stk =
          this.bind (y : T__60) -> x.ret(a)
        end
