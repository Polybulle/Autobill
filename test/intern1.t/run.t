Test the prelude internalizer
  $ autobill -i test_prelude.bill
  decl type test_so__7 : nat__6
  decl type test1__8 : +
  type test2__9 : + = unit
  type test3__10 (a__15 : +) (b__16 : -) : (+ -> (- -> -)) = b__16
  type test4__11 : - = (test3__10 unit top)
  type test5__12 (a__17 : -) : (- -> -) = test4__11
  data test7__13 =
    case cons1__18()
    case cons2__19(test2__9, test1__8)
  comput test8__14 =
    case this.destr1__20().ret() : (thunk unit)
  /* constructor "cons1__18" is cons1__18() : test7__13*/
  /* constructor "cons2__19" is cons2__19(test2__9, test1__8) : test7__13*/
  /* destructor "destr1__20" is destr1__20().ret((thunk unit)) : test8__14*/

Test the program internalizer on name shadowing:
  $ autobill -i test_prog.bill
  val<pol__8> test9__6 : <t__9> = unit()
  val<pol__50> test9__10 : <t__51> =
    bind/cc<pol__11>
      a__13 : <t__12> ->
      unit().bind<pol__48> (x__19 : <t__18>) ->
              cmd<pol__47>
              : <t__20> val =
                bind/cc<pol__22>
                  b__24 : <t__23> ->
                  unit().bind<pol__36> (x__30 : <t__29>) ->
                          x__30.ret(b__24)
              stk =
                this.bind<pol__46> (y__40 : <t__39>) ->
                      x__19.ret(a__13)
              end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
  decl type test_so__7 : nat__6
  decl type test1__8 : +
  type test2__9 : + = unit
  type test3__10 (a__15 : +) (b__16 : -) : (+ -> (- -> -)) = b__16
  type test4__11 : - = (test3__10 unit top)
  type test5__12 (a__17 : -) : (- -> -) = test4__11
  data test7__13 =
    case cons1__18()
    case cons2__19(test2__9, test1__8)
  comput test8__14 =
    case this.destr1__20().ret() : (thunk unit)
  val test9__21 = unit()
  val test9__25 =
    bind/cc a__28 -> unit()
      .bind x__34 ->
        cmd val =
          bind/cc b__39 -> unit().bind x__45 -> x__45.ret(b__39)
        stk =
          this.bind y__55 -> x__34.ret(a__28)
        end
