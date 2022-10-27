Test the prelude internalizer
  $ autobill -i test_prelude.bill
  decl type test1__6 : +
  type test2__7 : + = unit
  type test3__8 (a__13 : +) (b__14 : -) : - = b__14
  type test4__9 : - = (test3__8 unit top)
  type test5__10 (a__15 : -) : - = test4__9
  data test7__11 =
    case cons1__16()
    case cons2__17(test2__7, test1__6)
  comput test8__12 =
    case this.destr1__18().ret() : (shift- unit)
  /* constructor "cons1__16" is cons1__16() : test7__11*/
  /* constructor "cons2__17" is cons2__17(test2__7, test1__6) : test7__11*/
  /* destructor "destr1__18" is destr1__18().ret((shift- unit)) : test8__12*/

Test the program internalizer on name shadowing:
  $ autobill -i test_prog.bill
  val<pol_8> test9__6 : <t__9> = unit()
  val<pol_50> test9__10 : <t__51> =
    bind/cc<pol_11>
      a__13 : <t__12> ->
      unit().bind<pol_48> (x__19 : <t__18>) ->
              cmd<pol_47>
              : <t__20> val =
                bind/cc<pol_22>
                  b__24 : <t__23> ->
                  unit().bind<pol_36> (x__30 : <t__29>) ->
                          x__30.ret(b__24)
              stk =
                this.bind<pol_46> (y__40 : <t__39>) ->
                      x__19.ret(a__13)
              end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
  decl type test1__6 : +
  type test2__7 : + = unit
  type test3__8 (a__13 : +) (b__14 : -) : - = b__14
  type test4__9 : - = (test3__8 unit top)
  type test5__10 (a__15 : -) : - = test4__9
  data test7__11 =
    case cons1__16()
    case cons2__17(test2__7, test1__6)
  comput test8__12 =
    case this.destr1__18().ret() : (shift- unit)
  val test9__19 = unit()
  val test9__23 =
    bind/cc a__26 -> unit()
      .bind x__32 ->
        cmd val =
          bind/cc b__37 -> unit().bind x__43 -> x__43.ret(b__37)
        stk =
          this.bind y__53 -> x__32.ret(a__26)
        end
