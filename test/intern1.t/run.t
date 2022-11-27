Test the prelude internalizer
  $ autobill -i test_prelude.bill
  decl type test_so__13 : nat__12
  decl type test1__14 : +
  type test2__15 : + = unit
  type test3__16 (a__21 : +) (b__22 : -) : (+ -> (- -> -)) = b__22
  type test4__17 : - = (test3__16 unit top)
  type test5__18 (a__23 : -) : (- -> -) = test4__17
  data test7__19 =
    case cons1__24()
    case cons2__25(test2__15, test1__14)
  comput test8__20 =
    case this.destr1__26().ret() : (thunk unit)
  /* constructor "cons1__24" is cons1__24() : (test7__19 )*/
  /* constructor "cons2__25" is cons2__25(test2__15,
      test1__14) : (test7__19 )*/
  /* destructor "destr1__26" is destr1__26().ret((thunk unit)) : (test8__20 )*/

Test the program internalizer on name shadowing:
  $ autobill -i test_prog.bill
  val<pol__14> test9__12 : t__15 = unit()
  val<pol__56> test9__16 : t__57 =
    bind/cc<pol__17> a__19 : t__18 -> unit()
      .bind<pol__54> (x__25 : t__24) ->
        cmd<pol__53>
        : t__26 val =
          bind/cc<pol__28> b__30 : t__29 -> unit()
            .bind<pol__42> (x__36 : t__35) -> x__36.ret(b__30)
        stk =
          this.bind<pol__52> (y__46 : t__45) -> x__25.ret(a__19)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
  decl type test_so__13 : nat__12
  decl type test1__14 : +
  type test2__15 : + = unit
  type test3__16 (a__21 : +) (b__22 : -) : (+ -> (- -> -)) = b__22
  type test4__17 : - = (test3__16 unit top)
  type test5__18 (a__23 : -) : (- -> -) = test4__17
  data test7__19 =
    case cons1__24()
    case cons2__25(test2__15, test1__14)
  comput test8__20 =
    case this.destr1__26().ret() : (thunk unit)
  val test9__27 : t__30 = unit()
  val test9__31 : t__72 =
    bind/cc a__34 : t__33 -> unit()
      .bind (x__40 : t__39) ->
        cmd
        : t__41 val =
          bind/cc b__45 : t__44 -> unit()
            .bind (x__51 : t__50) -> x__51.ret(b__45)
        stk =
          this.bind (y__61 : t__60) -> x__40.ret(a__34)
        end
