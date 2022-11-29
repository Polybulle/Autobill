Test the prelude internalizer
  $ autobill -i test_prelude.bill
  decl type test_so : nat
  decl type test1 : +
  type test2 : + = unit
  type test3 (a : +) (b : -) : (+ -> (- -> -)) = b
  type test4 : - = (test3 unit top)
  type test5 (a : -) : (- -> -) = test4
  data test7 =
    case cons1()
    case cons2(test2, test1)
  comput test8 =
    case this.destr1().ret() : (thunk unit)
  /* constructor "cons1" is cons1() : (test7 )*/
  /* constructor "cons2" is cons2(test2, test1) : (test7 )*/
  /* destructor "destr1" is destr1().ret((thunk unit)) : (test8 )*/

Test the program internalizer on name shadowing:
  $ autobill -i test_prog.bill
  val<pol__14> test9 : t__15 = unit()
  val<pol__56> test9 : t__57 =
    bind/cc<pol__17> a : t__18 -> unit()
      .bind<pol__54> (x : t__24) ->
        cmd<pol__53>
        : t__26 val =
          bind/cc<pol__28> b : t__29 -> unit()
            .bind<pol__42> (x : t__35) -> x.ret(b)
        stk =
          this.bind<pol__52> (y : t__45) -> x.ret(a)
        end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill -i | autobill -p
  decl type test_so : nat
  decl type test1 : +
  type test2 : + = unit
  type test3 (a : +) (b : -) : (+ -> (- -> -)) = b
  type test4 : - = (test3 unit top)
  type test5 (a : -) : (- -> -) = test4
  data test7 =
    case cons1()
    case cons2(test2, test1)
  comput test8 =
    case this.destr1().ret() : (thunk unit)
  val test9 : t__30 = unit()
  val test9 : t__72 =
    bind/cc a : t__33 -> unit()
      .bind (x : t__39) ->
        cmd
        : t__41 val =
          bind/cc b : t__44 -> unit().bind (x : t__50) -> x.ret(b)
        stk =
          this.bind (y : t__60) -> x.ret(a)
        end
