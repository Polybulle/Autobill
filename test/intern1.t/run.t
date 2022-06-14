Test the prelude internalizer
  $ autobill intern test_prelude.bill
  decl type test1<0> : (-) -> +
  type test2<1> : + = unit
  type test3<2> (a<4> : +) (b<5> : -) : - = b<5>
  type test4<3> : - = (test3<2> unit top)
  type test5<4> (a<6> : -) : - = test4<3>
  type test6<5> : + = (test1<0> test4<3>)
  data test7<6> =
    case :cons1<0>()
    case :cons2<1>(test2<1>, test6<5>)
  codata test8<7> =
    case this.destr1<0>().ret() : (shift- unit)
  /* constructor "cons1<0>" is :cons1<0>() : test7<6> */
  /* constructor "cons2<1>" is :cons2<1>(test2<1>, test6<5>) : test7<6> */
  /* destructor "destr1<0>" for test8<7> is
      this.destr1<0>().ret() : (shift- unit) */
  

Test the program internalizer on name shadowing:
  $ autobill intern test_prog.bill
  term<pol2> test9<0> : <t5> = unit()
  term<pol13> test9<1> : <t20> =
    bind/cc<pol3> (ret() : <t6>) ->
      unit().bind<pol11> (x<2> : <t9>) ->
              step<pol10>
                bind/cc<pol4> (ret() : <t11>) ->
                  unit().bind<pol6> (x<3> : <t14>) ->
                          x<3>.ret()
              : <t10>
              into
                this.bind<pol9> (y<4> : <t17>) ->
                      x<2>.ret()
              end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill intern | autobill parse
  decl type test1 : (-) -> +
  type test2 : + = unit
  type test3 (a : +) (b : -) : - = b
  type test4 : - = (test3 unit top)
  type test5 (a : -) : - = test4
  type test6 : + = (test1 test4)
  data test7 =
    case :cons1()
    case :cons2(test2, test6)
  codata test8 =
    case this.destr1().ret() : (shift- unit)
  term test9 = unit()
  term test9 =
    bind/cc -> unit()
      .bind x ->
        step
          bind/cc -> unit().bind x -> x.ret()
        into
          this.bind y -> x.ret()
        end
