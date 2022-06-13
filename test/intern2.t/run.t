Test the prelude internalizer
  $ autobill polinfer test_prelude.bill
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
  /* constructor "cons1<0>" is :cons1<0>() : test7<6>*/
  /* constructor "cons2<1>" is :cons2<1>(test2<1>, test6<5>) : test7<6>*/
  /* destructor "destr1<0>" is .destr1<0>().ret((shift- unit)) : test8<7>*/
  

Test the program internalizer on name shadowing:
  $ autobill polinfer test_prog.bill
  /* var x<0> : <t9> */
  /* var x<1> : <t14> */
  /* var y<2> : <t17> */
  /* tyvar t6 : + */
  /* tyvar t9 : + */
  /* tyvar t11 : + */
  /* tyvar t14 : + */
  /* tyvar t17 : + */
  term<+> test9<0> : <t5> = unit()
  term<+> test9<1> : <t20> =
    bind/cc+ (ret() : <t6>) -> unit()
      .bind+ (x<0> : <t9>) ->
        step+
          bind/cc+ (ret() : <t11>) -> unit().bind+ (x<1> : <t14>) -> x<1>.ret()
        : <t10>
        into
          this.bind+ (y<2> : <t17>) -> x<0>.ret()
        end

Test both programs:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer
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
  /* constructor "cons1<0>" is :cons1<0>() : test7<6>*/
  /* constructor "cons2<1>" is :cons2<1>(test2<1>, test6<5>) : test7<6>*/
  /* destructor "destr1<0>" is .destr1<0>().ret((shift- unit)) : test8<7>*/
  /* var x<0> : <t12> */
  /* var x<1> : <t17> */
  /* var y<2> : <t20> */
  /* tyvar t9 : + */
  /* tyvar t12 : + */
  /* tyvar t14 : + */
  /* tyvar t17 : + */
  /* tyvar t20 : + */
  term<+> test9<0> : <t8> = unit()
  term<+> test9<1> : <t23> =
    bind/cc+ (ret() : <t9>) -> unit()
      .bind+ (x<0> : <t12>) ->
        step+
          bind/cc+ (ret() : <t14>) -> unit().bind+ (x<1> : <t17>) -> x<1>.ret()
        : <t13>
        into
          this.bind+ (y<2> : <t20>) -> x<0>.ret()
        end


Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer | autobill parse
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
    bind/cc+ -> unit()
      .bind+ x ->
        step+
          bind/cc+ -> unit().bind+ x -> x.ret()
        into
          this.bind+ y -> x.ret()
        end
