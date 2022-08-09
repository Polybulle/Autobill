Test the prelude internalizer
  $ autobill polinfer test_prelude.bill
  decl type test1<0> : (-) -> +
  type test2<1> : + = unit
  type test3<2> (a<10> : +) (b<11> : -) : - = b<11>
  type test4<3> : - = (test3<2> unit top)
  type test5<4> (a<12> : -) : - = test4<3>
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
  /* var x<2> : <t15> */
  /* var x<3> : <t20> */
  /* var y<4> : <t23> */
  /* tyvar t12 : + */
  /* tyvar t15 : + */
  /* tyvar t17 : + */
  /* tyvar t20 : + */
  /* tyvar t23 : + */
  term<+> test9<0> : <t11> = unit()
  term<+> test9<1> : <t26> =
    bind/cc+ (ret() : <t12>) -> unit()
      .bind+ (x<2> : <t15>) ->
        step+
          bind/cc+ (ret() : <t17>) -> unit().bind+ (x<3> : <t20>) -> x<3>.ret()
        : <t16>
        into
          this.bind+ (y<4> : <t23>) -> x<2>.ret()
        end

Test both programs:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer
  decl type test1<0> : (-) -> +
  type test2<1> : + = unit
  type test3<2> (a<10> : +) (b<11> : -) : - = b<11>
  type test4<3> : - = (test3<2> unit top)
  type test5<4> (a<12> : -) : - = test4<3>
  type test6<5> : + = (test1<0> test4<3>)
  data test7<6> =
    case :cons1<0>()
    case :cons2<1>(test2<1>, test6<5>)
  codata test8<7> =
    case this.destr1<0>().ret() : (shift- unit)
  /* constructor "cons1<0>" is :cons1<0>() : test7<6>*/
  /* constructor "cons2<1>" is :cons2<1>(test2<1>, test6<5>) : test7<6>*/
  /* destructor "destr1<0>" is .destr1<0>().ret((shift- unit)) : test8<7>*/
  /* var x<2> : <t18> */
  /* var x<3> : <t23> */
  /* var y<4> : <t26> */
  /* tyvar t15 : + */
  /* tyvar t18 : + */
  /* tyvar t20 : + */
  /* tyvar t23 : + */
  /* tyvar t26 : + */
  term<+> test9<0> : <t14> = unit()
  term<+> test9<1> : <t29> =
    bind/cc+ (ret() : <t15>) -> unit()
      .bind+ (x<2> : <t18>) ->
        step+
          bind/cc+ (ret() : <t20>) -> unit().bind+ (x<3> : <t23>) -> x<3>.ret()
        : <t19>
        into
          this.bind+ (y<4> : <t26>) -> x<2>.ret()
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
