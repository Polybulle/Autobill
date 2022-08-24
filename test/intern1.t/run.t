Test the prelude internalizer
  $ autobill intern test_prelude.bill
  decl type test1<12> : +
  type test2<13> : + = unit
  type test3<14> (a<19> : +) (b<20> : -) : - = b<20>
  type test4<15> : - = (test3<14> unit top)
  type test5<16> (a<21> : -) : - = test4<15>
  data test7<17> =
    case :cons1<22>()
    case :cons2<23>(test2<13>, test1<12>)
  codata test8<18> =
    case this.destr1<24>().ret() : (shift- unit)
  /* constructor "cons1<22>" is cons1<22>() : test7<17>*/
  /* constructor "cons2<23>" is cons2<23>(test2<13>, test1<12>) : test7<17>*/
  /* destructor "destr1<24>" is destr1<24>().ret((shift- unit)) : test8<18>*/
  

Test the program internalizer on name shadowing:
  $ autobill intern test_prog.bill
  term<pol_14> test9<12> : t<15> = unit()
  term<pol_49> test9<16> : t<50> =
    bind/cc<pol_17> (ret() : t<18>) ->
      unit().bind<pol_47> (x<22> : t<23>) ->
              step<pol_46>
                bind/cc<pol_26> (ret() : t<27>) ->
                  unit().bind<pol_37> (x<31> : t<32>) ->
                          x<31>.ret()
              : t<24>
              into
                this.bind<pol_45> (y<39> : t<40>) ->
                      x<22>.ret()
              end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill intern | autobill parse
  decl type test1 : +
  type test2 : + = unit
  type test3 (a : +) (b : -) : - = b
  type test4 : - = (test3 unit top)
  type test5 (a : -) : - = test4
  data test7 =
    case :cons1()
    case :cons2(test2, test1)
  codata test8 =
    case this.destr1().ret() : (shift- unit)
  term test9 : t = unit()
  term test9 : t =
    bind/cc (ret() : t) -> unit()
      .bind (x : t) ->
        step
          bind/cc (ret() : t) -> unit().bind (x : t) -> x.ret()
        : t
        into
          this.bind (y : t) -> x.ret()
        end
