Test the prelude internalizer
  $ autobill intern test_prelude.bill
  decl type test1<12> : (-) -> +
  type test2<13> : + = unit
  type test3<14> (a<20> : +) (b<21> : -) : - = b<21>
  type test4<15> : - = (test3<14> unit top)
  type test5<16> (a<22> : -) : - = test4<15>
  type test6<17> : + = (test1<12> test4<15>)
  data test7<18> =
    case :cons1<23>()
    case :cons2<24>(test2<13>, test6<17>)
  codata test8<19> =
    case this.destr1<25>().ret() : (shift- unit)
  /* constructor "cons1<23>" is cons1<23>() : test7<18>*/
  /* constructor "cons2<24>" is cons2<24>(test2<13>, test6<17>) : test7<18>*/
  /* destructor "destr1<25>" is destr1<25>().ret((shift- unit)) : test8<19>*/
  

Test the program internalizer on name shadowing:
  $ autobill intern test_prog.bill
  term<pol<14>> test9<12> : <t<15>> = unit()
  term<pol<49>> test9<16> : <t<50>> =
    bind/cc<pol<17>> (ret() : <t<18>>) ->
      unit().bind<pol<47>> (x<22> : <t<23>>) ->
              step<pol<46>>
                bind/cc<pol<26>> (ret() : <t<27>>) ->
                  unit().bind<pol<37>> (x<31> : <t<32>>) ->
                          x<31>.ret()
              : <t<24>>
              into
                this.bind<pol<45>> (y<39> : <t<40>>) ->
                      x<22>.ret()
              end
Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill intern | autobill parse
  Fatal error: exception Failure("15:13:Lexing failed because of unexpected >")
  [2]
