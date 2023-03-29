Test a simple pack/spec program.

  $ autobill -M -s sorts.bill
  data T__21 (A__27 : +) =
    | c__28()
  comput U__22 (A__29 : -) =
    | this.d__30().ret(A__29)
  decl type C__23 : -
  decl type D__24 : +
  decl val- x__43 : C__23
  decl val+ z__45 : D__24
  val+ test__47 : T__51 =
    c__28()
  val- test0__52 : T__63 =
    match
      | this.d__30().ret(a__54 : T__55) -> x__43.ret(a__54)
    end
  val- test1__64 : T__76 =
    bind/cc- a__67 : T__66 ->
      x__43.d__30().ret(a__67)


  $ autobill -M -t types.bill
  Fatal error: exception Autobill.Intern_common.Bad_sort("The parameters here must have a base parameter sort", _)
  [2]
