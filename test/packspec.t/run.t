Test a simple pack/spec program.

  $ autobill -M -s sorts.bill
  data T__21 (A__25 : +) =
    | c__26()
  comput U__22 (A__27 : -) =
    | this.d__28().ret(A__27)
  decl type C__23 : -
  decl type D__24 : +
  decl val- x__41 : C__23
  decl val+ z__43 : D__24
  val+ test__45 : T__49 =
    c__26()
  val- test0__50 : T__61 =
    match
      | this.d__28().ret(a__52 : T__53) -> x__41.ret(a__52)
    end
  val- test1__62 : T__74 =
    bind/cc- a__65 : T__64 ->
      x__41.d__28().ret(a__65)


  $ autobill -M -t types.bill
  Fatal error: exception Failure("A")
  [2]
