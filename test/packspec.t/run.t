Test a simple pack/spec program.

  $ autobill -M -s sorts.bill
  data T__19 (A__23 : +) =
    | c__24()
  comput U__20 (A__25 : -) =
    | this.d__26().ret(A__25)
  decl type C__21 : -
  decl type D__22 : +
  decl val- x__39 : C__21
  decl val+ z__41 : D__22
  val+ y__43 : T__47 =
    c__24()
  val- y__48 : T__59 =
    match
      | this.d__26().ret(a__50 : T__51) -> x__39.ret(a__50)
    end
  cmd- anon__62 ret a__61 : T__60 =
    x__39.d__26().ret(a__61)
  cmd+ anon__73 ret a__72 : T__71 =
    z__41.match
      | c__24() -> z__41.ret(a__72)
      end
  cmd- anon__88 ret a__87 : T__86 =
    c__24().match
      | c__24() -> x__39.ret(a__87)
      end


  $ autobill -M -t types.bill
  Fatal error: exception Failure("A")
  [2]
