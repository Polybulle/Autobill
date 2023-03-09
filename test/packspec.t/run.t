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
  val- y__48 : T__58 =
    match
      | this.d__26().ret(a__50 : T__51) -> x__39.ret(a__50)
    end
  cmd- anon__61 ret a__60 : T__59 =
    x__39.d__26().ret(a__60)
  cmd+ anon__72 ret a__71 : T__70 =
    z__41.match
      | c__24() -> z__41.ret(a__71)
      end
  cmd- anon__86 ret a__85 : T__84 =
    c__24().match
      | c__24() -> x__39.ret(a__85)
      end


  $ autobill -M -t types.bill
  Fatal error: exception Failure("A")
  [2]
