Test a simple pack/spec program.

  $ autobill -r -s <<EOF
  > pack t (a : +) = c[](unit)
  > spec u (a : -) = this.d[]().ret() : a
  > 
  > decl type c : -
  > decl val x : c
  > decl type d : +
  > decl val z : d
  > 
  > val y = c[](unit())
  > val y = match this.d[]().ret(a) -> x.ret(a)
  > cmd ret a = x.d[]().ret(a)
  > cmd ret a = z.match c[](y) -> z.ret(a)
  > cmd ret a = c[](unit()).match c[](x) -> x.ret(a)
  pack t__6 (a__10 : +) : + = c__11[](unit)
  spec u__7 (a__12 : -) : - = this.d__13[]().ret() : a__12
  decl type c__8 : -
  decl type d__9 : +
  /* constructor "c__11" is forall (a__10 : +). c__11(unit) : (t__6 a__10)*/
  /* destructor "d__13" is forall (a__12 : -). d__13().ret(a__12) : (u__7
                                                                      a__12)*/
  decl val<-> x__14 : c__8
  decl val<+> z__16 : d__9
  val<+> y__18 : t__22 = c__11[](unit())
  val<-> y__23 : t__33 =
    match this.d__13[]()..ret(a__24 : t__25) -> x__14.ret(a__24)
  cmd<-> anon__36 ret a__35 : t__34 = x__14.d__13[]().ret(a__35)
  cmd<+> anon__48 ret a__47 : t__46 =
    z__16match c__11[](y__53 : t__54) -> z__16.ret(a__47)
  cmd<+> anon__66 ret a__65 : t__64 =
    cmd+
    : t__67 val =
      c__11[](unit())
    stk =
      thismatch c__11[](x__72 : t__73) -> x__72.ret(a__65)
    end


  $ autobill -t <<EOF
  > spec id = this.inst[a : +]().ret() : (fun a -> (thunk a))
  > val id2 =
  >  match this.inst[b:+]().ret(a) ->
  >  cmd val =
  >    match this.call(x).ret(b) ->
  >    thunk(x).ret(b)
  >  stk =
  >    this.ret(a)
  >  end
  spec id__6 : - =
    this.inst__8[(a__7 : +)]().ret() : (fun a__7 -> (thunk a__7))
  /* destructor "inst__8" is exists (a__7 : +). inst__8().ret((fun a__7
                                                                -> (thunk a__7))) : id__6*/
  val<-> id2__9 : id__6 =
    match this.inst__8[(t__17 : +)]()..ret(a__11 : (fun t__17 -> (thunk t__17))) ->
    cmd-
    : (fun t__17 -> (thunk t__17)) val =
      match
        case this.call(x__16 : t__17).ret(b__18 : (thunk t__17)) ->
          thunk(x__16).ret(b__18)
      end
    stk =
      this.ret(a__11)
    end
