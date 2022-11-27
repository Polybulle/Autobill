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
  pack t__6 (a__10 : +) : (+ -> +) = c__11[](unit)
  spec u__7 (a__12 : -) : (- -> -) = this.d__13[]().ret() : a__12
  decl type c__8 : -
  decl type d__9 : +
  /* constructor "c__11" is forall (a__10 : +). c__11(unit) : (t__6 a__10)*/
  /* destructor "d__13" is forall (a__12 : -). d__13().ret(a__12) : (u__7
                                                                      a__12)*/
  /* tyvar t__19 : + */
  /* tyvar t__20 : + */
  /* tyvar t__22 : + */
  /* tyvar t__25 : - */
  /* tyvar t__26 : - */
  /* tyvar t__27 : - */
  /* tyvar t__28 : - */
  /* tyvar t__29 : - */
  /* tyvar t__31 : - */
  /* tyvar t__33 : - */
  /* tyvar t__34 : - */
  /* tyvar t__37 : - */
  /* tyvar t__38 : - */
  /* tyvar t__39 : - */
  /* tyvar t__41 : - */
  /* tyvar t__42 : - */
  /* tyvar t__43 : - */
  /* tyvar t__46 : + */
  /* tyvar t__49 : + */
  /* tyvar t__50 : + */
  /* tyvar t__51 : + */
  /* tyvar t__54 : + */
  /* tyvar t__55 : + */
  /* tyvar t__56 : + */
  /* tyvar t__57 : + */
  /* tyvar t__58 : + */
  /* tyvar t__60 : + */
  /* tyvar t__61 : + */
  /* tyvar t__64 : + */
  /* tyvar t__67 : + */
  /* tyvar t__68 : + */
  /* tyvar t__69 : + */
  /* tyvar t__70 : + */
  /* tyvar t__73 : + */
  /* tyvar t__74 : + */
  /* tyvar t__75 : + */
  /* tyvar t__76 : + */
  /* tyvar t__77 : + */
  /* tyvar t__79 : + */
  /* tyvar t__80 : + */
  /* var y__53 used ? : t__54 */
  /* var x__72 used ? : t__73 */
  /* cont a__24 used ? : t__25 */
  decl val<-> x__14 : c__8
  decl val<+> z__16 : d__9
  val<+> y__18 : t__22 = c__11[](unit())
  val<-> y__23 : t__33 =
    match this.d__13[]().ret(a__24 : t__25) -> x__14.ret(a__24)
  cmd<-> anon__36 ret a__35 : t__34 = x__14.d__13[]().ret(a__35)
  cmd<+> anon__48 ret a__47 : t__46 =
    z__16.match c__11[](y__53 : t__54) -> z__16.ret(a__47)
  cmd<+> anon__66 ret a__65 : t__64 =
    cmd+
    : t__67 val =
      c__11[](unit())
    stk =
      this.match c__11[](x__72 : t__73) -> x__72.ret(a__65)
    end


  $ autobill -t <<EOF
  > spec id = this.inst[a : +]().ret() : (fun (thunk a) a)
  > val id2 =
  >  match this.inst[b:+]().ret(a) ->
  >  cmd val =
  >    match this.call(x).ret(b) ->
  >    thunk(x).ret(b)
  >  stk =
  >    this.ret(a)
  >  end
  Fatal error: exception Autobill.Intern_common.Undefined_type("a__7", _)
  [2]
