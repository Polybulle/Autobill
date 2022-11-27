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
  pack t__12 (a__16 : +) : (+ -> +) = c__17[](unit)
  spec u__13 (a__18 : -) : (- -> -) = this.d__19[]().ret() : a__18
  decl type c__14 : -
  decl type d__15 : +
  /* constructor "c__17" is forall (a__16 : +). c__17(unit) : (t__12 a__16)*/
  /* destructor "d__19" is forall (a__18 : -). d__19().ret(a__18) : (u__13
                                                                      a__18)*/
  /* tyvar t__25 : + */
  /* tyvar t__26 : + */
  /* tyvar t__28 : + */
  /* tyvar t__31 : - */
  /* tyvar t__32 : - */
  /* tyvar t__33 : - */
  /* tyvar t__34 : - */
  /* tyvar t__35 : - */
  /* tyvar t__37 : - */
  /* tyvar t__39 : - */
  /* tyvar t__40 : - */
  /* tyvar t__43 : - */
  /* tyvar t__44 : - */
  /* tyvar t__45 : - */
  /* tyvar t__47 : - */
  /* tyvar t__48 : - */
  /* tyvar t__49 : - */
  /* tyvar t__52 : + */
  /* tyvar t__55 : + */
  /* tyvar t__56 : + */
  /* tyvar t__57 : + */
  /* tyvar t__60 : + */
  /* tyvar t__61 : + */
  /* tyvar t__62 : + */
  /* tyvar t__63 : + */
  /* tyvar t__64 : + */
  /* tyvar t__66 : + */
  /* tyvar t__67 : + */
  /* tyvar t__70 : + */
  /* tyvar t__73 : + */
  /* tyvar t__74 : + */
  /* tyvar t__75 : + */
  /* tyvar t__76 : + */
  /* tyvar t__79 : + */
  /* tyvar t__80 : + */
  /* tyvar t__81 : + */
  /* tyvar t__82 : + */
  /* tyvar t__83 : + */
  /* tyvar t__85 : + */
  /* tyvar t__86 : + */
  /* tyvar t__93 : + */
  /* tyvar t__99 : - */
  /* tyvar t__117 : - */
  /* tyvar t__130 : + */
  /* tyvar t__144 : + */
  /* tyvar t__150 : + */
  /* var y__59 used ? : t__60 */
  /* var x__78 used ? : t__79 */
  /* cont a__30 used ? : t__31 */
  decl val- x__20 : c__14
  decl val+ z__22 : d__15
  val+ y__24 : t__28 = c__17[](unit())
  val- y__29 : t__39 = match this.d__19[]()a__30 : t__31 -> x__20.ret(a__30)
  cmd- anon__42 ret a__41 : t__40 = x__20.d__19[]().ret(a__41)
  cmd+ anon__54 ret a__53 : t__52 =
    z__22.match c__17[](y__59 : t__60) -> z__22.ret(a__53)
  cmd+ anon__72 ret a__71 : t__70 =
    cmd+
    : t__73 val =
      c__17[](unit())
    stk =
      this.match c__17[](x__78 : t__79) -> x__78.ret(a__71)
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
  spec id__12 : - =
    this.inst__14[(a__13 : +)]().ret() : (fun (thunk a__13) a__13)
  /* destructor "inst__14" is exists (a__13 : +). inst__14().ret((fun
                                                                   (thunk
                                                                     a__13)
                                                                   a__13)) : 
      (id__12 )*/
  /* tyvar b__16 : + */
  /* tyvar t__18 : - */
  /* tyvar t__19 : - */
  /* tyvar t__20 : - */
  /* tyvar t__21 : - */
  /* tyvar t__23 : + */
  /* tyvar t__25 : - */
  /* tyvar t__26 : - */
  /* tyvar t__27 : - */
  /* tyvar t__28 : - */
  /* tyvar t__29 : + */
  /* tyvar t__30 : - */
  /* tyvar t__32 : - */
  /* tyvar t__34 : - */
  /* tyvar t__36 : - */
  /* tyvar t__39 : + */
  /* tyvar t__58 : + */
  /* tyvar t__83 : + */
  /* var x__22 used 1 : t__23 */
  /* cont a__17 used 1 : t__18 */
  /* cont b__24 used 1 : t__25 */
  val- id2__15 : id__12 =
    match this.inst__14[(t__23 : +)]()a__17 : (fun (thunk t__23) t__23) ->
    cmd-
    : (fun (thunk t__23) t__23) val =
      match
        case this.call(x__22 : t__23)b__24 : (thunk t__23) -> thunk(x__22)
          .ret(b__24)
      end
    stk =
      this.ret(a__17)
    end
