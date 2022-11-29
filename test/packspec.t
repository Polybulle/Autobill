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
  pack t (a : +) : (+ -> +) = c[](unit)
  spec u (a : -) : (- -> -) = this.d[]().ret() : a
  decl type c : -
  decl type d : +
  /* constructor "c" is forall (a : +). c(unit) : (t a)*/
  /* destructor "d" is forall (a : -). d().ret(a) : (u a)*/
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
  /* var y used ? : t__60 */
  /* var x used ? : t__79 */
  /* cont a used ? : t__31 */
  decl val- x : c
  decl val+ z : d
  val+ y : t__28 = c[](unit())
  val- y : t__39 = match this.d[]()a : t__31 -> x.ret(a)
  cmd- anon ret a : t__40 = x.d[]().ret(a)
  cmd+ anon ret a : t__52 = z.match c[](y : t__60) -> z.ret(a)
  cmd+ anon ret a : t__70 =
    cmd+
    : t__73 val =
      c[](unit())
    stk =
      this.match c[](x : t__79) -> x.ret(a)
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
  spec id : - = this.inst[(a : +)]().ret() : (fun (thunk a) a)
  /* destructor "inst" is exists (a : +). inst().ret((fun (thunk a) a)) : 
      (id )*/
  /* tyvar b : + */
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
  /* var id2 used 0 : id */
  /* var x used 1 : t__23 */
  /* cont a used 1 : (fun (thunk t__23) t__23) */
  /* cont b used 1 : (thunk t__23) */
  val- id2 : id =
    match this.inst[(t__23 : +)]()a : (fun (thunk t__23) t__23) ->
    cmd-
    : (fun (thunk t__23) t__23) val =
      match
        case this.call(x : t__23)b : (thunk t__23) -> thunk(x).ret(b)
      end
    stk =
      this.ret(a)
    end
