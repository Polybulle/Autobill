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
