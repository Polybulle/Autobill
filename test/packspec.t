Test a simple pack/spec program.

  $ autobill -r -s <<EOF
  > pack t (a : +) = c[](unit)
  > spec u (a : -) = this.d[]().ret() : a
  > 
  > decl type c : -
  > decl term x : c
  > decl type d : +
  > decl term z : d
  > 
  > term y = c[](unit())
  > term y = match this.d[]().ret(a) -> x.ret(a)
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
  decl term<-> x__14 : c__8
  decl term<+> z__16 : d__9
  term<+> y__18 : t__22 =
    bind/cc+.ret(a__152 : t__22) ->
      step+
        c__11[](unit())
      : t__22
      into
        this.ret(a__152)
      end
  term<-> y__23 : t__33 =
    bind/cc-.ret(a__155 : t__33) ->
      step-
        match this.d__13[]()..ret(a__24 : t__25) -> x__14.ret(a__24)
      : t__33
      into
        this.ret(a__155)
      end
  cmd<-> anon__36 ret a__35 : t__34 = x__14.d__13[]().ret(a__35)
  cmd<+> anon__48 ret a__47 : t__46 =
    z__16match c__11[](y__53 : t__54) -> z__16.ret(a__47)
  cmd<+> anon__66 ret a__65 : t__64 = unit().ret(a__65)


  $ autobill -t <<EOF
  > spec id = this.inst[a : +]().ret() : (fun a -> (shift- a))
  > term id2 =
  >  match this.inst[b:+]().ret(a) ->
  >  step
  >    match this.call(x).ret(b) ->
  >    step match this.shift-().ret(c) ->
  >      x.ret(c)
  >    into
  >      this.ret(b)
  >    end
  >  into
  >    this.ret(a)
  >  end
  spec id__6 : - =
    this.inst__8[(a__7 : +)]().ret() : (fun a__7 -> (shift- a__7))
  /* destructor "inst__8" is exists (a__7 : +). inst__8().ret((fun a__7
                                                                -> (shift-
                                                                     a__7))) : id__6*/
  term<-> id2__9 : id__6 =
    match this.inst__8[(t__17 : +)]()..ret(a__11
                                             : (fun t__17 -> (shift- t__17))) ->
    step-
      match
        case this.call(x__16 : t__17).ret(b__18 : (shift- t__17)) ->
          step-
            match
              case this.shift-().ret(c__23 : t__17) -> x__16.ret(c__23)
            end
          : (shift- t__17)
          into
            this.ret(b__18)
          end
      end
    : (fun t__17 -> (shift- t__17))
    into
      this.ret(a__11)
    end
