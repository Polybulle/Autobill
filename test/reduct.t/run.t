Test that reduction works
  $ autobill -L -r <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a)
  > EOF
  cmd- anon_37 ret a_36 : Top =
    cmd- : Top val =
      GOT_TOP
    stk =
      this.ret(a_36)

Test reduction with declarations
  $ autobill -r -L <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a)
  > EOF
  decl val- y_35 : Top
  cmd- anon_39 ret a_38 : Top =
    cmd- : Top val =
      y_35
    stk =
      this.ret(a_38)

Test shifting
  $ autobill -r -L <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = match this.thunk().ret(b) -> x.ret(b) in
  >   y.ret(a)
  cmd- anon_37 ret a_36 : (Thunk Unit) =
    cmd- : (Thunk Unit) val =
      match this.thunk().ret(a_230 : Unit) ->
      cmd+ : Unit val =
        unit()
      stk =
        this.ret(a_230)
    stk =
      this.ret(a_36)

Test function calls
  $ autobill -r -L swap.bill
  decl type A_21 : +
  decl type B_22 : +
  decl val+ x0_37 : A_21
  decl val+ y0_39 : B_22
  cmd+ anon_43 ret a_42 : (B_22 * A_21) =
    cmd+ : (B_22 * A_21) val =
      tuple(y0_39, x0_37)
    stk =
      this.ret(a_42)
