Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd<<->> anon ret a : T__12 =
    cmd<<->> : T__15
    val =
      GOT_TOP
    stk =
      this.bind<<->> (x : T__18) -> x.ret(a)
    end

Test reduction with declarations
  $ autobill -s <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val<<->> y : Top
  cmd<<->> anon ret a : T__14 =
    y.bind<<->> (x : T__21) -> x.ret(a)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = thunk(x) in
  >   y.ret(a)
  cmd<<->> anon ret a : T__12 =
    unit().bind<<+>> (x : T__19) -> thunk(x).bind<<->> (y : (Thunk Unit)) -> y.ret(a)

Test function calls
  $ autobill -s <<EOF
  > decl type A : +
  > decl type B : +
  > decl type C : +
  > decl val x : A
  > decl val y : B
  > decl val z : C
  > cmd ret a =
  > val f =
  >   match this.call(x,y,z).ret(b) ->
  >     thunk(tuple(y,z,x)).ret(b)
  > in
  >   f.call(x,y,z).ret(a)
  decl type A : +
  decl type B : +
  decl type C : +
  decl val<<+>> x : A
  decl val<<+>> y : B
  decl val<<+>> z : C
  cmd<<->> anon ret a : T__21 =
    cmd<<->> : T__24
    val =
      match
        | this.call(x : T__28, y : T__30, z : T__32).ret(b : T__34) -> thunk(tuple(y, z, x)).ret(b)
      end
    stk =
      this.bind<<->> (f : T__45) -> f.call(x, y, z).ret(a)
    end
