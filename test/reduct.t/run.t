Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd- anon ret a : T__0 =
    cmd- : T__3 val =
      GOT_TOP
    stk =
      this.bind- (x : T__5) ->
        x.ret(a)
    end

Test reduction with declarations
  $ autobill -s <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val- y : Top
  cmd- anon ret a : T__2 =
    y.ret(a)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = thunk(x) in
  >   y.ret(a)
  cmd- anon ret a : T__0 =
    thunk(unit()).ret(a)

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
  decl val+ x : A
  decl val+ y : B
  decl val+ z : C
  cmd- anon ret a : T__9 =
    thunk(tuple(y, z, x)).ret(a)
