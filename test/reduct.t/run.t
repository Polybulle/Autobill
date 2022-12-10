Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd- anon ret a : t__12 =
    cmd- : t__15
    val =
      GOT_TOP
    stk =
      this.bind- (x : t__18) -> x.ret(a)
    end

Test reduction with declarations
  $ autobill -s <<EOF
  > decl val y : top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val- y : top
  cmd- anon ret a : t__14 = y.bind- (x : t__21) -> x.ret(a)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (thunk unit) = thunk(x) in
  >   y.ret(a)
  cmd- anon ret a : t__12 = unit()
    .bind+ (x : t__19) -> thunk(x).bind- (y : (thunk unit)) -> y.ret(a)

Test function calls
  $ autobill -s <<EOF
  > decl type a : +
  > decl type b : +
  > decl type c : +
  > decl val x : a
  > decl val y : b
  > decl val z : c
  > cmd ret a =
  > val f =
  >   match this.call(x,y,z).ret(b) ->
  >     thunk(tupple(y,z,x)).ret(b)
  > in
  >   f.call(x,y,z).ret(a)
  decl type a : +
  decl type b : +
  decl type c : +
  decl val+ x : a
  decl val+ y : b
  decl val+ z : c
  cmd- anon ret a : t__21 =
    cmd- : t__24
    val =
      match
        case this.call(x : t__28, y : t__30, z : t__32).ret(b : t__34) ->
          thunk(tupple(y, z, x)).ret(b)
      end
    stk =
      this.bind- (f : t__45) -> f.call(x, y, z).ret(a)
    end
