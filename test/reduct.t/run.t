Test that reduction works
  $ autobill -M -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd- anon__33 ret a__32 : T__31 =
    cmd- : T__34 val =
      GOT_TOP
    stk =
      this.bind- (x__37 : T__36) ->
        x__37.ret(a__32)
    end

Test reduction with declarations
  $ autobill -s -M <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val- y__31 : Top
  cmd- anon__35 ret a__34 : T__33 =
    y__31.bind- (x__40 : T__39) ->
      x__40.ret(a__34)

Test shifting
  $ autobill -s -M <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = thunk(x) in
  >   y.ret(a)
  cmd- anon__33 ret a__32 : T__31 =
    unit().bind+ (x__38 : T__37) ->
      thunk(x__38).bind- (y__43 : (Thunk Unit)) ->
        y__43.ret(a__32)

Test function calls
  $ autobill -s -M <<EOF
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
  decl type A__19 : +
  decl type B__20 : +
  decl type C__21 : +
  decl val+ x__34 : A__19
  decl val+ y__36 : B__20
  decl val+ z__38 : C__21
  cmd- anon__42 ret a__41 : T__40 =
    cmd- : T__43 val =
      match
        | this.call(x__45 : T__46, y__47 : T__48, z__49 : T__50).ret(b__51 : T__52) -> thunk(tuple(y__47, z__49, x__45)).ret(b__51)
      end
    stk =
      this.bind- (f__75 : T__74) ->
        f__75.call(x__34, y__36, z__38).ret(a__41)
    end
