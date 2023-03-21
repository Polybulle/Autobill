Test that reduction works
  $ autobill -M -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd- anon__35 ret a__34 : T__33 =
    cmd- : T__36 val =
      GOT_TOP
    stk =
      this.bind- (x__39 : T__38) ->
        x__39.ret(a__34)
    end

Test reduction with declarations
  $ autobill -s -M <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val- y__33 : Top
  cmd- anon__37 ret a__36 : T__35 =
    y__33.bind- (x__42 : T__41) ->
      x__42.ret(a__36)

Test shifting
  $ autobill -s -M <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = thunk(x) in
  >   y.ret(a)
  cmd- anon__35 ret a__34 : T__33 =
    unit().bind+ (x__40 : T__39) ->
      thunk(x__40).bind- (y__45 : (Thunk Unit)) ->
        y__45.ret(a__34)

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
  decl type A__21 : +
  decl type B__22 : +
  decl type C__23 : +
  decl val+ x__36 : A__21
  decl val+ y__38 : B__22
  decl val+ z__40 : C__23
  cmd- anon__44 ret a__43 : T__42 =
    cmd- : T__45 val =
      match
        | this.call(x__47 : T__48, y__49 : T__50, z__51 : T__52).ret(b__53 : T__54) -> thunk(tuple(y__49, z__51, x__47)).ret(b__53)
      end
    stk =
      this.bind- (f__77 : T__76) ->
        f__77.call(x__36, y__38, z__40).ret(a__43)
    end
