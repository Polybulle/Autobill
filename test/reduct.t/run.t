Test that reduction works
  $ autobill -M -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd- anon__37 ret a__36 : T__35 =
    cmd- : T__38 val =
      GOT_TOP
    stk =
      this.bind- (x__41 : T__40) ->
        x__41.ret(a__36)
    end

Test reduction with declarations
  $ autobill -s -M <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val- y__35 : Top
  cmd- anon__39 ret a__38 : T__37 =
    y__35.bind- (x__44 : T__43) ->
      x__44.ret(a__38)

Test shifting
  $ autobill -s -M <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = thunk(x) in
  >   y.ret(a)
  cmd- anon__37 ret a__36 : T__35 =
    unit().bind+ (x__42 : T__41) ->
      thunk(x__42).bind- (y__47 : (Thunk Unit)) ->
        y__47.ret(a__36)

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
  decl val+ x__38 : A__21
  decl val+ y__40 : B__22
  decl val+ z__42 : C__23
  cmd- anon__46 ret a__45 : T__44 =
    cmd- : T__47 val =
      match
        | this.call(x__49 : T__50, y__51 : T__52, z__53 : T__54).ret(b__55 : T__56) -> thunk(tuple(y__51, z__53, x__49)).ret(b__55)
      end
    stk =
      this.bind- (f__79 : T__78) ->
        f__79.call(x__38, y__40, z__42).ret(a__45)
    end
