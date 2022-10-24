Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = step GOT_TOP into this.bind x -> x.ret(a) end
  > EOF
  cmd<-> anon__8 ret a__7 : t__6 =
    step-
      GOT_TOP
    : t__14
    into
      this.ret(a__7)
    end

Test reduction with declarations
  $ autobill -s -r <<EOF
  > decl term y : top
  > cmd ret a = step y into this.bind x -> x.ret(a) end
  > EOF
  decl term<-> y__6 : top
  cmd<-> anon__10 ret a__9 : t__8 = y__6.ret(a__9)

Test shifting
  $ autobill -s -r <<EOF
  > cmd ret a =
  >   term x = unit() in
  >   term y : (shift- unit) = match this.shift-().ret(b) -> x.ret(b) in
  >   y.ret(a)
  > cmd ret a =
  >   shift+(GOT_TOP).match shift+(x) -> x.ret(a)
  cmd<-> anon__8 ret a__7 : t__6 =
    step-
      match
        case this.shift-().ret(b__17 : t__18) -> unit().ret(b__17)
      end
    : t__26
    into
      this.ret(a__7)
    end
  cmd<-> anon__38 ret a__37 : t__36 =
    step-
      GOT_TOP
    : t__46
    into
      this.ret(a__37)
    end

Test function calls
  $ autobill -s -r <<EOF
  > decl type a : +
  > decl type b : +
  > decl type c : +
  > decl term x : a
  > decl term y : b
  > decl term z : c
  > cmd ret a =
  > term f =
  >   match this.call(x,y,z).ret(b) ->
  >     step
  >       match this.shift-().ret(c) -> tupple(y,z,x).ret(c)
  >     into
  >       this.ret(b)
  >     end
  > in
  >   f.call(x,y,z).ret(a)
  decl type a__6 : +
  decl type b__7 : +
  decl type c__8 : +
  decl term<+> x__9 : a__6
  decl term<+> y__11 : b__7
  decl term<+> z__13 : c__8
  cmd<-> anon__17 ret a__16 : t__15 =
    step-
      match
        case this.shift-().ret(c__32 : t__33) -> tupple(y__11, z__13, x__9)
          .ret(c__32)
      end
    : t__29
    into
      this.ret(a__16)
    end
