Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  cmd<-> anon__8 ret a__7 : t__6 =
    cmd-
    : t__9 val =
      GOT_TOP
    stk =
      this.bind- (x__13 : t__12) -> x__13.ret(a__7)
    end

Test reduction with declarations
  $ autobill -s <<EOF
  > decl val y : top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  decl val<-> y__6 : top
  cmd<-> anon__10 ret a__9 : t__8 = y__6
    .bind- (x__16 : t__15) -> x__16.ret(a__9)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (thunk unit) = thunk(x) in
  >   y.ret(a)
  cmd<-> anon__8 ret a__7 : t__6 = unit()
    .bind+ (x__14 : t__13) ->
      cmd-
      : (shift- unit) val =
        match
          case this.shift-().ret(b__17 : t__18) -> x__14.ret(b__17)
        end
      stk =
        this.bind- (y__25 : (shift- unit)) -> y__25.ret(a__7)
      end
  cmd<-> anon__38 ret a__37 : t__36 =
    shift+(GOT_TOP).match
                     case shift+(x__45 : t__44) -> x__45.ret(a__37)
                   end

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
  decl type a__6 : +
  decl type b__7 : +
  decl type c__8 : +
  decl val<+> x__9 : a__6
  decl val<+> y__11 : b__7
  decl val<+> z__13 : c__8
  cmd<-> anon__17 ret a__16 : t__15 =
    cmd-
    : t__18 val =
      match
        case this.call(x__21 : t__22, y__23 : t__24,
          z__25 : t__26).ret(b__27 : t__28) ->
          cmd-
          : t__29 val =
            match
              case this.shift-().ret(c__32 : t__33) ->
                tupple(y__23, z__25, x__21).ret(c__32)
            end
          stk =
            this.ret(b__27)
          end
      end
    stk =
      this.bind- (f__46 : t__45) -> f__46.call(x__9, y__11, z__13).ret(a__16)
    end
