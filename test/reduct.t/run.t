Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  /* tyvar t__12 : - */
  /* var x__13 used ? : t__12 */
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
  /* tyvar t__15 : - */
  /* var x__16 used ? : t__15 */
  decl val<-> y__6 : top
  cmd<-> anon__10 ret a__9 : t__8 = y__6
    .bind- (x__16 : t__15) -> x__16.ret(a__9)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (thunk unit) = thunk(x) in
  >   y.ret(a)
  /* tyvar t__13 : + */
  /* var x__14 used ? : t__13 */
  /* var y__19 used ? : (thunk unit) */
  cmd<-> anon__8 ret a__7 : t__6 = unit()
    .bind+ (x__14 : t__13) -> thunk(x__14)
      .bind- (y__19 : (thunk unit)) -> y__19.ret(a__7)

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
  /* tyvar t__22 : + */
  /* tyvar t__24 : + */
  /* tyvar t__26 : + */
  /* tyvar t__28 : - */
  /* tyvar t__39 : - */
  /* var x__21 used ? : t__22 */
  /* var y__23 used ? : t__24 */
  /* var z__25 used ? : t__26 */
  /* var f__40 used ? : t__39 */
  /* cont b__27 used ? : t__28 */
  decl val<+> x__9 : a__6
  decl val<+> y__11 : b__7
  decl val<+> z__13 : c__8
  cmd<-> anon__17 ret a__16 : t__15 =
    cmd-
    : t__18 val =
      match
        case this.call(x__21 : t__22, y__23 : t__24,
          z__25 : t__26).ret(b__27 : t__28) ->
          thunk(tupple(y__23, z__25, x__21)).ret(b__27)
      end
    stk =
      this.bind- (f__40 : t__39) -> f__40.call(x__9, y__11, z__13).ret(a__16)
    end
