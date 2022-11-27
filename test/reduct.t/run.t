Test that reduction works
  $ autobill -r -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a) end
  > EOF
  /* tyvar t__12 : - */
  /* tyvar t__15 : - */
  /* tyvar t__16 : - */
  /* tyvar t__17 : - */
  /* tyvar t__18 : - */
  /* tyvar t__20 : - */
  /* tyvar t__21 : - */
  /* tyvar t__22 : - */
  /* tyvar t__23 : - */
  /* var x__19 used ? : t__18 */
  cmd- anon__14 ret a__13 : t__12 =
    cmd-
    : t__15 val =
      GOT_TOP
    stk =
      this.bind- (x__19 : t__18) -> x__19.ret(a__13)
    end

Test reduction with declarations
  $ autobill -s <<EOF
  > decl val y : top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a) end
  > EOF
  /* tyvar t__14 : - */
  /* tyvar t__17 : - */
  /* tyvar t__18 : - */
  /* tyvar t__19 : - */
  /* tyvar t__20 : - */
  /* tyvar t__21 : - */
  /* tyvar t__23 : - */
  /* tyvar t__24 : - */
  /* tyvar t__25 : - */
  /* tyvar t__26 : - */
  /* var x__22 used ? : t__21 */
  decl val- y__12 : top
  cmd- anon__16 ret a__15 : t__14 = y__12
    .bind- (x__22 : t__21) -> x__22.ret(a__15)

Test shifting
  $ autobill -s <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (thunk unit) = thunk(x) in
  >   y.ret(a)
  /* tyvar t__12 : - */
  /* tyvar t__15 : + */
  /* tyvar t__16 : - */
  /* tyvar t__17 : + */
  /* tyvar t__18 : - */
  /* tyvar t__19 : + */
  /* tyvar t__21 : - */
  /* tyvar t__22 : - */
  /* tyvar t__23 : + */
  /* tyvar t__24 : - */
  /* tyvar t__26 : - */
  /* tyvar t__27 : - */
  /* tyvar t__28 : - */
  /* tyvar t__29 : - */
  /* var x__20 used ? : t__19 */
  /* var y__25 used ? : (thunk unit) */
  cmd- anon__14 ret a__13 : t__12 = unit()
    .bind+ (x__20 : t__19) -> thunk(x__20)
      .bind- (y__25 : (thunk unit)) -> y__25.ret(a__13)

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
  decl type a__12 : +
  decl type b__13 : +
  decl type c__14 : +
  /* tyvar t__21 : - */
  /* tyvar t__24 : - */
  /* tyvar t__25 : - */
  /* tyvar t__26 : - */
  /* tyvar t__28 : + */
  /* tyvar t__30 : + */
  /* tyvar t__32 : + */
  /* tyvar t__34 : - */
  /* tyvar t__35 : - */
  /* tyvar t__36 : - */
  /* tyvar t__37 : - */
  /* tyvar t__38 : + */
  /* tyvar t__39 : + */
  /* tyvar t__40 : + */
  /* tyvar t__41 : + */
  /* tyvar t__42 : - */
  /* tyvar t__44 : - */
  /* tyvar t__45 : - */
  /* tyvar t__47 : - */
  /* tyvar t__48 : - */
  /* tyvar t__49 : - */
  /* tyvar t__50 : - */
  /* tyvar t__51 : - */
  /* tyvar t__52 : + */
  /* tyvar t__53 : + */
  /* tyvar t__54 : + */
  /* tyvar t__55 : - */
  /* var x__27 used ? : t__28 */
  /* var y__29 used ? : t__30 */
  /* var z__31 used ? : t__32 */
  /* var f__46 used ? : t__45 */
  /* cont b__33 used ? : t__34 */
  decl val+ x__15 : a__12
  decl val+ y__17 : b__13
  decl val+ z__19 : c__14
  cmd- anon__23 ret a__22 : t__21 =
    cmd-
    : t__24 val =
      match
        case this.call(x__27 : t__28, y__29 : t__30,
          z__31 : t__32)b__33 : t__34 -> thunk(tupple(y__29, z__31, x__27))
          .ret(b__33)
      end
    stk =
      this.bind- (f__46 : t__45) -> f__46.call(x__15, y__17, z__19).ret(a__22)
    end
