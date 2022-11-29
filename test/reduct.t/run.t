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
  /* var x used ? : t__18 */
  cmd- anon ret a : t__12 =
    cmd-
    : t__15 val =
      GOT_TOP
    stk =
      this.bind- (x : t__18) -> x.ret(a)
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
  /* var x used ? : t__21 */
  decl val- y : top
  cmd- anon ret a : t__14 = y.bind- (x : t__21) -> x.ret(a)

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
  /* var x used ? : t__19 */
  /* var y used ? : (thunk unit) */
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
  /* var x used ? : t__28 */
  /* var y used ? : t__30 */
  /* var z used ? : t__32 */
  /* var f used ? : t__45 */
  /* cont b used ? : t__34 */
  decl val+ x : a
  decl val+ y : b
  decl val+ z : c
  cmd- anon ret a : t__21 =
    cmd-
    : t__24 val =
      match
        case this.call(x : t__28, y : t__30, z : t__32)b : t__34 ->
          thunk(tupple(y, z, x)).ret(b)
      end
    stk =
      this.bind- (f : t__45) -> f.call(x, y, z).ret(a)
    end
