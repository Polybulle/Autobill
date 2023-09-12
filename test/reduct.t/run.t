Test that reduction works
  $ autobill -L -s <<EOF
  > cmd ret a = cmd val= GOT_TOP stk= this.bind x -> x.ret(a)
  > EOF
  cmd- anon$37 ret a$36 : T$35 =
    cmd- : T$38 val =
      GOT_TOP
    stk =
      this.bind- x$41 : T$40 -> x$41.ret(a$36)

Test reduction with declarations
  $ autobill -s -L <<EOF
  > decl val y : Top
  > cmd ret a = cmd val = y stk = this.bind x -> x.ret(a)
  > EOF
  decl val- y$35 : Top
  cmd- anon$39 ret a$38 : T$37 =
    y$35.bind- x$44 : T$43 -> x$44.ret(a$38)

Test shifting
  $ autobill -s -L <<EOF
  > cmd ret a =
  >   val x = unit() in
  >   val y : (Thunk Unit) = match this.thunk().ret(b) -> x.ret(b) in
  >   y.ret(a)
  cmd- anon$37 ret a$36 : T$35 =
    unit().bind+ x$42 : T$41 ->
      cmd- : (Thunk Unit) val =
        match this.thunk().ret(b$44 : T$45) -> x$42.ret(b$44)
      stk =
        this.bind- y$53 : (Thunk Unit) -> y$53.ret(a$36)

Test function calls
  $ autobill -s -L <<EOF
  > decl type A : +
  > decl type B : +
  > decl type C : +
  > decl val x : A
  > decl val y : B
  > decl val z : C
  > cmd ret a =
  > val f =
  >   match this.call(x,y,z).ret(b) ->
  >      cmd
  >      stk = this.ret(b)
  >      val = match this.thunk().ret(c) -> tuple(x,y,z).ret(c)
  > in
  >   f.call(x,y,z).thunk().ret(a)
  decl type A$21 : +
  decl type B$22 : +
  decl type C$23 : +
  decl val+ x$38 : A$21
  decl val+ y$40 : B$22
  decl val+ z$42 : C$23
  cmd+ anon$46 ret a$45 : T$44 =
    cmd- : T$47 val =
      match this.call(x$49 : T$50, y$51 : T$52, z$53 : T$54).ret(b$55 : T$56) ->
        cmd- : T$61 val =
          match this.thunk().ret(c$63 : T$64) -> tuple(x$49, y$51, z$53).ret(c$63)
        stk =
          this.ret(b$55)
    stk =
      this.bind- f$85 : T$84 -> f$85.call(x$38, y$40, z$42).thunk().ret(a$45)
