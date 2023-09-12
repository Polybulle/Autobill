Test a simple pack/spec program.

  $ autobill -L -s sorts.bill
  data T$21 (A$27 : +) =
    | c$28()
  comput U$22 (A$29 : -) =
    | this.d$30().ret(A$29)
  decl type C$23 : -
  decl type D$24 : +
  decl val- x$43 : C$23
  decl val+ z$45 : D$24
  val+ test$47 : T$51 =
    c$28()
  val- test0$52 : T$63 =
    match this.d$30().ret(a$54 : T$55) -> x$43.ret(a$54)
  val- test1$64 : T$76 =
    bind/cc- a$67 : T$66 ->
      x$43.d$30().ret(a$67)


  $ autobill -L -t types.bill
  comput Id$21 =
    | this.inst$25<A$24 : +>().ret((Fun A$24 -> (Thunk A$24)))
  val- id2$38 : Id$21 =
    match this.inst$25<B$40 : +>().ret(a$41 : (Fun T$57 -> (Thunk T$57))) ->
      cmd- : (Fun T$57 -> (Thunk T$57)) val =
        match this.call(x$46 : T$57).ret(b$48 : (Thunk T$57)) ->
          cmd- : (Thunk T$57) val =
            match this.thunk().ret(c$54 : T$57) -> x$46.ret(c$54)
          stk =
            this.ret(b$48)
      stk =
        this.ret(a$41)
