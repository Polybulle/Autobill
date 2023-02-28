Test a simple pack/spec program.

  $ autobill -M -s sorts.bill
  data T (A : +) =
    | c()
  comput U (A : -) =
    | this.d().ret(A)
  decl type C : -
  decl type D : +
  decl val- x : C
  decl val+ z : D
  val+ y : T__28 =
    c()
  val- y : T__39 =
    match
      | this.d().ret(a : T__33) -> x.ret(a)
    end
  cmd- anon ret a : T__40 =
    x.d().ret(a)
  cmd+ anon ret a : T__51 =
    z.match
      | c() -> z.ret(a),end
  cmd- anon ret a : T__65 =
    c().match
      | c() -> x.ret(a),end


  $ autobill -M -t types.bill
  comput Id =
    | this.inst<A : +>().ret((Fun A -> (Thunk A)))
  val- id2 : Id =
    match
      | this.inst<T__24 : +>().ret(a : (Fun T__24 -> (Thunk T__24))) ->
        cmd- : (Fun T__24 -> (Thunk T__24)) val =
          match
            | this.call(x : T__24).ret(b : (Thunk T__24)) -> thunk(x).ret(b)
          end
        stk =
          this.ret(a)
        end
    end
