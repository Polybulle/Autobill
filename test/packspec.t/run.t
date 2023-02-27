Test a simple pack/spec program.

  $ autobill -r -s sorts.bill
  data T (A : +) =
    | c()
  comput U (A : -) =
    | this.d().ret(A)
  decl type C : -
  decl type D : +
  decl val- x : C
  decl val+ z : D
  val+ y : T__16 =
    c()
  val- y : T__27 =
    match
      | this.d().ret(a : T__21) -> x.ret(a)
    end
  cmd- anon ret a : T__28 =
    x.d().ret(a)
  cmd+ anon ret a : T__39 =
    z.match
      | c() -> z.ret(a),end
  cmd- anon ret a : T__53 =
    c().match
      | c() -> x.ret(a),end


  $ autobill -t types.bill
  comput Id =
    | this.inst<A : +>().ret((Fun A -> (Thunk A)))
  val- id2 : Id =
    bind/cc- a__45 : Id ->
      cmd- : Id val =
        match
          | this.inst<T__12 : +>().ret(a : (Fun T__12 -> (Thunk T__12))) ->
            cmd- : (Fun T__12 -> (Thunk T__12)) val =
              match
                | this.call(x : T__12).ret(b : (Thunk T__12)) -> thunk(x).ret(b)
              end
            stk =
              this.ret(a)
            end
        end
      stk =
        this.ret(a__45)
      end
