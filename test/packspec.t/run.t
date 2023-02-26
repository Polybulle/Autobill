Test a simple pack/spec program.

  $ autobill -r -s sorts.bill
  data T (A : +) =
    | c()
  comput U (A : -) =
    | this.d().ret(A)
  decl type C : -
  decl type D : +
  decl val<<->> x : C
  decl val<<+>> z : D
  val<<+>> y : T__28 =
    c()
  val<<->> y : T__40 =
    match
      | this.d().ret(a : T__33) -> x.ret(a)
    end
  cmd<<->> anon ret a : T__41 =
    x.d().ret(a)
  cmd<<+>> anon ret a : T__53 =
    z.match
       | c() -> z.ret(a)
     end
  cmd<<->> anon ret a : T__69 =
    c().match
         | c() -> x.ret(a)
       end


  $ autobill -t types.bill
  comput Id =
    | this.inst<A : +>().ret((Fun (A) -> (Thunk A)))
  val<<->> id2 : Id =
    match
      | this.inst<T__25 : +>().ret(a : (Fun (T__25) -> (Thunk T__25))) ->
        cmd<<->> : (Fun (T__25) -> (Thunk T__25))
        val =
          match
            | this.call(x : T__25).ret(b : (Thunk T__25)) -> thunk(x).ret(b)
          end
        stk =
          this.ret(a)
        end
    end
