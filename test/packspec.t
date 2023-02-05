Test a simple pack/spec program.

  $ autobill -r -s <<EOF
  > data T (A : +) = c()
  > comput U (A : -) = this.d().ret(A)
  > 
  > decl type C : -
  > decl val x : C
  > decl type D : +
  > decl val z : D
  > 
  > val y = c()
  > val y = match this.d().ret(a) -> x.ret(a)
  > cmd ret a = x.d().ret(a)
  > cmd ret a = z.match c() -> z.ret(a)
  > cmd ret a = c().match c() -> x.ret(a)
  data t (a : +) =
    | C
  comput u (a : -) =
    | this.D().ret(a)
  decl type c : -
  decl type d : +
  decl val- x : c
  decl val+ z : d
  val+ y : t__28 = C
  val- y : t__40 = match
                     | this.D().ret(a : t__33) -> x.ret(a)
                   end
  cmd- anon ret a : t__41 = x.D().ret(a)
  cmd+ anon ret a : t__53 = z.match
                               | C(y : t__62) -> z.ret(a)
                             end
  cmd- anon ret a : t__71 = C.match
                               | C -> x.ret(a)
                             end


  $ autobill -t <<EOF
  > comput Id = this.inst<A : +>().ret(Fun (A)-> Thunk A)
  > val id2 =
  >  match this.inst<B:+>().ret(a) ->
  >  cmd val =
  >    match this.call(x).ret(b) ->
  >    thunk(x).ret(b)
  >  stk =
  >    this.ret(a)
  >  end
  comput id =
    | this.Inst<a : +>().ret((fun (a) -> (thunk a)))
  val- id2 : id =
    match
      | this.Inst<t__25 : +>().ret(a : (fun (t__25) -> (thunk t__25))) ->
        cmd- : (fun (t__25) -> (thunk t__25))
        val =
          match
            | this.call(x : t__25).ret(b : (thunk t__25)) -> thunk(x).ret(b)
          end
        stk =
          this.ret(a)
        end
    end
