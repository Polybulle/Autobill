Test a simple pack/spec program.

  $ autobill -r -s <<EOF
  > data t (a : +) = case C
  > comput u (a : -) = case this.D().ret(a)
  > 
  > decl type c : -
  > decl val x : c
  > decl type d : +
  > decl val z : d
  > 
  > val y = C
  > val y = match this.D.ret(a) -> x.ret(a)
  > cmd ret a = x.D.ret(a)
  > cmd ret a = z.match C(y) -> z.ret(a)
  > cmd ret a = C.match C -> x.ret(a)
  data t (a : +) =
    case C
  comput u (a : -) =
    case this.D().ret(a)
  decl type c : -
  decl type d : +
  decl val- x : c
  decl val+ z : d
  val+ y : t__27 = C
  val- y : t__38 = match
                     case this.D().ret(a : t__31) -> x.ret(a)
                   end
  cmd- anon ret a : t__39 = x.D().ret(a)
  cmd+ anon ret a : t__50 = z.match
                               case C(y : t__58) -> z.ret(a)
                             end
  cmd- anon ret a : t__67 = C.match
                               case C -> x.ret(a)
                             end


  $ autobill -t <<EOF
  > comput id = case this.Inst<a : +>().ret(fun (a)-> thunk a)
  > val id2 =
  >  match this.Inst<b:+>().ret(a) ->
  >  cmd val =
  >    match this.call(x).ret(b) ->
  >    thunk(x).ret(b)
  >  stk =
  >    this.ret(a)
  >  end
  comput id =
    case this.Inst<a : +>().ret((fun (a) -> (thunk a)))
  val- id2 : id =
    match
      case this.Inst<t__24 : +>().ret(a : (fun (t__24) -> (thunk t__24))) ->
        cmd- : (fun (t__24) -> (thunk t__24))
        val =
          match
            case this.call(x : t__24).ret(b : (thunk t__24)) -> thunk(x).ret(b)
          end
        stk =
          this.ret(a)
        end
    end
