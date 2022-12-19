Test the parser on a BILL program testingthe whole grammar
  $ autobill -p test.bill
  decl sort nat
  decl type zero_t : nat
  decl type test : +
  decl type test : -
  type test : + = tvar
  type test (a : +) (b : -) (c : -) (d : +) : + = (d e (f g h))
  type test : + = (exp (aff (lin a)))
  type test (a : -) : + = (unit * (zero + (top & (fun (bottom) -> a))))
  data test =
    case Test(unit)
  comput test (a : +) (b : -) =
    case this.Mycall(a).ret(b)
  comput test (a : +) (b : -) =
    case this.Myyes().ret(a)
    case this.Myno().ret(b)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : t = x
  val test = Mycons
  val test = Mycons(x, y, z)
  val test = tupple(inj(0/2, unit()), inj(1/2, inj(1/3, unit())))
  val test = box(exp)a : t -> v.ret(a)
  val test = box(exp)a -> v.ret(a)
  val test = bind/cc+ a : t -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               case this.Cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               case this.Cons(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
             end
  val test =
    match
      case this.Cons1(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
      case this.Cons2(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
    end
  cmd test ret a : t = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj(0/2).proj(1/2).proj(1/3).ret(a)
  cmd test ret a = unit().Mycons().ret(a)
  cmd test ret a = unit().Mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : t) -> v.ret(a)
  cmd test ret a = unit().match
                           case Cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            case Cons1(x : t, y : u, z : v) -> v.ret(a)
            case Cons2(x : t, y : u, z : v) -> v.ret(a)
          end
  val test = fun (x : t) -> v
  val test = box(lin, v)
  cmd test ret a = match Cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.Cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
  cmd test ret a = val x = v in x.ret(a)
  cmd test ret a = stk b = this.ret(a) in v.ret(b)

Now test the parser with a roundtrip
  $ autobill -p test.bill | autobill -p
  decl sort nat
  decl type zero_t : nat
  decl type test : +
  decl type test : -
  type test : + = tvar
  type test (a : +) (b : -) (c : -) (d : +) : + = (d e (f g h))
  type test : + = (exp (aff (lin a)))
  type test (a : -) : + = (unit * (zero + (top & (fun (bottom) -> a))))
  data test =
    case Test(unit)
  comput test (a : +) (b : -) =
    case this.Mycall(a).ret(b)
  comput test (a : +) (b : -) =
    case this.Myyes().ret(a)
    case this.Myno().ret(b)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : t = x
  val test = Mycons
  val test = Mycons(x, y, z)
  val test = tupple(inj(0/2, unit()), inj(1/2, inj(1/3, unit())))
  val test = box(exp)a : t -> v.ret(a)
  val test = box(exp)a -> v.ret(a)
  val test = bind/cc+ a : t -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               case this.Cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               case this.Cons(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
             end
  val test =
    match
      case this.Cons1(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
      case this.Cons2(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
    end
  cmd test ret a : t = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj(0/2).proj(1/2).proj(1/3).ret(a)
  cmd test ret a = unit().Mycons().ret(a)
  cmd test ret a = unit().Mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : t) -> v.ret(a)
  cmd test ret a = unit().match
                           case Cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            case Cons1(x : t, y : u, z : v) -> v.ret(a)
            case Cons2(x : t, y : u, z : v) -> v.ret(a)
          end
  val test = fun (x : t) -> v
  val test = box(lin, v)
  cmd test ret a = match Cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.Cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
  cmd test ret a = val x = v in x.ret(a)
  cmd test ret a = stk b = this.ret(a) in v.ret(b)
