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
    case test(unit)
  comput test (a : +) (b : -) =
    case this.mycall(a).ret(b)
  comput test (a : +) (b : -) =
    case this.myyes().ret(a)
    case this.myno().ret(b)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : t = x
  val test = mycons
  val test = mycons(x, y, z)
  val test = tupple(inj(0/2, unit()), inj(1/2, inj(1/3, unit())))
  val test = box(exp)a : t -> v.ret(a)
  val test = box(exp)a -> v.ret(a)
  val test = bind/cc+ a : t -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               case this.cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               case this.cons(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
             end
  val test =
    match
      case this.cons1(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
      case this.cons2(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
    end
  cmd test ret a : t = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj(0/2).proj(1/2).proj(1/3).ret(a)
  cmd test ret a = unit().mycons().ret(a)
  cmd test ret a = unit().mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : t) -> v.ret(a)
  cmd test ret a = unit().match
                           case cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            case cons1(x : t, y : u, z : v) -> v.ret(a)
            case cons2(x : t, y : u, z : v) -> v.ret(a)
          end
  val test = fun (x : t) -> v
  val test = box(lin, v)
  cmd test ret a = match cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
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
    case test(unit)
  comput test (a : +) (b : -) =
    case this.mycall(a).ret(b)
  comput test (a : +) (b : -) =
    case this.myyes().ret(a)
    case this.myno().ret(b)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : t = x
  val test = mycons
  val test = mycons(x, y, z)
  val test = tupple(inj(0/2, unit()), inj(1/2, inj(1/3, unit())))
  val test = box(exp)a : t -> v.ret(a)
  val test = box(exp)a -> v.ret(a)
  val test = bind/cc+ a : t -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               case this.cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               case this.cons(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
             end
  val test =
    match
      case this.cons1(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
      case this.cons2(x : t, y : u, z : v).ret(a : w) -> v.ret(a)
    end
  cmd test ret a : t = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj(0/2).proj(1/2).proj(1/3).ret(a)
  cmd test ret a = unit().mycons().ret(a)
  cmd test ret a = unit().mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : t) -> v.ret(a)
  cmd test ret a = unit().match
                           case cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            case cons1(x : t, y : u, z : v) -> v.ret(a)
            case cons2(x : t, y : u, z : v) -> v.ret(a)
          end
  val test = fun (x : t) -> v
  val test = box(lin, v)
  cmd test ret a = match cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
  cmd test ret a = val x = v in x.ret(a)
  cmd test ret a = stk b = this.ret(a) in v.ret(b)
