Test the parser on a BILL program testingthe whole grammar
  $ autobill parse test.bill
  decl type test : +
  decl type test : -
  type test : + = tvar
  type test (a : +) (b : -) (c : -) (d : +) : + = (d e (f g h))
  type test : + = (exp (aff (lin a)))
  type test (a : -) : + = (prod unit (sum zero (choice top (fun bottom a))))
  data test =
    case :test(unit)
  codata test (a : +) (b : -) =
    case this.mycall(a).ret() : b
  codata test (a : +) (b : -) =
    case this.myyes().ret() : a
    case this.myno().ret() : b
  cmd test = v.ret()
  cmd test = v.ret()
  cmd test = v.ret()
  term test : t = x
  term test = :mycons()
  term test = :mycons(x, y, z)
  term test = pair(left(unit()), right(unit()))
  term test = box(exp) (ret() : t) -> v.ret()
  term test = box(exp) -> v.ret()
  term test = bind/cc+ (ret() : t) -> v.ret()
  term test = bind/cc -> v.ret()
  term test = match
                case this.cons(x, y, z).ret() -> v.ret()
              end
  term test = match
                case this.cons(x : t, y : u, z : v).ret() : w -> v.ret()
              end
  term test =
    match
      case this.cons1(x : t, y : u, z : v).ret() : w -> v.ret()
      case this.cons2(x : t, y : u, z : v).ret() : w -> v.ret()
    end
  cmd test : t = unit().ret()
  cmd test = unit().call(x).yes().no().ret()
  cmd test = unit().mycons().ret()
  cmd test = unit().mycons2(x, y, z).ret()
  cmd test = unit().unbox(lin).ret()
  cmd test = unit().bind x -> v.ret()
  cmd test = unit().bind+ (x : t) -> v.ret()
  cmd test = unit().match
                     case :cons(x, y, z) -> v.ret()
                   end
  cmd test =
    unit().match
            case :cons1(x : t, y : u, z : v) -> v.ret()
            case :cons2(x : t, y : u, z : v) -> v.ret()
          end
  term test = fun (x : t) -> v
  term test = box(lin) v
  cmd test = match :cons(x, y, z) = v in v.ret()
  cmd test = match env this.cons(x, y, z).ret() in v.ret()
  cmd test = term x = v in v.ret()
  cmd test = env this.ret() in v.ret()

Now test the parser with a roundtrip
  $ autobill parse test.bill | autobill parse
  decl type test : +
  decl type test : -
  type test : + = tvar
  type test (a : +) (b : -) (c : -) (d : +) : + = (d e (f g h))
  type test : + = (exp (aff (lin a)))
  type test (a : -) : + = (prod unit (sum zero (choice top (fun bottom a))))
  data test =
    case :test(unit)
  codata test (a : +) (b : -) =
    case this.mycall(a).ret() : b
  codata test (a : +) (b : -) =
    case this.myyes().ret() : a
    case this.myno().ret() : b
  cmd test = v.ret()
  cmd test = v.ret()
  cmd test = v.ret()
  term test : t = x
  term test = :mycons()
  term test = :mycons(x, y, z)
  term test = pair(left(unit()), right(unit()))
  term test = box(exp) (ret() : t) -> v.ret()
  term test = box(exp) -> v.ret()
  term test = bind/cc+ (ret() : t) -> v.ret()
  term test = bind/cc -> v.ret()
  term test = match
                case this.cons(x, y, z).ret() -> v.ret()
              end
  term test = match
                case this.cons(x : t, y : u, z : v).ret() : w -> v.ret()
              end
  term test =
    match
      case this.cons1(x : t, y : u, z : v).ret() : w -> v.ret()
      case this.cons2(x : t, y : u, z : v).ret() : w -> v.ret()
    end
  cmd test : t = unit().ret()
  cmd test = unit().call(x).yes().no().ret()
  cmd test = unit().mycons().ret()
  cmd test = unit().mycons2(x, y, z).ret()
  cmd test = unit().unbox(lin).ret()
  cmd test = unit().bind x -> v.ret()
  cmd test = unit().bind+ (x : t) -> v.ret()
  cmd test = unit().match
                     case :cons(x, y, z) -> v.ret()
                   end
  cmd test =
    unit().match
            case :cons1(x : t, y : u, z : v) -> v.ret()
            case :cons2(x : t, y : u, z : v) -> v.ret()
          end
  term test = fun (x : t) -> v
  term test = box(lin) v
  cmd test = match :cons(x, y, z) = v in v.ret()
  cmd test = match env this.cons(x, y, z).ret() in v.ret()
  cmd test = term x = v in v.ret()
  cmd test = env this.ret() in v.ret()
