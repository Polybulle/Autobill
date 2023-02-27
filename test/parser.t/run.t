Test the parser on a BILL program testingthe whole grammar
  $ autobill -p test.bill
  decl sort nat
  decl type Zero_t : nat
  decl type Test : +
  decl type Test : -
  type Test : + = Tvar
  type Test (A : +) (B : -) (C : -) (D : +) : + = (D E (F G H))
  type Test : + = (Exp (Aff (Lin A)))
  type Test (A : -) : + = (Unit * (Zero + (Top & (Fun Bottom -> A))))
  data Test =
    | test(Unit)
  comput Test (A : +) (B : -) =
    | this.mycall(A).ret(B)
  comput Test (A : +) (B : -) =
    | this.myyes().ret(A)
    | this.myno().ret(B)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : T = x
  val test = mycons
  val test = mycons(x, y, z)
  val test = tupple(inj{1,2}(unit()), inj{2,2}(inj{1,3}(unit())))
  val test = box(Exp)a : T -> v.ret(a)
  val test = box(Exp)a -> v.ret(a)
  val test = bind/cc+ a : T -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               | this.cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               | this.cons(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
             end
  val test =
    match
      | this.cons1(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
      | this.cons2(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
    end
  cmd test ret a : T = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj{1,2}().proj{2,2}().proj{1,3}().ret(a)
  cmd test ret a = unit().mycons().ret(a)
  cmd test ret a = unit().mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(Lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : T) -> v.ret(a)
  cmd test ret a = unit().match
                           | cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            | cons1(x : T, y : U, z : V) -> v.ret(a)
            | cons2(x : T, y : U, z : V) -> v.ret(a)
          end
  val test = fun (x : T) -> v
  val test = box(Lin, v)
  cmd test ret a = match cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
  cmd test ret a = val x = v in x.ret(a)
  cmd test ret a = stk b = this.ret(a) in v.ret(b)

Now test the parser with a roundtrip
  $ autobill -p test.bill | autobill -p
  decl sort nat
  decl type Zero_t : nat
  decl type Test : +
  decl type Test : -
  type Test : + = Tvar
  type Test (A : +) (B : -) (C : -) (D : +) : + = (D E (F G H))
  type Test : + = (Exp (Aff (Lin A)))
  type Test (A : -) : + = (Unit * (Zero + (Top & (Fun Bottom -> A))))
  data Test =
    | test(Unit)
  comput Test (A : +) (B : -) =
    | this.mycall(A).ret(B)
  comput Test (A : +) (B : -) =
    | this.myyes().ret(A)
    | this.myno().ret(B)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  cmd test ret a = v.ret(a)
  val test : T = x
  val test = mycons
  val test = mycons(x, y, z)
  val test = tupple(inj{1,2}(unit()), inj{2,2}(inj{1,3}(unit())))
  val test = box(Exp)a : T -> v.ret(a)
  val test = box(Exp)a -> v.ret(a)
  val test = bind/cc+ a : T -> v.ret(a)
  val test = bind/cc a -> v.ret(a)
  val test = match
               | this.cons(x, y, z).ret(a) -> v.ret(a)
             end
  val test = match
               | this.cons(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
             end
  val test =
    match
      | this.cons1(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
      | this.cons2(x : T, y : U, z : V).ret(a : W) -> v.ret(a)
    end
  cmd test ret a : T = unit().ret(a)
  cmd test ret a = cmd val =
                     GOT_TOP
                   stk =
                     this.GOT_ZERO()
                   end
  cmd test ret a = unit().call(x).proj{1,2}().proj{2,2}().proj{1,3}().ret(a)
  cmd test ret a = unit().mycons().ret(a)
  cmd test ret a = unit().mycons2(x, y, z).ret(a)
  cmd test ret a = unit().unbox(Lin).ret(a)
  cmd test ret a = unit().bind (x) -> v.ret(a)
  cmd test ret a = unit().bind+ (x : T) -> v.ret(a)
  cmd test ret a = unit().match
                           | cons(x, y, z) -> v.ret(a)
                         end
  cmd test ret a =
    unit().match
            | cons1(x : T, y : U, z : V) -> v.ret(a)
            | cons2(x : T, y : U, z : V) -> v.ret(a)
          end
  val test = fun (x : T) -> v
  val test = box(Lin, v)
  cmd test ret a = match cons(x, y, z) = v in v.ret(a)
  cmd test ret a =
    match stk this.cons(x, y, z).ret(b) = this.ret(a) in v.ret(b)
  cmd test ret a = val x = v in x.ret(a)
  cmd test ret a = stk b = this.ret(a) in v.ret(b)
