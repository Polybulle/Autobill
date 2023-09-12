Give the remaining logical constraint
  $ autobill -L -e eqns_pack.bill
  (:exists (T$80 T$81 T$82 T$83 T$214 T$215 T$216 T$217 T$218 T$219 T$220 T$221)
   :witness ()
   :with ((:eq-idx T$82 (F$22 T$83 T$80))
          (:eq-idx T$82 T$215)
          (:eq-idx T$83 (F$22 T$82 T$81))
          (:eq-idx T$83 T$214)
          (:eq-idx T$214 Y0$25)
          (:eq-idx T$215 X0$24)
          (:eq-idx T$216 Y0$25)
          (:eq-idx T$217 X0$24)
          (:eq-idx T$218 (F$22 T$83 T$80))
          (:eq-idx T$219 (F$22 T$82 T$81))
          (:eq-idx T$220 X0$24)
          (:eq-idx T$221 Y0$25))
   :then (:model ((:eq-idx T$82 X0$24)
                  (:eq-idx T$83 Y0$25))))

Give the elaborated program
  $ autobill -L eqns_pack.bill
  decl sort idx
  decl type F : idx
  decl type Carrier : +
  decl type X0 : idx
  decl type Y0 : idx
  data Foo (A : idx) (B : idx) =
    | foo<X : idx, Y : idx>((Carrier X Y)) with X = (F Y A), Y = (F X B)
  decl val+ foor : _
  val+ fooz =
    bind/cc+ a$222 ->
      foo<X0, Y0>(foor).ret(a$222)

Give the remaining logical constraint
  $ autobill -L -e eqns_spec.bill
  (:forall (X0$50 Y0$51)
   :exists (X0$50 Y0$51 T$98 T$99 T$114 T$115 T$241 T$242 T$243 T$244)
   :assume ((:eq-idx X0$50 T$98)
            (:eq-idx Y0$51 T$99))
   :witness ((:eq-idx (Max3$23 T$114 T$114 T$115) (Max3$23 T$98 T$99 T$99))
             (:eq-idx T$114 T$241)
             (:eq-idx T$115 T$242)
             (:eq-idx T$241 X0$50)
             (:eq-idx T$242 Y0$51)
             (:eq-idx T$243 (Max3$23 T$114 T$114 T$115))
             (:eq-idx T$244 (Max3$23 T$98 T$99 T$99))) :then :true)

Give the elaborated program
  $ autobill -L eqns_spec.bill
  decl sort idx
  decl type Carrier : -
  decl type Max3 : idx
  comput Foo =
    | this.foo<X : idx, Y : idx>().ret((Carrier (Max3 X X Y)))
  comput Bar =
    | this.bar<X : idx, Y : idx>().ret((Carrier (Max3 X Y Y)))
  decl val- x : _
  val- y =
    bind/cc- a$245 ->
      cmd- val =
        match this.bar<X0 : idx, Y0 : idx>().ret(a) -> x.foo<X0, Y0>().ret(a)
      stk =
        this.ret(a$245)
