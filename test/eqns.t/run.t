Give the remaining logical constraint
  $ autobill -M -C eqns_pack.bill
  (:exists (T__80 T__81 T__82 T__83 T__208 T__209 T__210 T__211 T__212 T__213 T__214 T__215)
   :witness ()
   :with ((:eq-idx T__82 (F__22 T__83 T__80))
          (:eq-idx T__82 T__209)
          (:eq-idx T__83 (F__22 T__82 T__81))
          (:eq-idx T__83 T__208)
          (:eq-idx T__208 Y0__25)
          (:eq-idx T__209 X0__24)
          (:eq-idx T__210 Y0__25)
          (:eq-idx T__211 X0__24)
          (:eq-idx T__212 (F__22 T__83 T__80))
          (:eq-idx T__213 (F__22 T__82 T__81))
          (:eq-idx T__214 X0__24)
          (:eq-idx T__215 Y0__25))
   :then (:model ((:eq-idx T__82 X0__24)
                  (:eq-idx T__83 Y0__25))))

Give the elaborated program
  $ autobill -M eqns_pack.bill
  decl sort idx
  decl type F : idx
  decl type Carrier : +
  decl type X0 : idx
  decl type Y0 : idx
  data Foo (A : idx) (B : idx) =
    | foo<X : idx, Y : idx>((Carrier X Y)) with X = (F Y A), Y = (F X B)
  decl val+ foor : _
  val+ fooz =
    bind/cc+ a__216 ->
      foo<X0, Y0>(foor).ret(a__216)

Give the remaining logical constraint
  $ autobill -M -C eqns_spec.bill
  (:forall (X0__50 Y0__51)
   :exists (X0__50 Y0__51 T__98 T__99 T__114 T__115 T__235 T__236 T__237 T__238)
   :assume ((:eq-idx X0__50 T__98)
            (:eq-idx Y0__51 T__99))
   :witness ((:eq-idx (Max3__23 T__114 T__114 T__115) (Max3__23 T__98 T__99 T__99))
             (:eq-idx T__114 T__235)
             (:eq-idx T__115 T__236)
             (:eq-idx T__235 X0__50)
             (:eq-idx T__236 Y0__51)
             (:eq-idx T__237 (Max3__23 T__114 T__114 T__115))
             (:eq-idx T__238 (Max3__23 T__98 T__99 T__99))) :then :true)

Give the elaborated program
  $ autobill -M eqns_spec.bill
  decl sort idx
  decl type Carrier : -
  decl type Max3 : idx
  comput Foo =
    | this.foo<X : idx, Y : idx>().ret((Carrier (Max3 X X Y)))
  comput Bar =
    | this.bar<X : idx, Y : idx>().ret((Carrier (Max3 X Y Y)))
  decl val- x : _
  val- y =
    bind/cc- a__239 ->
      cmd- val =
       match
         | this.bar<X0 : idx, Y0 : idx>().ret(a) -> x.foo<X0, Y0>().ret(a)
        end
      stk =
       this.ret(a__239)
      end
