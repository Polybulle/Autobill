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
  decl type F : idx__21
  decl type Carrier : +
  decl type X0 : idx__21
  decl type Y0 : idx__21
  data Foo (A__29 : idx__21) (B__30 : idx__21) =
    | foo<X__31 : idx__21, Y__32 : idx__21>((Carrier__23 X__31 Y__32)) with X__31 = (F__22 Y__32 A__29), Y__32 = (F__22 X__31 B__30)
  decl val+ foor : _
  val+ fooz__48 =
    bind/cc+ a__216 ->
      foo<X0__24, Y0__25>(foor).ret(a__216)

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
  decl type Max3 : idx__21
  comput Foo =
    | this.foo<X__28 : idx__21, Y__29 : idx__21>().ret((Carrier__22 (Max3__23 X__28 X__28 Y__29)))
  comput Bar =
    | this.bar<X__31 : idx__21, Y__32 : idx__21>().ret((Carrier__22 (Max3__23 X__31 Y__32 Y__32)))
  decl val- x : _
  val- y__48 =
    bind/cc- a__239 ->
      cmd- val =
        match
          | this.bar<X0__50 : idx__21, Y0__51 : idx__21>().ret(a__52) -> x.foo<X0__50, Y0__51>().ret(a)
        end
      stk =
        this.ret(a__239)
      end
