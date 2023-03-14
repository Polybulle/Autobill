Give the remaining logical constraint
  $ autobill -M -C eqns_pack.bill
  (:exists (T__76 T__77 T__78 T__79 T__206 T__207 T__208 T__209 T__210 T__211)
   :witness (T__204 T__205)
   :with ((:eq-idx T__78 (F__20 T__79 T__76))
          (:eq-idx T__78 T__204)
          (:eq-idx T__79 (F__20 T__78 T__77))
          (:eq-idx T__79 T__205)
          (:eq-idx T__204 X0__22)
          (:eq-idx T__205 Y0__23)
          (:eq-idx T__206 Y0__23)
          (:eq-idx T__207 X0__22)
          (:eq-idx T__208 Y0__23)
          (:eq-idx T__209 X0__22)
          (:eq-idx T__210 (F__20 T__78 T__77))
          (:eq-idx T__211 (F__20 T__79 T__76)))
   :then (:model ((:eq-idx T__78 X0__22)
                  (:eq-idx T__79 Y0__23))))

Give the elaborated program
  $ autobill -M eqns_pack.bill
  decl sort idx__19
  decl type F__20 : idx__19
  decl type Carrier__21 : +
  decl type X0__22 : idx__19
  decl type Y0__23 : idx__19
  data Foo__24 (A__25 : idx__19) (B__26 : idx__19) =
    | foo__29<X__27 : idx__19, Y__28 : idx__19>((Carrier__21 X__27 Y__28)) with X__27 = (F__20 Y__28 A__25), Y__28 = (F__20 X__27 B__26)
  decl val+ foor__42 : (Carrier__21 T__78 T__79)
  val+ fooz__44 : (Foo__24 T__76 T__77) =
    bind/cc+ a__212 : (Foo__24 T__76 T__77) ->
      foo__29<X0__22, Y0__23>(foor__42).ret(a__212)

Give the remaining logical constraint
  $ autobill -M -C eqns_spec.bill
  (:forall (X0__46 Y0__47 T__94 T__95)
   :let (T__110 T__111 T__231 T__232 T__233 T__234)
   :assume ((:eq-idx (Max3__21 T__110 T__110 T__111) (Max3__21 T__94 T__95 T__95))
            (:eq-idx X0__46 T__94)
            (:eq-idx Y0__47 T__95)
            (:eq-idx T__233 (Max3__21 T__110 T__110 T__111))
            (:eq-idx T__234 (Max3__21 T__94 T__95 T__95)))
   :then (:exists ()
          :witness (T__231 T__232)
          :with ((:eq-idx T__110 T__231)
                 (:eq-idx T__111 T__232)
                 (:eq-idx T__231 X0__46)
                 (:eq-idx T__232 Y0__47))
          :then :true))

Give the elaborated program
  $ autobill -M eqns_spec.bill
  decl sort idx__19
  decl type Carrier__20 : -
  decl type Max3__21 : idx__19
  comput Foo__22 =
    | this.foo__26<X__24 : idx__19, Y__25 : idx__19>().ret((Carrier__20 (Max3__21 X__24 X__24 Y__25)))
  comput Bar__23 =
    | this.bar__29<X__27 : idx__19, Y__28 : idx__19>().ret((Carrier__20 (Max3__21 X__27 Y__28 Y__28)))
  decl val- x__42 : Foo__22
  val- y__44 : Bar__23 =
    bind/cc- a__235 : Bar__23 ->
      cmd- : Bar__23 val =
        match
          | this.bar__29<X0__46 : idx__19, Y0__47 : idx__19>().ret(a__48 : (Carrier__20 (Max3__21 T__110 T__110 T__111))) ->
            x__42.foo__26<X0__46, Y0__47>().ret(a__48)
        end
      stk =
        this.ret(a__235)
      end
