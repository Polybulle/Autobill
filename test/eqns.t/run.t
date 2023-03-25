Give the remaining logical constraint
  $ autobill -M -C eqns_pack.bill
  (:exists (T__80 T__81)
   :witness ()
   :with ((:eq-idx X0__24 (F__22 Y0__25 T__80))
          (:eq-idx Y0__25 (F__22 X0__24 T__81))
          (:eq-idx (F__22 Y0__25 T__80) X0__24)
          (:eq-idx (F__22 (F__22 Y0__25 T__80) T__81) (F__22 X0__24 T__81)))
   :then :true)

Give the elaborated program
  $ autobill -M eqns_pack.bill
  decl sort idx__21
  decl type F__22 : idx__21
  decl type Carrier__23 : +
  decl type X0__24 : idx__21
  decl type Y0__25 : idx__21
  data Foo__26 (A__29 : idx__21) (B__30 : idx__21) =
    | foo__33<X__31 : idx__21, Y__32 : idx__21>((Carrier__23 X__31 Y__32)) with X__31 = (F__22 Y__32 A__29), Y__32 = (F__22 X__31 B__30)
  decl val+ foor__46 : (Carrier__23 T__82 T__83)
  val+ fooz__48 : (Foo__26 T__80 T__81) =
    bind/cc+ a__216 : (Foo__26 T__80 T__81) ->
      foo__33<X0__24, Y0__25>(foor__46).ret(a__216)

Give the remaining logical constraint
  $ autobill -M -C eqns_spec.bill
  (:forall (X0__50 Y0__51)
   :let (T__114 T__115)
   :assume ((:eq-idx (Max3__23 T__114 T__114 T__115) (Max3__23 X0__50 Y0__51 Y0__51)))
   :then :true)

Give the elaborated program
  $ autobill -M eqns_spec.bill
  decl sort idx__21
  decl type Carrier__22 : -
  decl type Max3__23 : idx__21
  comput Foo__24 =
    | this.foo__30<X__28 : idx__21, Y__29 : idx__21>().ret((Carrier__22 (Max3__23 X__28 X__28 Y__29)))
  comput Bar__25 =
    | this.bar__33<X__31 : idx__21, Y__32 : idx__21>().ret((Carrier__22 (Max3__23 X__31 Y__32 Y__32)))
  decl val- x__46 : Foo__24
  val- y__48 : Bar__25 =
    bind/cc- a__239 : Bar__25 ->
      cmd- : Bar__25 val =
        match
          | this.bar__33<X0__50 : idx__21, Y0__51 : idx__21>().ret(a__52 : (Carrier__22 (Max3__23 T__114 T__114 T__115))) ->
            x__46.foo__30<X0__50, Y0__51>().ret(a__52)
        end
      stk =
        this.ret(a__239)
      end
