Give the remaining logical constraint
  $ autobill -M -C eqns_pack.bill
  (:exists (T__78 T__79)
   :witness ()
   :with ((:eq-idx X0__24 (F__22 Y0__25 T__78))
          (:eq-idx Y0__25 (F__22 X0__24 T__79))
          (:eq-idx (F__22 Y0__25 T__78) X0__24)
          (:eq-idx (F__22 (F__22 Y0__25 T__78) T__79) (F__22 X0__24 T__79)))
   :then :true)

Give the elaborated program
  $ autobill -M eqns_pack.bill
  decl sort idx__21
  decl type F__22 : idx__21
  decl type Carrier__23 : +
  decl type X0__24 : idx__21
  decl type Y0__25 : idx__21
  data Foo__26 (A__27 : idx__21) (B__28 : idx__21) =
    | foo__31<X__29 : idx__21, Y__30 : idx__21>((Carrier__23 X__29 Y__30)) with X__29 = (F__22 Y__30 A__27), Y__30 = (F__22 X__29 B__28)
  decl val+ foor__44 : (Carrier__23 T__80 T__81)
  val+ fooz__46 : (Foo__26 T__78 T__79) =
    bind/cc+ a__214 : (Foo__26 T__78 T__79) ->
      foo__31<X0__24, Y0__25>(foor__44).ret(a__214)

Give the remaining logical constraint
  $ autobill -M -C eqns_spec.bill
  (:forall (X0__48 Y0__49)
   :let (T__112 T__113)
   :assume ((:eq-idx (Max3__23 T__112 T__112 T__113) (Max3__23 X0__48 Y0__49 Y0__49)))
   :then :true)

Give the elaborated program
  $ autobill -M eqns_spec.bill
  decl sort idx__21
  decl type Carrier__22 : -
  decl type Max3__23 : idx__21
  comput Foo__24 =
    | this.foo__28<X__26 : idx__21, Y__27 : idx__21>().ret((Carrier__22 (Max3__23 X__26 X__26 Y__27)))
  comput Bar__25 =
    | this.bar__31<X__29 : idx__21, Y__30 : idx__21>().ret((Carrier__22 (Max3__23 X__29 Y__30 Y__30)))
  decl val- x__44 : Foo__24
  val- y__46 : Bar__25 =
    bind/cc- a__237 : Bar__25 ->
      cmd- : Bar__25 val =
        match
          | this.bar__31<X0__48 : idx__21, Y0__49 : idx__21>().ret(a__50 : (Carrier__22 (Max3__23 T__112 T__112 T__113))) ->
            x__44.foo__28<X0__48, Y0__49>().ret(a__50)
        end
      stk =
        this.ret(a__237)
      end
