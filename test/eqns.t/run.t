Give the remaining logical constraint
  $ autobill -L -e eqns_pack.bill
  (:exists (T_80 T_81 T_82 T_83 T_214 T_215 T_216 T_217 T_218 T_219 T_220 T_221)
   :witness ()
   :with ((:eq-idx T_82 (F_22 T_83 T_80))
          (:eq-idx T_82 T_215)
          (:eq-idx T_83 (F_22 T_82 T_81))
          (:eq-idx T_83 T_214)
          (:eq-idx T_214 Y0_25)
          (:eq-idx T_215 X0_24)
          (:eq-idx T_216 Y0_25)
          (:eq-idx T_217 X0_24)
          (:eq-idx T_218 (F_22 T_83 T_80))
          (:eq-idx T_219 (F_22 T_82 T_81))
          (:eq-idx T_220 X0_24)
          (:eq-idx T_221 Y0_25))
   :then (:model ((:eq-idx T_82 X0_24)
                  (:eq-idx T_83 Y0_25))))

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
    foo<X0, Y0>(foor)

Give the remaining logical constraint
  $ autobill -L -e eqns_spec.bill
  (:forall (X0_50 Y0_51)
   :exists (X0_50 Y0_51 T_98 T_99 T_114 T_115 T_241 T_242 T_243 T_244)
   :assume ((:eq-idx X0_50 T_98)
            (:eq-idx Y0_51 T_99))
   :witness ((:eq-idx (Max3_23 T_114 T_114 T_115) (Max3_23 T_98 T_99 T_99))
             (:eq-idx T_114 T_241)
             (:eq-idx T_115 T_242)
             (:eq-idx T_241 X0_50)
             (:eq-idx T_242 Y0_51)
             (:eq-idx T_243 (Max3_23 T_114 T_114 T_115))
             (:eq-idx T_244 (Max3_23 T_98 T_99 T_99))) :then :true)

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
    match this.bar<X0 : idx, Y0 : idx>().ret(a) ->
    cmd- val =
      x
    stk =
      this.foo<X0, Y0>().ret(a)
