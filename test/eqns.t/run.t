Give the remaining logical constraint
  $ autobill -M -C eqns_pack.bill
  (:exists (T__44 T__45 T__46 T__47 T__49 T__50 T__51 T__52 T__63 T__64)
   :witness ((:eq T__46 (F T__47 T__44)) (:eq T__47 (F T__46 T__45))
             (:eq T__49 (F T__47 T__44)) (:eq T__50 (F T__46 T__45))
             (:eq T__51 X0) (:eq T__52 Y0) (:eq T__63 X0) (:eq T__64 Y0))
   :then (:and
          ((:eq T__47 Y0))
          ((:eq T__46 X0))
          ((:eq T__46 X0) (:eq T__47 Y0))))
  

Give the elaborated program
  $ autobill -M eqns_pack.bill
  decl sort idx
  
  decl type F : idx
  decl type Carrier : +
  decl type X0 : idx
  decl type Y0 : idx
  data Foo (A : idx) (B : idx) =
    | foo<X : idx, Y : idx>((Carrier X Y)) with X = (F Y A), Y = (F X B)
  decl val+ foor : (Carrier T__46 T__47)
  val+ fooz : (Foo T__44 T__45) =
    bind/cc+ a__66 : (Foo T__44 T__45) ->
      foo<X0, Y0>(foor).ret(a__66)

Give the remaining logical constraint
  $ autobill -M -C eqns_spec.bill
  (:forall (X0 Y0)
   :exists (T__60 T__61 T__64 T__65 T__72 T__79)
   :assume ((:eq T__72 (Max3 T__64 T__64 T__65))
            (:eq T__79 (Max3 T__60 T__61 T__61)))
   :then (:and
          ((:eq T__64 X0))
          ((:eq T__65 Y0))
          ((:eq T__60 X0))
          ((:eq T__61 Y0))
          ((:eq (Max3 T__64 T__64 T__65) (Max3 T__60 T__61 T__61)))))
  

Give the elaborated program
  $ autobill -M eqns_spec.bill
  decl sort idx
  
  decl type Carrier : -
  decl type Max3 : idx
  comput Foo =
    | this.foo<X : idx, Y : idx>().ret((Carrier (Max3 X X Y)))
  comput Bar =
    | this.bar<X : idx, Y : idx>().ret((Carrier (Max3 X Y Y)))
  decl val- x : Foo
  val- y : Bar =
    bind/cc- a__86 : Bar ->
      cmd- : Bar val =
        match
          | this.bar<X0 : idx, Y0 : idx>().ret(a : (Carrier (Max3 T__64 T__64 T__65))) -> x.foo<X0, Y0>().ret(a)
        end
      stk =
        this.ret(a__86)
      end
