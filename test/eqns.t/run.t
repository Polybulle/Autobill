Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (exists T__34, T__35, T__36, T__37.
    T__36 = (F T__37 T__34) & T__37 = (F T__36 T__35)
    & T__36 = X0 & T__37 = Y0 & T__36 = X0 & T__37 = Y0)
  

Give the elaborated program
  $ autobill eqns_pack.bill
  decl sort idx
  
  decl type F : idx
  decl type Carrier : +
  decl type X0 : idx
  decl type Y0 : idx
  data Foo (A : idx) (B : idx) =
    | foo<X : idx, Y : idx>((Carrier X Y)) with X = (F Y A) & Y = (F X B)
  decl val+ foor : (Carrier T__36 T__37)
  val+ fooz : (Foo T__34 T__35) =
    bind/cc+ a__56 : (Foo T__34 T__35) ->
      foo<X0, Y0>(foor).ret(a__56)

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (forall X0, Y0. exists T__50, T__51, T__54, T__55.
    
    => (Max3 T__54 T__54 T__55) = (Max3 T__50 T__51 T__51) & T__51 = Y0
       & T__50 = X0 & T__55 = Y0 & T__54 = X0)
  

Give the elaborated program
  $ autobill eqns_spec.bill
  decl sort idx
  
  decl type Carrier : -
  decl type Max3 : idx
  comput Foo =
    | this.foo<X : idx, Y : idx>().ret((Carrier (Max3 X X Y)))
  comput Bar =
    | this.bar<X : idx, Y : idx>().ret((Carrier (Max3 X Y Y)))
  decl val- x : Foo
  val- y : Bar =
    bind/cc- a__76 : Bar ->
      cmd- : Bar val =
        match
          | this.bar<X0 : idx, Y0 : idx>().ret(a : (Carrier (Max3 T__54 T__54 T__55))) -> x.foo<X0, Y0>().ret(a)
        end
      stk =
        this.ret(a__76)
      end
