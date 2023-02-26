Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (exists T__44, T__45, T__46, T__47.
    T__46 = (F T__47 T__44) & T__47 = (F T__46 T__45)
    & T__46 = X0 & T__47 = Y0 & T__46 = X0 & T__47 = Y0)
  

Give the elaborated program
  $ autobill eqns_pack.bill
  decl sort idx
  
  decl type F : (idx -> (idx -> idx))
  decl type Carrier : (idx -> (idx -> +))
  decl type X0 : idx
  decl type Y0 : idx
  data Foo (A : idx) (B : idx) =
    | foo<X : idx, Y : idx>((Carrier X Y)) with X = (F Y A) & Y = (F X B)
  decl val<<+>> foor : (Carrier T__46 T__47)
  val<<+>> fooz : (Foo T__44 T__45) =
    bind/cc<<+>> a__62 : (Foo T__44 T__45) -> foo<X0, Y0>(foor).ret(a__62)

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (forall X0, Y0. exists T__63, T__64, T__67, T__68.
    
    => T__64 = Y0 & T__63 = X0
       & T__67 = X0 & T__68 = Y0
         & (Max3 T__67 T__67 T__68) = (Max3 T__63 T__64 T__64))
  

Give the elaborated program
  $ autobill eqns_spec.bill
  decl sort idx
  
  decl type Carrier : (idx -> -)
  decl type Max3 : (idx -> (idx -> (idx -> idx)))
  comput Foo =
    | this.foo<X : idx, Y : idx>().ret((Carrier (Max3 X X Y)))
  comput Bar =
    | this.bar<X : idx, Y : idx>().ret((Carrier (Max3 X Y Y)))
  decl val<<->> x : Foo
  val<<->> y : Bar =
    bind/cc<<->> a__83 : Bar ->
      cmd<<->> : Bar
      val =
        match
          | this.bar<X0 : idx, Y0 : idx>().ret(a : (Carrier (Max3 T__67 T__67 T__68))) -> x.foo<X0, Y0>().ret(a)
        end
      stk =
        this.ret(a__83)
      end
