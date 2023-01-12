Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (exists t__44, t__45, t__46, t__47.
    t__46 = (f t__47 t__44) & t__47 = (f t__46 t__45)
    & t__46 = x0 & t__47 = y0 & t__46 = x0 & t__47 = y0)
  

Give the elaborated program
  $ autobill eqns_pack.bill
  decl type f : idx (s, s) -> s
  decl type carrier : type (s, s) -> +
  decl type x0 : s
  decl type y0 : s
  data foo_t (a : s) (b : s) =
    | Foo<x : s, y : s>((carrier x y)) with x = (f y a) & y = (f x b)
  decl val+ foor : (carrier t__46 t__47)
  val+ fooz : (foo_t t__44 t__45) =
    bind/cc+ a__71 : (foo_t t__44 t__45) -> Foo<x0, y0>(foor).ret(a__71)

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (forall x0, y0. exists t__61, t__62, t__65, t__66.
    
    => t__62 = y0 & t__61 = x0
       & t__65 = x0 & t__66 = y0
         & (max3 t__65 t__65 t__66) = (max3 t__61 t__62 t__62))
  

Give the elaborated program
  $ autobill eqns_spec.bill
  decl type carrier : type s -> -
  decl type max3 : idx (s, s, s) -> s
  comput foo_t =
    | this.Foo<x : s, y : s>().ret((carrier (max3 x x y)))
  comput bar_t =
    | this.Bar<x : s, y : s>().ret((carrier (max3 x y y)))
  decl val- x : foo_t
  val- y : bar_t =
    bind/cc- a__85 : bar_t ->
      cmd- : bar_t
      val =
        match
          | this.Bar<x0 : s, y0 : s>().ret(a
                                             : (carrier
                                                 (max3 t__65 t__65 t__66))) ->
            x.Foo<x0, y0>().ret(a)
        end
      stk =
        this.ret(a__85)
      end
