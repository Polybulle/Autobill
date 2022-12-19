Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (exists t__40, t__41, t__42, t__43.
    t__42 = (f t__43 t__40) & t__43 = (f t__42 t__41)
    & t__42 = x0 & t__43 = y0 & t__42 = x0 & t__43 = y0)
  

Give the elaborated program
  $ autobill eqns_pack.bill
  decl type f : (idx -> (idx -> idx))
  decl type carrier : (idx -> (idx -> +))
  decl type x0 : idx
  decl type y0 : idx
  data foo_t (a : idx) (b : idx) =
    case Foo<x : idx, y : idx>((carrier x y)) with x = (f y a) & y = (f x b)
  decl val+ foor : (carrier t__42 t__43)
  val+ fooz : (foo_t t__40 t__41) =
    bind/cc+ a__58 : (foo_t t__40 t__41) -> Foo<x0, y0>(foor).ret(a__58)

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (forall x0, y0. exists t__57, t__58, t__61, t__62.
    
    => t__58 = y0 & t__57 = x0
       & t__61 = x0 & t__62 = y0
         & (max3 t__61 t__61 t__62) = (max3 t__57 t__58 t__58))
  

Give the elaborated program
  $ autobill eqns_spec.bill
  decl type carrier : (idx -> -)
  decl type max3 : (idx -> (idx -> (idx -> idx)))
  comput foo_t =
    case this.Foo<x : idx, y : idx>().ret((carrier (max3 x x y)))
  comput bar_t =
    case this.Bar<x : idx, y : idx>().ret((carrier (max3 x y y)))
  decl val- x : foo_t
  val- y : bar_t =
    bind/cc- a__77 : bar_t ->
      cmd- : bar_t
      val =
        match
          case this.Bar<x0 : idx, y0 : idx>().ret(a
                                                    : (carrier
                                                        (max3 t__61 t__61
                                                          t__62))) -> x
            .Foo<x0, y0>().ret(a)
        end
      stk =
        this.ret(a__77)
      end
