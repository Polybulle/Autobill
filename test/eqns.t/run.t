Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (exists t__40, t__41, t__42, t__43.
    t__42 = (f t__43 t__40) & t__43 = (f t__42 t__41)
    & t__42 = x0 & t__43 = y0 & t__43 = y0 & t__42 = x0)
  

Give the elaborated program
  $ autobill eqns_pack.bill
  decl type f : (idx -> (idx -> idx))
  decl type carrier : (idx -> (idx -> +))
  decl type x0 : idx
  decl type y0 : idx
  pack foo_t (a : idx) (b : idx) : (idx -> (idx -> +)) = foo[x : idx,
    y : idx]((carrier x y)) with x = (f y a) & y = (f x b)
  decl val+ foor : (carrier x0 y0)
  val+ fooz : (foo_t t__40 t__41) =
    bind/cc+ a__58 : (foo_t t__40 t__41) ->
      cmd+ : (foo_t t__40 t__41)
      val =
        foo[x0, y0](foor)
      stk =
        this.ret(a__58)
      end

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (forall x0, y0. exists t__58, t__59, t__64, t__65.
    
    => t__59 = y0 & t__58 = x0
       & t__64 = x0 & t__65 = y0
         & (max3 t__64 t__64 t__65) = (max3 t__58 t__59 t__59))
  

Give the elaborated program
  $ autobill eqns_spec.bill
  decl type carrier : (idx -> -)
  decl type max3 : (idx -> (idx -> (idx -> idx)))
  spec foo_t : - = this.foo[x : idx, y : idx]().ret((carrier (max3 x x y)))
  spec bar_t : - = this.bar[x : idx, y : idx]().ret((carrier (max3 x y y)))
  decl val- x : foo_t
  val- y : bar_t =
    bind/cc- a__80 : bar_t ->
      cmd- : bar_t
      val =
        match this.bar[x0 : idx,
        y0 : idx]().ret(a : (carrier (max3 t__58 t__59 t__59))) -> x.foo[x0,
        y0]().ret(a)
      stk =
        this.ret(a__80)
      end
