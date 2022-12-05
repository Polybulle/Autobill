Give the remaining logical constraint
  $ autobill -C eqns_pack.bill
  (∃ (t__41 t__42 t__43 t__44)
    (& (= t__43 (f t__44 t__41)) (= t__44 (f t__43 t__42)))
    (Loc eqns_pack.bill:17:11-27
      (& (& (= x0 t__43) (= y0 t__44)) (= t__44 y0) (= t__43 x0))))

Give the elaborated program
  $ autobill eqns_pack.bill
  decl type f : (idx -> (idx -> idx))
  decl type carrier : (idx -> (idx -> +))
  decl type x0 : idx
  decl type y0 : idx
  pack foo_t (a : idx) (b : idx) : (idx -> (idx -> +)) = foo[(x : idx),
    (y : idx)]((carrier x y)) where (x = (f y a)) (y = (f x b))
  decl val+ foor : (carrier x0 y0)
  val+ fooz : (foo_t t__41 t__42) =
    bind/cc+ a__77 : (foo_t t__41 t__42) ->
      cmd+
      : (foo_t t__41 t__42) val =
        foo[x0, y0](foor)
      stk =
        this.ret(a__77)
      end

Give the remaining logical constraint
  $ autobill -C eqns_spec.bill
  (Loc eqns_spec.bill:16:8-71
    (∀ (t__59 t__60) T
      (∃ (x0 y0 t__71 t__72) T
        (& (= t__60 y0) (= t__59 x0)
          (Loc eqns_spec.bill:16:52-71
            (& (= t__71 x0) (= t__72 y0)
              (= (max3 t__59 t__60 t__60) (max3 t__71 t__71 t__72))))))))

Give the elaborated program
  $ autobill eqns_spec.bill
  decl type carrier : (idx -> -)
  decl type max3 : (idx -> (idx -> (idx -> idx)))
  spec foo_t : - = this.foo[(x : idx),
    (y : idx)]().ret() : (carrier (max3 x x y))
  spec bar_t : - = this.bar[(x : idx),
    (y : idx)]().ret() : (carrier (max3 x y y))
  decl val- x : foo_t
  val- y : bar_t =
    bind/cc- a__97 : bar_t ->
      cmd-
      : bar_t val =
        match this.bar[(x0 : idx),
        (y0 : idx)]()a : (carrier (max3 t__59 t__60 t__60)) -> x.foo[x0, y0]()
        .ret(a)
      stk =
        this.ret(a__97)
      end
