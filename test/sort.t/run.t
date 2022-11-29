Test the sort inference
  $ autobill -s test.bill
  decl type int : +
  decl type char : +
  data list (c : +) =
    case nil()
    case cons(c, (list c))
  /* constructor "nil" is forall (c : +). nil() : (list c)*/
  /* constructor "cons" is forall (c : +). cons(c, (list c)) : (list c)*/
  /* tyvar t__23 : + */
  /* tyvar t__24 : + */
  /* tyvar t__25 : + */
  /* tyvar t__26 : + */
  /* tyvar t__27 : + */
  /* tyvar t__29 : + */
  decl val+ z : int
  decl val- succ : (fun (thunk int) int)
  val+ lz : t__29 = :cons(z, :cons(z, :nil()))
  decl val- map
              : (fun (thunk (list char)) (closure (fun (thunk char) int))
                  (list int))
