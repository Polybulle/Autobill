Test the sort inference
  $ autobill -s test.bill
  decl type int : +
  decl type char : +
  data list (c : +) =
    case nil()
    case cons(c, (list c))
  decl val+ z : int
  decl val- succ : (fun (thunk int) int)
  val+ lz : t__29 = :cons(z, :cons(z, :nil()))
  decl val- map
              : (fun (thunk (list char)) (closure (fun (thunk char) int))
                  (list int))
