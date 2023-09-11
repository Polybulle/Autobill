(*let test1 =
  (6 = (do {
    let z <- {
      let baba <- 1;
      let kiki <- 2;
      pure (baba+kiki)
    };
    pure (z+z)
  }));;*)

let test2 = do {
  let x := 1;
  pure x
};;

0
