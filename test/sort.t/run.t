Test the sort inference
  $ autobill sort test.bill
  decl type int<12> : +
  decl type char<13> : +
  data list<14> (c<15> : +) =
    case :nil<16>()
    case :cons<17>(c<15>, (list<14> c<15>))
  /* constructor "nil<16>" is
      forall (c<15> : +). nil<16>() : (list<14> c<15>)*/
  /* constructor "cons<17>" is forall (c<15> : +). cons<17>(c<15>,
      (list<14> c<15>)) : (list<14> c<15>)*/
  decl term<+> z<18> : int<12>
  decl term<-> succ<20> : (fun int<12> -> (shift- int<12>))
  term<+> lz<22> : t<29> = :cons<17>(z<18>, :cons<17>(z<18>, :nil<16>()))
  decl term<-> map<30>
                 : (fun (shift+ (fun int<12> -> (shift- char<13>)))
                     (list<14> int<12>) -> (shift- (list<14> char<13>)))
