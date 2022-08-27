Test a simple pack/spec program.

  $ autobill simplify <<EOF
  > pack t (a : +) = :c[](unit)
  > spec u (a : -) = this.d[]().ret() : a
  > 
  > decl type c : -
  > decl term x : c
  > decl type d : +
  > decl term z : d
  > 
  > term y = :c[](unit())
  > term y = match this.d[]().ret() -> x.ret()
  > cmd do x.d[]().ret()
  > cmd do z.match :c[](y) -> z.ret()
  > cmd do :c[](unit()).match :c[](x) -> x.ret()
  pack t<12> (a<16> : +) : + = c<17>[](unit)
  pack u<13> (a<18> : -) : - = this.d<19>[]().ret() : a<18>
  decl type c<14> : -
  decl type d<15> : +
  /* constructor "c<17>" is forall (a<16> : +). c<17>(unit) : (t<12> a<16>)*/
  /* destructor "d<19>" is forall (a<18> : -). d<19>().ret(a<18>) : (u<13>
                                                                      a<18>)*/
  decl term<-> x<20> : c<14>
  decl term<+> z<22> : d<15>
  term<+> y<24> : t<28> = c<17>[](unit())
  term<-> y<29> : t<38> = match this.d<19>[]().ret() : t<30> -> x<20>.ret()
  cmd<-> anon<40> : t<39> = x<20>.d<19>[]().ret()
  cmd<+> anon<50> : t<49> = z<22>match c<17>[](y<54> : t<55>) -> z<22>.ret()
  cmd<+> anon<66> : t<65> = unit().ret()
