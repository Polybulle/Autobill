Test that reduction works works
  $ autobill simplify <<EOF
  > cmd do step GOT_TOP into this.bind x -> x.ret() end
  > EOF
  /* var x<1> : <t12> */
  /* tyvar t12 : - */
  cmd<-> anon<0> : <t10> = step-
                             GOT_TOP
                           : <t13>
                           into
                             this.ret()
                           end

Test reduction with declarations
  $ autobill simplify <<EOF
  > decl term y : top
  > cmd do step y into this.bind x -> x.ret() end
  > EOF
  /* var x<2> : <t13> */
  /* tyvar t13 : - */
  decl term<-> y<0> : top
  cmd<-> anon<1> : <t10> = y<0>.ret()

Test shifting
  $ autobill simplify <<EOF
  > cmd do
  >   term x = unit() in
  >   term y : (shift- unit) = match this.shift-().ret() -> x.ret() in
  >   y.ret()
  > cmd do
  >   shift+(GOT_TOP).match shift+(x) -> x.ret()
  /* var x<1> : <t13> */
  /* var y<2> : (shift- unit) */
  /* var x<4> : <t26> */
  /* tyvar t13 : + */
  /* tyvar t16 : + */
  /* tyvar t26 : + */
  cmd<-> anon<0> : <t10> =
    step-
      match
        case this.shift-().ret() : <t16> -> unit().ret()
      end
    : <t19>
    into
      this.ret()
    end
  cmd<-> anon<3> : <t22> = step-
                             GOT_TOP
                           : <t27>
                           into
                             this.ret()
                           end

Test function calls
  $ autobill simplify <<EOF
  > decl type a : +
  > decl type b : +
  > decl type c : +
  > decl term x : a
  > decl term y : b
  > decl term z : c
  > cmd do
  > term f =
  >   match this.call(x,y,z).ret() ->
  >     step
  >       match this.shift-().ret() -> tupple(y,z,x).ret()
  >     into
  >       this.ret()
  >     end
  > in
  >   f.call(x,y,z).ret()
