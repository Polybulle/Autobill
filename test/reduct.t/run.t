Test that reduction works works
  $ autobill -r -s <<EOF
  > cmd do step GOT_TOP into this.bind x -> x.ret() end
  > EOF
  cmd<-> anon<13> : t<12> = step-
                              GOT_TOP
                            : t<18>
                            into
                              this.ret()
                            end

Test reduction with declarations
  $ autobill -s -r <<EOF
  > decl term y : top
  > cmd do step y into this.bind x -> x.ret() end
  > EOF
  decl term<-> y<12> : top
  cmd<-> anon<15> : t<14> = y<12>.ret()

Test shifting
  $ autobill -s -r <<EOF
  > cmd do
  >   term x = unit() in
  >   term y : (shift- unit) = match this.shift-().ret() -> x.ret() in
  >   y.ret()
  > cmd do
  >   shift+(GOT_TOP).match shift+(x) -> x.ret()
  cmd<-> anon<13> : t<12> =
    step-
      match
        case this.shift-().ret() : t<22> -> unit().ret()
      end
    : t<28>
    into
      this.ret()
    end
  cmd<-> anon<38> : t<37> = step-
                              GOT_TOP
                            : t<45>
                            into
                              this.ret()
                            end

Test function calls
  $ autobill -s -r <<EOF
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
  decl type a<12> : +
  decl type b<13> : +
  decl type c<14> : +
  decl term<+> x<15> : a<12>
  decl term<+> y<17> : b<13>
  decl term<+> z<19> : c<14>
  cmd<-> anon<22> : t<21> =
    step-
      match
        case this.shift-().ret() : t<38> -> tupple(y<17>, z<19>, x<15>).ret()
      end
    : t<34>
    into
      this.ret()
    end
