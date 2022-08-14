Test that reduction works works
  $ autobill simplify <<EOF
  > cmd do step GOT_TOP into this.bind x -> x.ret() end
  > EOF

Test reduction with declarations
  $ autobill simplify <<EOF
  > decl term y : top
  > cmd do step y into this.bind x -> x.ret() end
  > EOF

Test shifting
  $ autobill simplify <<EOF
  > cmd do
  >   term x = unit() in
  >   term y : (shift- unit) = match this.shift-().ret() -> x.ret() in
  >   y.ret()
