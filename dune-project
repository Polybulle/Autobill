(lang dune 3.15)
(using menhir 2.0)
(name autobill)
(cram enable)

(package 
 (name autobill)
 (synopsis "Prototype resource analyser based on linear call-by-push-value")
 (allow_empty)
 (depends
  ocaml
  dune
  menhir
  js_of_ocaml
  js_of_ocaml-toplevel
  js_of_ocaml-ppx))
