

let constraint_as_string (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let x,_ = elab_prog_items items in
  let s1 = Sexpr.to_string (con_to_sexpr (fun n -> V(string_of_int n)) x) in
  let s2 = Sexpr.to_string (subst_to_sexpr !_state) in
  s1 ^ "\n" ^ s2

let post_contraint_as_string (prelude, items, post) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let s1 = Sexpr.to_string (Constraint.post_con_to_sexpr (fun n -> V (string_of_int n)) post) in
  let s2 = Sexpr.to_string (subst_to_sexpr !_state) in
  s1 ^ "\n" ^ s2

let type_infer ~trace:trace (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make (P) in
  let items,post = go ~trace items in
  (prelude, items, post)
