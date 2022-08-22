
let first_order_type_infer ~trace:trace (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make (P) in
  let items = go ~trace items in
  (prelude, items)
