type 'a list =
| Cons of 'a *  ('a list)
| Nil
;;
let rec append l1 l2 =
  (match l1 with
    | [] -> l2
    | (x::xs) -> (x::(append xs l2)))
;;
let hello = [72;101;108;108;111] in
let world = [87;111;114;108;100] in
let hello1 = (append hello [32]) in
let world1 = (append world [33]) in
(append hello1 world1)
