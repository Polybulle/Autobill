open Util

let type_cons_names =
  ["unit"; "zero"; "top"; "bottom"; "prod"; "sum"; "fun"; "choice"]

type ('tycons, 't) type_cons =
  | Unit
  | Zero
  | Top
  | Bottom
  | Prod of 't * 't
  | Sum of 't * 't
  | Fun of 't * 't
  | Choice of 't * 't
  | Cons of 'tycons * 't list

let unit_t = Unit
let zero = Zero
let top = Top
let bottom = Bottom
let prod a b = Prod (a,b)
let sum a b = Sum (a,b)
let func a b = Fun (a,b)
let choice a b = Choice (a,b)
let typecons v args = Cons (v,args)

let string_of_type_cons kvar k cons =

  let pp_texp cons args =
    Util.paren (List.fold_left (fun a b -> a ^ " " ^ b) cons args) in

  match cons with
  | Unit -> "unit"
  | Zero -> "zero"
  | Top -> "top"
  | Bottom -> "bottom"
  | Prod (a,b) -> pp_texp "prod" [k a; k b]
  | Sum (a,b) -> pp_texp "sum" [k a; k b]
  | Fun (a,b) -> pp_texp "fun" [k a; k b]
  | Choice (a,b) -> pp_texp "choice" [k a; k b]
  | Cons (var,args) -> pp_texp (kvar var) (List.map k args)


let cons_names = ["unit"; "pair"; "left"; "right"]

type ('var, 'x) constructor =
  | Unit
  | Pair of 'x * 'x
  | Left of 'x
  | Right of 'x
  | PosCons of 'var * 'x list

let unit = Unit
let pair a b = Pair (a,b)
let left a = Left a
let right b = Right b
let poscons c args = PosCons (c,args)

let string_of_constructor kvar k = function
  | Unit -> "unit()"
  | Pair (x,y) -> "pair" ^ (string_of_tupple k [x;y])
  | Left x -> "left(" ^ k x ^ ")"
  | Right x -> "right(" ^ k x ^ ")"
  | PosCons (name, args) -> (kvar name) ^ (string_of_tupple k args)

let consvar_of_constructor = function
  | PosCons (name, _) -> Some name
  | _ -> None

let destr_names = ["call"; "yes"; "no"]

type ('var, 'x ,'a) destructor =
  | Call of 'x * 'a
  | Yes of 'a
  | No of 'a
  | NegCons of 'var * 'x list * 'a

let call x a = Call (x,a)
let yes a = Yes a
let no a = No a
let negcons c args cont = NegCons (c,args,cont)

let string_of_destructor kvar kx ka = function
  | Call (x,a) -> Printf.sprintf ".call(%s)%s" (kx x) (ka a)
  | Yes a -> ".yes()" ^ ka a
  | No a -> ".no()" ^ ka a
  | NegCons (name, args, a) ->
    let cons = kvar name in
    let tup = string_of_tupple kx args in
    let a = ka a in
    Printf.sprintf ".%s%s%s" cons tup a

let destrvar_of_destructor = function
  | NegCons (name, _, _) -> Some name
  | _ -> None
