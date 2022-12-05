open Misc

let cons_names = ["unit"; "pair"; "left"; "right"; "thunk"]

type ('var, 'x) constructor =
  | Unit
  | Thunk of 'x
  | Tupple of 'x list
  | Inj of int * int * 'x
  | PosCons of 'var * 'x list

let unit = Unit
let pair a b = Tupple [a;b]
let left a = Inj (0, 2, a)
let right b = Inj (1, 2, b)
let thunk a = Thunk a
let poscons c args = PosCons (c,args)

let string_of_constructor kvar k = function
  | Unit -> "unit()"
  | Thunk x -> "thunk(" ^ k x ^ ")"
  | Tupple t -> "pair" ^ (string_of_tupple k t)
  | Inj (i,n,x) ->
    "inj(" ^ string_of_int i ^ "/" ^ string_of_int n ^ ", " ^ k x ^ ")"
  | PosCons (name, args) -> (kvar name) ^ (string_of_tupple k args)

let consvar_of_constructor = function
  | PosCons (name, _) -> Some name
  | _ -> None

let destr_names = ["call"; "yes"; "no"; "closure"]

type ('var, 'x ,'a) destructor =
  | Call of 'x list * 'a
  | Proj of int * int * 'a
  | Closure of 'a
  | NegCons of 'var * 'x list * 'a

let call x a = Call (x,a)
let yes a = Proj (0, 2, a)
let no a = Proj (1, 2, a)
let closure a = Closure a
let negcons c args cont = NegCons (c,args,cont)

let string_of_destructor kvar kx ka = function
  | Call (x,a) -> Printf.sprintf ".call%s%s" (string_of_tupple kx x) (ka a)
  | Proj (i, n, a) -> ".proj(" ^ string_of_int i ^ "/" ^ string_of_int n ^ ")" ^ ka a
  | Closure a -> ".closure()" ^ ka a
  | NegCons (name, args, a) ->
    let cons = kvar name in
    let tup = string_of_tupple kx args in
    let a = ka a in
    Printf.sprintf ".%s%s%s" cons tup a

let destrvar_of_destructor = function
  | NegCons (name, _, _) -> Some name
  | _ -> None
