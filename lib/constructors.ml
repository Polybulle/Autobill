open Vars
open Util

type 'x constructor =
  | Unit
  | Pair of 'x * 'x
  | Fst of 'x
  | Snd of 'x
  | PosCons of Vars.ConsVar.t * 'x list
type ('x ,'a) destructor =
  | Call of 'x * 'a
  | Yes of 'a
  | No of 'a
  | NegCons of Vars.ConsVar.t * 'x list * 'a

let unit = Unit
let pair a b = Pair (a,b)
let fst a = Fst a
let snd b = Snd b
let poscons c args = PosCons (c,args)
let call x a = Call (x,a)
let yes a = Yes a
let no a = No a
let negcons c args cont = NegCons (c,args,cont)

let string_of_constructor k = function
  | Unit -> ":unit"
  | Pair (x,y) -> ":pair" ^ (string_of_tupple k [x;y])
  | Fst x -> ":fst(" ^ k x ^ ")"
  | Snd x -> ":snd(" ^ k x ^ ")"
  | PosCons (name, args) -> (Vars.ConsVar.to_string name) ^ (string_of_tupple k args)

let string_of_destructor kx ka = function
  | Call (x,a) -> Printf.sprintf "it.call(%s).%s" (kx x) (ka a)
  | Yes a -> "it.yes(" ^ ka a ^ ")"
  | No a -> "it.no(" ^ ka a ^ ")"
  | NegCons (name, args, a) ->
    let cons = ConsVar.to_string name in
    let tup = string_of_tupple kx args in
    let a = ka a in
    Printf.sprintf "it.%s%s.%s" cons tup a

let definition_of_constructor k = function
  | PosCons (name, args) ->
    (match args with
     | [] -> ConsVar.to_string name
     | first :: rest ->
       let args = List.fold_left (fun acc arg -> acc ^ " * " ^ k arg) (k first) rest in
       Printf.sprintf "%s of %s" (ConsVar.to_string name) args)
  | _ -> failwith "This is not a definable constructor"

let definition_of_destructor k = function
  | NegCons (name, args, cont) ->
    (match args with
     | [] -> ConsVar.to_string name ^ " cont " ^ k cont
     | first :: rest ->
       let args = List.fold_left (fun acc arg -> acc ^ " * " ^ k arg) (k first) rest in
       Printf.sprintf "%s of %s cont %s" (ConsVar.to_string name) args (k cont))
  | _ -> failwith "This is not a definable destructor"
