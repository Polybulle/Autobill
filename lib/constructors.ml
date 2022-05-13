open Vars

  type 't pos_type_cons =
    | Unit
    | Zero
    | Prod of 't * 't
    | Sum of 't * 't
    | Cons of Vars.TyVar.t * 't list
    (*let unit = Unit*)
  let unit_t = Unit
  let zero = Zero
  let prod a b = Prod (a,b)
  let sum a b = Sum (a,b)
  let posvar v = Cons (v, [])
  let postype v args = Cons (v,args)
  let string_of_pos_type_cons k = function
    | Unit -> "+unit"
    | Zero -> "+zero"
    | Prod (a,b) -> pp_texp "+prod" [k a; k b]
    | Sum (a,b) -> pp_texp "+sum" [k a; k b]
    | Cons (var,args) -> pp_texp (Vars.TyVar.to_string var) (List.map k args)

  type 't neg_type_cons =
    | Top
    | Bottom
    | Fun of 't * 't
    | Choice of 't * 't
    | Cons of Vars.TyVar.t * 't list
  let top = Top
  let bottom = Bottom
  let func a b = Fun (a,b)
  let choice a b = Choice (a,b)
  let negvar v = Cons (v, [])
  let negtype v args = Cons (v,args)
  let string_of_neg_type_cons k = function
    | Top -> "-top"
    | Bottom -> "-bottom"
    | Fun (a,b) -> pp_texp "-fun" [k a; k b]
    | Choice (a,b) -> pp_texp "-choice" [k a; k b]
    | Cons (var,args) -> pp_texp (Vars.TyVar.to_string var) (List.map k args)

  type 'x constructor =
    | Unit
    | Pair of 'x * 'x
    | Fst of 'x
    | Snd of 'x
    | PosCons of Vars.ConsVar.t * 'x list
  let unit = Unit
  let pair a b = Pair (a,b)
  let fst a = Fst a
  let snd b = Snd b
  let poscons c args = PosCons (c,args)
  let string_of_constructor k = function
    | Unit -> ":unit"
    | Pair (x,y) -> pp_sexp ":pair" [k x; k y]
    | Fst x -> pp_sexp ":fst" [k x]
    | Snd y -> pp_sexp ":snd" [k y]
    | PosCons (name, args) -> pp_sexp (Vars.ConsVar.to_string name) (List.map k args)
let definition_of_constructor k = function
  | PosCons (name, args) ->
      (match args with
      | [] -> ConsVar.to_string name
      | first :: rest ->
        let args = List.fold_left (fun acc arg -> acc ^ " * " ^ k arg) (k first) rest in
        Printf.sprintf "%s of %s" (ConsVar.to_string name) args)
    | _ -> failwith "This is not a definable constructor"

  type ('x ,'a) destructor =
    | Call of 'x * 'a
    | Yes of 'a
    | No of 'a
    | NegCons of Vars.ConsVar.t * 'x list * 'a
  let call x a = Call (x,a)
  let yes a = Yes a
  let no a = No a
  let negcons c args cont = NegCons (c,args,cont)
  let string_of_destructor kx ka = function
    | Call (x,a) -> pp_sexp ":call" [kx x; ka a]
    | Yes a -> pp_sexp ":yes" [ka a]
    | No a -> pp_sexp ":no" [ka a]
    | NegCons (name, args, a) -> pp_sexp (Vars.ConsVar.to_string name) (ka a :: List.map kx args)
let definition_of_destructor k = function
    | NegCons (name, args, cont) ->
      (match args with
      | [] -> ConsVar.to_string name ^ " cont " ^ k cont
      | first :: rest ->
        let args = List.fold_left (fun acc arg -> acc ^ " * " ^ k arg) (k first) rest in
        Printf.sprintf "%s of %s cont %s" (ConsVar.to_string name) args (k cont))
    | _ -> failwith "This is not a definable destructor"
