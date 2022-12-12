open Format

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

let destrvar_of_destructor = function
  | NegCons (name, _, _) -> Some name
  | _ -> None

let pp_comma_sep fmt () = fprintf fmt ",@, "

let pp_constructor pp_cons pp_k fmt cons =
  match cons with
  | Unit -> pp_print_string fmt "unit()"
  | Tupple vs ->
    fprintf fmt "@[<hov 2>tupple(%a)@]" (pp_print_list ~pp_sep:pp_comma_sep pp_k) vs
  | Inj (i,n,x) -> fprintf fmt "inj(%n/%n, %a)" i n pp_k x
  | Thunk x -> fprintf fmt "thunk(%a)" pp_k x
  | PosCons (c, args) ->
    fprintf fmt "%a(@[<hov 2>%a@])"
      pp_cons c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args

let pp_destructor pp_destr pp_k pp_ka fmt destr =
  match destr with
  | Call (x,a) -> fprintf fmt ".call(%a)%a"
                    (pp_print_list ~pp_sep:pp_comma_sep pp_k) x pp_ka a
  | Proj (i,n,a) -> fprintf fmt ".proj(%n/%n)%a" i n pp_ka a
  | Closure a -> fprintf fmt ".closure()%a" pp_ka a
  | NegCons (c, args, a) ->
    fprintf fmt ".%a(@[<hov 2>%a@])%a"
      pp_destr c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args
      pp_ka a
