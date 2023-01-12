open Misc
open Vars
open Format

type polarity = Positive | Negative

type 'var idx_sort =
  | IdxVar of 'var
  | IdxArrow of 'var idx_sort list * 'var idx_sort

type 'var mono_sort =  Mono of 'var idx_sort list * polarity

type 'var poly_sort =
  | Poly of 'var mono_sort list * 'var mono_sort
  | Idx of 'var idx_sort

let positive = Positive
let negative = Negative
let idx_var i = IdxVar i
let idx_arr idxs idx_ret = if idxs = [] then idx_ret else IdxArrow (idxs, idx_ret)
let mono_base p = Mono([], p)
let mono_arrow arg ret = if arg = [] then Mono ([], ret) else Mono (arg, ret)
let mono_postype = Mono ([], Positive)
let mono_negtype = Mono ([], Negative)
let sort_idx i = Idx i

let pp_polarity fmt = function
  | Positive -> pp_print_string fmt "+"
  | Negative -> pp_print_string fmt "-"

let pp_idx_sort pp_idx_var fmt x =
  let rec go fmt = function
    | IdxVar i -> pp_idx_var fmt i
    | IdxArrow (s,t) ->
      fprintf fmt "idx %a -> %a"
        (pp_tupple_or_item go) s
       go t
  in go fmt x

let pp_mono_sort pp_idx_var fmt  (Mono (idxs, base)) =
  if idxs = [] then
    pp_polarity fmt base
  else
    fprintf fmt "type %a -> %a"
      (pp_tupple_or_item (pp_idx_sort pp_idx_var)) idxs
      pp_polarity base

let pp_poly_sort pp_idx_var fmt = function
  | Idx i -> pp_idx_sort pp_idx_var fmt i
  | (Poly (args, ret)) ->
    if args = [] then
      pp_mono_sort pp_idx_var fmt ret
    else
      fprintf fmt "type %a -> %a"
        (pp_tupple_or_item (pp_mono_sort pp_idx_var)) args
        (pp_mono_sort pp_idx_var) ret

type box_kind = Linear | Affine | Exponential

let linear = Linear
let affine = Affine
let exp = Exponential

let string_of_box_kind = function
  | Linear -> "lin"
  | Affine -> "aff"
  | Exponential -> "exp"



type var_multiplicity =
  | MulZero
  | MulOne
  | MulMany

let pp_mult fmt = function
  | MulZero -> pp_print_string fmt "0"
  | MulOne -> pp_print_string fmt "1"
  | MulMany -> pp_print_string fmt "*"


let mult a b = match a,b with
  | MulZero, x | x, MulZero -> x
  | MulMany, _ | _, MulMany -> MulMany
  | MulOne, MulOne -> MulMany

let update a b = match b with
  | None -> Some a
  | Some b -> Some (mult a b)


let type_cons_names =
  ["unit"; "zero"; "top"; "bottom"; "prod"; "sum"; "fun"; "choice"; "thunk"; "closure"]

type 'tycons type_cons =
  | Unit
  | Zero
  | Top
  | Bottom
  | Thunk
  | Closure
  | Fix
  | Box of box_kind
  | Prod of int
  | Sum of int
  | Fun of int
  | Choice of int
  | Cons of 'tycons

let pp_type_cons kvar fmt cons =
  match cons with
  | Unit -> pp_print_string fmt "unit"
  | Zero -> pp_print_string fmt "zero"
  | Top -> pp_print_string fmt "top"
  | Bottom -> pp_print_string fmt "bottom"
  | Thunk -> pp_print_string fmt "thunk"
  | Closure -> pp_print_string fmt "closure"
  | Box k -> pp_print_string fmt (string_of_box_kind k)
  | Fix -> pp_print_string fmt "fix"
  | Prod _ -> pp_print_string fmt "prod"
  | Sum _ -> pp_print_string fmt "sum"
  | Fun _ -> pp_print_string fmt "fun"
  | Choice _ -> pp_print_string fmt "choice"
  | Cons var -> kvar fmt var


let sort_of_type_cons aux tcons  =
  let (-->) xs x = Poly (List.map (fun x -> Mono ([], x)) xs, Mono ([], x)) in
  let pos = Positive and neg = Negative in
  match tcons with
  | Unit | Zero -> [] --> pos
  | Top | Bottom -> [] --> neg
  | Prod n | Sum n -> (List.init n (fun _ -> pos) ) --> pos
  | Choice n -> (List.init n (fun _ -> neg)) --> neg
  | Fun n -> (neg :: List.init n (fun _ -> pos)) --> neg
  | Thunk -> [pos]-->neg
  | Closure -> [neg]-->pos
  | Box _ -> [neg]-->pos
  | Cons c -> aux c
  | Fix -> [neg]-->neg

type ('tycons, 'var) pre_typ =
  | TCons of {node : 'tycons type_cons;
              loc : position}
  | TApp of {tfun : ('tycons, 'var) pre_typ;
             args : ('tycons, 'var) pre_typ list;
             loc : position}
  | TVar of {node : 'var;
             loc : position}
  | TPos of ('tycons, 'var) pre_typ
  | TNeg of ('tycons, 'var) pre_typ
  | TInternal of 'var

type typ = (TyConsVar.t, TyVar.t) pre_typ


let pos t = TPos t
let neg t = TNeg t
let tvar ?loc:(loc = dummy_pos) node = TVar {node; loc}
let posvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let negvar ?loc:(loc = dummy_pos) v = tvar ~loc:loc v
let cons ?loc:(loc = dummy_pos) node = TCons {node; loc}
let app ?loc:(loc = dummy_pos) tfun args = TApp {tfun; args; loc}

let unit_t = cons Unit
let zero = cons Zero
let top = cons Top
let bottom = cons Bottom
let prod ts = app (cons (Prod (List.length ts))) ts
let sum ts = app (cons (Sum (List.length ts))) ts
let func ts = app (cons (Fun (List.length ts - 1))) ts
let choice ts = app (cons (Choice (List.length ts))) ts
let typecons v args = app (cons (Cons v)) args
let thunk_t t = app (cons Thunk) [t]
let closure_t t = app (cons Closure) [t]
let fix_t t = app (cons Fix) [t]
let boxed kind t = app (cons (Box kind)) [t]

let pp_tyvar fmt v = pp_print_string fmt (TyVar.to_string v)

let pp_typ pp_tycons pp_tyvar fmt t =
  let rec go fmt t = match t with
  | TPos t -> fprintf fmt "+%a" go t
  | TNeg t -> fprintf fmt "-%a" go t
  | TVar v -> pp_tyvar fmt v.node
  | TInternal v -> pp_tyvar fmt v
  | TCons {node;_} -> pp_type_cons pp_tycons fmt node
  | TApp {tfun;args;_} ->
    match tfun with
    | TCons {node=Prod _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ * ") go)  args
    | TCons {node=Sum _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ + ") go) args
    | TCons {node=Choice _;_} ->
      fprintf fmt "(%a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@ & ") go) args
    | TCons{node=Fun _;_} when args != [] ->
      let[@warning "-partial-match"] ret::args = args in
      fprintf fmt "(fun (%a) -> %a)"
        (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt "@,, ") go) args
        go ret
    | _ ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]"
        go tfun
        (pp_print_list ~pp_sep:pp_print_space go) args

  in go fmt t
