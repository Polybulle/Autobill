open Format

let cons_names = ["true"; "false"; "int"; "unit"; "pair"; "left"; "right"; "thunk"]

  type 'var constructor_tag =
    | Unit
    | Closure
    | Bool of bool
    | Int of int
    | Tupple of int
    | Inj of int * int
    | PosCons of 'var

  type ('var, 'idx, 'arg) constructor = Raw_Cons of {
    tag : 'var constructor_tag;
    idxs : 'idx list;
    args : 'arg list
  }

  let cons tag idxs args = Raw_Cons {tag; idxs;  args}
  let unit = cons Unit [] []
  let tuple xs = cons (Tupple (List.length xs)) [] xs
  let inj i n a = cons (Inj (i,n)) [] [a]
  let closure a = cons Closure [] [a]

  let destr_names = ["call"; "yes"; "no"; "closure"]

  type 'var destructor_tag =
    | Call of int
    | Proj of int * int
    | Thunk
    | NegCons of 'var

  type ('var, 'idx, 'arg, 'cont) destructor = Raw_Destr of {
    tag : 'var destructor_tag;
    idxs : 'idx list;
    args : 'arg list;
    cont : 'cont
  }

  let destr tag idxs  args cont = Raw_Destr {tag; idxs; args; cont}
  let call xs a = destr (Call (List.length xs)) [] xs a
  let proj i n a = destr (Proj (i,n)) [] [] a
  let thunk a = destr Thunk [] [] a


  let pp_comma_sep fmt () = fprintf fmt ",@ "

type ('var, 'idx, 'arg, 'cont) pp_cons_aux = {
  pp_var : Format.formatter -> 'var -> unit;
  pp_idx : Format.formatter -> 'idx -> unit;
  pp_arg : Format.formatter -> 'arg -> unit;
  pp_cont : Format.formatter -> 'cont -> unit;
}

let pp_idxs aux fmt idxs =
  if idxs <> [] then
    fprintf fmt "<%a>"(pp_print_list ~pp_sep:pp_comma_sep aux.pp_idx) idxs

let pp_args aux fmt args =
  fprintf fmt "(@,%a)" (pp_print_list ~pp_sep:pp_comma_sep aux.pp_arg) args

(* TODO unsafe *)
let pp_constructor aux fmt (Raw_Cons {tag; idxs; args}) = begin
  pp_open_hovbox fmt 2;
  begin match tag with
    | Bool b -> fprintf fmt "%a()" pp_print_bool b
    | Int n -> fprintf fmt "int(%n)" n
    | Unit -> pp_print_string fmt "unit()"
    | Tupple _ -> fprintf fmt "tuple%a" (pp_args aux) args
    | Inj (i,n) -> fprintf fmt "inj(%n, %n, %a)" i n aux.pp_arg (List.hd args)
    | Closure -> fprintf fmt "closure(%a)" aux.pp_arg (List.hd args)
    | PosCons c -> aux.pp_var fmt c; pp_idxs aux fmt idxs; pp_args aux fmt args
  end;
  pp_close_box fmt ()
end


let pp_destructor aux fmt (Raw_Destr {tag; idxs; args; cont}) = begin
  pp_print_string fmt ".";
  pp_open_hovbox fmt 2;
  begin match tag with
    | Call _ -> fprintf fmt "call%a" (pp_args aux) args
    | Proj (i,n) -> fprintf fmt "proj(%n, %n)" i n
    | Thunk -> fprintf fmt "thunk()"
    | NegCons c -> aux.pp_var fmt c; pp_idxs aux fmt idxs; pp_args aux fmt args;
  end ;
  pp_close_box fmt ();
  aux.pp_cont fmt cont;
end
