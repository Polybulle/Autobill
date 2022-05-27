open Types
open Constructors
open Cst
open Format

let pp_comma_sep fmt () =
  fprintf fmt ",@ "

let pp_var fmt v = pp_print_string fmt v

let pp_tyvar fmt v = pp_print_string fmt v

let pp_consvar fmt v = pp_print_string fmt v

let pp_destrvar fmt v = pp_print_string fmt v

let pp_sort fmt (so : Cst.sort) = pp_print_string fmt (
    match so with
    | Base Positive -> "+"
    | Base Negative -> "-"
    | Base (PVar ()) -> "~"
    | SortVar _ -> .
  )

let rec pp_typ fmt t =
  match t with
  | TPos t -> fprintf fmt "+%a" pp_typ t
  | TNeg t -> fprintf fmt "-%a" pp_typ t
  | TVar v -> pp_print_string fmt v.node
  | TBox b -> fprintf fmt "@[<hov 2>(%s@ %a)@]" (string_of_box_kind b.kind) pp_typ b.node
  | TInternal n -> fprintf fmt "<%s>" n
  | TCons c -> match c.node with
    | Unit -> pp_print_string fmt "unit"
    | Zero -> pp_print_string fmt "zero"
    | Top -> pp_print_string fmt "top"
    | Bottom -> pp_print_string fmt "bottom"
    | Prod (a,b) -> fprintf fmt "@[<hov 2>(prod@ %a@ %a)@]" pp_typ a pp_typ b
    | Sum (a,b) -> fprintf fmt "@[<hov 2>(sum@ %a@ %a)@]" pp_typ a pp_typ b
    | Fun (a,b) -> fprintf fmt "@[<hov 2>(fun@ %a@ %a)@]" pp_typ a pp_typ b
    | Choice (a,b) -> fprintf fmt "@[<hov 2>(choice@ %a@ %a)@]" pp_typ a pp_typ b
    | Cons (c,args) ->
      fprintf fmt "@[<hov 2>(%a@ %a)@]"
        pp_tyvar c
        (pp_print_list ~pp_sep:pp_print_space pp_typ) args

let pp_constructor pp_kvar pp_k fmt cons =
  match cons with
  | Unit -> pp_print_string fmt "unit()"
  | Pair (a,b) -> fprintf fmt "@[<hov 2>pair(@,%a,@ %a)@]" pp_k a pp_k b
  | Left x -> fprintf fmt "left(%a)" pp_k x
  | Right x -> fprintf fmt "right(%a)" pp_k x
  | PosCons (c, args) ->
    fprintf fmt ":%a(@[<hov 2>%a@])"
      pp_kvar c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args

let pp_destructor pp_kvar pp_k pp_ka fmt destr =
  match destr with
  | Call (x,a) -> fprintf fmt ".call(%a)%a" pp_k x pp_ka a
  | Yes a -> fprintf fmt ".yes()%a" pp_ka a
  | No a -> fprintf fmt ".no()%a" pp_ka a
  | NegCons (c, args, a) ->
    fprintf fmt ".%a(@[<hov 2>%a@])%a"
      pp_kvar c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args
      pp_ka a


let pp_custom_binding ~prefix ~suffix fmt pp_v v pp_t t =
  match t with
  | None -> pp_v fmt v
  | Some t ->
  fprintf fmt "@[<hov 2>%s%a@ : %a%s@]" prefix pp_v v pp_t t suffix

let pp_bind fmt (v, t) =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_var v pp_typ t

let pp_bind_paren fmt (v, t) =
  pp_custom_binding ~prefix:"(" ~suffix:")" fmt pp_var v pp_typ t

let pp_bind_copatt fmt t =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_print_string ".ret()" pp_typ t

let pp_bind_bindcc fmt t =
  match t with
  | None -> ()
  | Some t -> fprintf fmt " @[<hov 2>(ret()@ : %a)@]" pp_typ t

let pp_bind_typ_paren fmt (t, so) =
  pp_custom_binding  ~prefix:"(" ~suffix:")" fmt pp_tyvar t pp_sort so


let pp_pattern fmt p =
  pp_constructor pp_consvar pp_bind fmt p

let pp_copattern fmt p =
  pp_destructor pp_destrvar pp_bind pp_bind_copatt fmt p

let pp_pol_annot fmt pol =
  match pol with
  | Positive -> fprintf fmt "+"
  | Negative -> fprintf fmt "-"
  | PVar () -> ()


let rec pp_value fmt = function

  | Var v -> pp_var fmt v.node

  | Bindcc {po; typ; cmd; _} ->
    fprintf fmt "@[<hov 2>bind/cc%a%a ->@ %a@]"
      pp_pol_annot po
      pp_bind_bindcc typ
      pp_cmd cmd

  | Box {kind; typ; cmd; _} ->
    fprintf fmt "@[<hov 2>box(%a)%a ->@ %a@]"
      pp_print_string (string_of_box_kind kind)
      pp_bind_bindcc typ
      pp_cmd cmd

  | Cons c -> pp_constructor pp_consvar pp_value fmt c.node

  | Destr patts ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>case this%a ->@ %a@]" pp_copattern p pp_cmd c in
    fprintf fmt "@[<v 0>@[<v 2>match@,%a@]@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) patts.node

  | Macro_box {kind; valu; _} ->
    fprintf fmt "box(%s) %a" (string_of_box_kind kind) pp_value valu

  | Macro_fun {arg; valu; typ; _} ->
    fprintf fmt "fun %a -> %a" pp_bind (arg, typ) pp_value valu

and pp_stack fmt s =
  pp_print_string fmt "this";
  pp_open_hbox fmt ();
  pp_stack_trail fmt s;
  pp_close_box fmt ()

and pp_stack_trail fmt s =
  match s with

  | Ret _ -> fprintf fmt "@,.ret()"

  | CoBind {po; name; typ; cmd; _} ->
    fprintf fmt "@,@[<hov 2>.bind%a %a ->@ %a@]"
      pp_pol_annot po
      pp_bind_paren (name, typ)
      pp_cmd cmd

  | CoBox {kind; stk; _} ->
    fprintf fmt "@,.unbox(%a)%a"
      pp_print_string (string_of_box_kind kind)
      pp_stack_trail stk

  | CoDestr d ->
    pp_print_cut fmt ();
    pp_destructor pp_destrvar pp_value pp_stack_trail fmt d.node

  | CoCons patts ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>case %a ->@ %a@]" pp_pattern p pp_cmd c in
    fprintf fmt "@[<v 0>@[<v 2>.match@,%a@]@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) patts.node

and pp_cmd fmt cmd =
   let pp_annot fmt typ =
      match typ with
      | None -> ()
      | Some typ -> fprintf fmt "@ : %a" pp_typ typ in

   match cmd with

   | Macro_term {name; valu; typ; cmd; _} ->
     fprintf fmt "@[<hov 2>term %a%a =@ %a in@ %a@]"
       pp_var name
       pp_annot typ
       pp_value valu
       pp_cmd cmd

   | Macro_env {stk; typ; cmd; _} ->
     fprintf fmt "@[<hov 2>env %a%a in@ %a@]"
       pp_stack stk
       pp_annot typ
       pp_cmd cmd

   | Macro_match_val {patt; valu; cmd; _} ->
     fprintf fmt "@[<hov 2>match %a =@ %a in@ %a@]"
       pp_pattern patt
       pp_value valu
       pp_cmd cmd

   | Macro_match_stk {copatt; cmd; _} ->
     fprintf fmt "@[<hov 2>match env this%a in@ %a@]"
       pp_copattern copatt
       pp_cmd cmd

   | Command {po; valu; typ; stk; _} ->
     match valu with
     | Var _ | Cons _ ->
       fprintf fmt "%a%a"
         pp_value valu
         pp_stack_trail stk
     | _ ->
       fprintf fmt "@[<v 0>step%a@;<1 2>%a%a@ into@;<1 2>%a@ end@]"
         pp_pol_annot po
         pp_value valu
         pp_annot typ
         pp_stack stk


let pp_typ_lhs fmt (name, args, sort) =
  if args = [] then
    pp_tyvar fmt name
  else
    fprintf fmt "@[<hov 2>%a %a@]"
      pp_tyvar name
      (pp_print_list ~pp_sep:pp_print_space pp_bind_typ_paren) args;
  match sort with
  | None -> ()
  | Some sort -> fprintf fmt " : %a" pp_sort sort

let pp_data_decl_item fmt item =
  match item with
  | PosCons (name, args) ->
    fprintf fmt "@[<hov 2>case :%a(%a)@]"
      pp_consvar name
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) args
  | _ -> failwith ("Some constructor has a reserved name in a data definition")

let pp_codata_decl_item fmt item =
  match item with
  | NegCons (name, args, cont) ->
    fprintf fmt "@[<hov 2>case this.%a(%a).ret() : %a@]"
      pp_destrvar name
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) args
      pp_typ cont
  | _ -> failwith ("Some constructor has a reserved name in a data definition")


let pp_prog_item fmt item =
  pp_open_box fmt 2;
  begin
    match item with
    | Type_declaration {name; sort; _} ->
      fprintf fmt "decl type %a : %a" pp_tyvar name pp_sort sort

    | Type_definition {name; sort; args; content; _} ->
      fprintf fmt "type %a =@ %a" pp_typ_lhs (name, args, sort) pp_typ content

    | Data_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>data %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content

    | Codata_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>codata %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

    | Term_definition {name; typ; content; _} ->
      fprintf fmt "term %a =@ %a"
        pp_bind (name, typ)
        pp_value content

    | Env_definition {name; typ; content; _} ->
      fprintf fmt "env %a =@ %a"
        pp_bind (name, typ)
        pp_stack content

    | Cmd_definition {name; content; typ; _} ->
      fprintf fmt "cmd %a =@ %a"
        pp_bind (name, typ)
        pp_cmd content
  end;
  pp_close_box fmt ()

let pp_program fmt prog =
  let pp_double_cut fmt () = pp_print_cut fmt (); pp_print_cut fmt () in
  pp_open_vbox fmt 0;
  pp_print_list ~pp_sep:pp_double_cut pp_prog_item fmt prog;
  pp_close_box fmt ()
