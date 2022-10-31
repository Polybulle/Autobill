open Types
open Constructors
open Cst
open Format

let pp_comma_sep fmt () =
  fprintf fmt ",@ "

let pp_var fmt v = pp_print_string fmt v

let pp_covar fmt a = pp_print_string fmt a

let pp_tyvar fmt v = pp_print_string fmt v

let pp_consvar fmt v = pp_print_string fmt v

let pp_destrvar fmt v = pp_print_string fmt v

let pp_sort fmt so =
    match so with
    | Base Positive -> pp_print_string fmt "+"
    | Base Negative -> pp_print_string fmt "-"

let rec pp_typ fmt t =
  match t with
  | TPos t -> fprintf fmt "+%a" pp_typ t
  | TNeg t -> fprintf fmt "-%a" pp_typ t
  | TVar v -> pp_print_string fmt v.node
  | TFix t -> fprintf fmt "@[<hov 2>(fix@ %a)@])" pp_typ t
  | TBox b -> fprintf fmt "@[<hov 2>(%s@ %a)@]" (string_of_box_kind b.kind) pp_typ b.node
  | TInternal n -> fprintf fmt "%s" n
  | TCons c -> match c.node with
    | Unit -> pp_print_string fmt "unit"
    | Zero -> pp_print_string fmt "zero"
    | Top -> pp_print_string fmt "top"
    | Bottom -> pp_print_string fmt "bottom"
    | Thunk a -> fprintf fmt "@[<hov 2>(thunk@ %a)@]" pp_typ a
    | Closure a -> fprintf fmt "@[<hov 2>(closure@ %a)@]" pp_typ a
    | Prod bs -> fprintf fmt "@[<hov 2>(prod@ %a)@]"
                   (pp_print_list ~pp_sep:pp_print_space pp_typ) bs
    | Sum bs -> fprintf fmt "@[<hov 2>(sum@ %a)@]"
                  (pp_print_list ~pp_sep:pp_print_space pp_typ) bs
    | Fun (a,b) -> fprintf fmt "@[<hov 2>(fun@ %a@ -> %a)@]"
                     (pp_print_list ~pp_sep:pp_print_space pp_typ) a pp_typ b
    | Choice bs -> fprintf fmt "@[<hov 2>(choice@ %a)@]"
                     (pp_print_list ~pp_sep:pp_print_space pp_typ) bs
    | Cons (c,args) ->
      if List.length args = 0 then
        pp_tyvar fmt c
      else
        fprintf fmt "@[<hov 2>(%a@ %a)@]"
          pp_tyvar c
          (pp_print_list ~pp_sep:pp_print_space pp_typ) args

let pp_constructor pp_kvar pp_k fmt cons =
  match cons with
  | Unit -> pp_print_string fmt "unit()"
  | Tupple vs ->
    fprintf fmt "@[<hov 2>tupple(%a)@]" (pp_print_list ~pp_sep:pp_comma_sep pp_k) vs
  | Inj (i,n,x) -> fprintf fmt "inj(%n/%n, %a)" i n pp_k x
  | Thunk x -> fprintf fmt "thunk(%a)" pp_k x
  | PosCons (c, args) ->
    fprintf fmt "%a(@[<hov 2>%a@])"
      pp_kvar c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args

let pp_destructor pp_kvar pp_k pp_ka fmt destr =
  match destr with
  | Call (x,a) -> fprintf fmt ".call(%a)%a"
                    (pp_print_list ~pp_sep:pp_comma_sep pp_k) x pp_ka a
  | Proj (i,n,a) -> fprintf fmt ".proj(%n/%n)%a" i n pp_ka a
  | Closure a -> fprintf fmt ".closure()%a" pp_ka a
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

let pp_bind_cc fmt bind =
  fprintf fmt ".ret(%a)"
    (fun fmt (a,t) -> pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_covar a pp_typ t)
    bind

let pp_bind_typ_paren fmt (t, so) =
  pp_custom_binding  ~prefix:"(" ~suffix:")" fmt pp_tyvar t pp_sort (Some so)


let pp_pattern fmt p =
  pp_constructor pp_consvar pp_bind fmt p

let pp_copattern fmt p =
  pp_destructor pp_destrvar pp_bind pp_bind_cc fmt p

let pp_pol_annot fmt pol =
  match pol with
  | Some Positive -> fprintf fmt "+"
  | Some Negative -> fprintf fmt "-"
  | _ -> ()

let rec pp_value fmt = function

  | Var v -> pp_var fmt v.node

  | CoTop _ -> fprintf fmt "GOT_TOP"

  | Bindcc {pol; bind; cmd; _} ->
    fprintf fmt "@[<hov 2>bind/cc%a %a ->@ %a@]"
      pp_pol_annot pol
      (fun fmt (a,t) -> pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_covar a pp_typ t) bind
      pp_cmd cmd

  | Box {kind; bind; cmd; _} ->
    fprintf fmt "@[<hov 2>box(%a)%a ->@ %a@]"
      pp_print_string (string_of_box_kind kind)
      (fun fmt (a,t) -> pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_covar a pp_typ t) bind
      pp_cmd cmd

  | Cons c -> pp_constructor pp_consvar pp_value fmt c.node

  | Destr patts ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>case this%a ->@ %a@]" pp_copattern p pp_cmd c in
    fprintf fmt "@[<v 0>@[<v 2>match@,%a@]@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) patts.node

  | Macro_box {kind; valu; _} ->
    fprintf fmt "box(%s, %a)" (string_of_box_kind kind) pp_value valu

  | Macro_fun {arg; valu; typ; _} ->
    fprintf fmt "fun %a -> %a" pp_bind_paren (arg, typ) pp_value valu

  | Fix {self; cont; cmd; _} ->
    fprintf fmt "@[<hov 2>match this.fix%a%a->@ %a@]"
      pp_bind_paren self
      pp_bind_cc cont
      pp_cmd cmd

  | Pack {cons; typs; content; _} ->
    fprintf fmt "@[<hov 2>:%a[%a](%a)@]"
      pp_consvar cons
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) typs
      pp_value content

  | Spec {destr; bind; spec_vars; cmd; _} ->
    fprintf fmt "@[spec this.%a[%a]()%a ->@ %a@]"
      pp_destrvar destr
      (pp_print_list ~pp_sep:pp_comma_sep pp_bind_typ_paren) spec_vars
      pp_bind_cc bind
      pp_cmd cmd


and pp_stack fmt s =
  pp_print_string fmt "this";
  pp_open_hbox fmt ();
  pp_stack_trail fmt s;
  pp_close_box fmt ()

and pp_stack_trail fmt s =
  match s with

  | Ret {var; _} -> fprintf fmt "@,.ret(%a)" pp_covar var

  | CoZero _ -> fprintf fmt "@,.GOT_ZERO()"

  | CoBind {pol; bind; cmd; _} ->
    fprintf fmt "@,@[<hov 2>.bind%a %a ->@ %a@]"
      pp_pol_annot pol
      pp_bind_paren bind
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

  | CoFix {stk; _} ->
    fprintf fmt "@,.fix()%a"
      pp_stack_trail stk

  | CoSpec {destr; typs; content; _} ->
    fprintf fmt "@,.%a[%a]()%a"
      pp_destrvar destr
      (pp_print_list ~pp_sep:pp_comma_sep pp_typ) typs
      pp_stack_trail content

  | CoPack {cons; bind; pack_vars; cmd; _} ->
    fprintf fmt "@[match %a[%a](%a) ->@ %a@]"
      pp_consvar cons
      (pp_print_list ~pp_sep:pp_comma_sep pp_bind_typ_paren) pack_vars
      pp_bind bind
      pp_cmd cmd

and pp_cmd fmt cmd =
   let pp_annot fmt typ =
      match typ with
      | None -> ()
      | Some typ -> fprintf fmt "@ : %a" pp_typ typ in

   match cmd with

   | Macro_term {name; valu; typ; cmd; _} ->
     fprintf fmt "@[<hov 2>val %a%a =@ %a in@ %a@]"
       pp_var name
       pp_annot typ
       pp_value valu
       pp_cmd cmd

   | Macro_env {stk; typ; cmd; name; _} ->
     fprintf fmt "@[<hov 2>stk %a = %a%a in@ %a@]"
       pp_covar name
       pp_stack stk
       pp_annot typ
       pp_cmd cmd

   | Macro_match_val {patt; valu; cmd; _} ->
     fprintf fmt "@[<hov 2>match %a =@ %a in@ %a@]"
       pp_pattern patt
       pp_value valu
       pp_cmd cmd

   | Macro_match_stk {copatt; cmd; stk; _} ->
     fprintf fmt "@[<hov 2>match stk this%a = %a in@ %a@]"
       pp_copattern copatt
       pp_stack stk
       pp_cmd cmd

   | Command {pol; valu; typ; stk; _} ->
     match valu with
     | Var _ | Cons _ ->
       fprintf fmt "%a%a"
         pp_value valu
         pp_stack_trail stk
     | _ ->
       fprintf fmt "@[<v 0>cmd%a%a val =@;<1 2>%a@ stk =@;<1 2>%a@ end@]"
         pp_pol_annot pol
         pp_annot typ
         pp_value valu
         pp_stack stk


let pp_typ_lhs fmt (name, args, sort) =
  if args = [] then
    pp_tyvar fmt name
  else
    fprintf fmt "@[<hov 2>%a %a@]"
      pp_tyvar name
      (pp_print_list ~pp_sep:pp_print_space pp_bind_typ_paren) args;
  match sort with
  | Some sort ->  fprintf fmt " : %a" pp_sort sort
  | None -> ()

let pp_data_decl_item fmt item =
  match item with
  | PosCons (name, args) ->
    fprintf fmt "@[<hov 2>case %a(%a)@]"
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
      fprintf fmt "type %a =@ %a" pp_typ_lhs (name, args, Some sort) pp_typ content

    | Data_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>data %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content

    | Codata_definition {name; args; content; _} ->
      fprintf fmt "@[<v 2>comput %a =@,%a@]"
        pp_typ_lhs (name, args, None)
        (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

    | Pack_definition {name; args; cons; private_typs; arg_typs; _} ->
      fprintf fmt "pack %a =@ %a[%a](%a)"
        pp_typ_lhs (name, args, None)
        pp_consvar cons
        (pp_print_list ~pp_sep:pp_comma_sep pp_bind_typ_paren) private_typs
        (pp_print_list ~pp_sep:pp_comma_sep pp_typ) arg_typs

     | Spec_definition {name; args; destr; private_typs; arg_typs; ret_typ; _} ->
      fprintf fmt "spec %a =@ this.%a[%a](%a).ret() : %a"
        pp_typ_lhs (name, args, None)
        pp_destrvar destr
        (pp_print_list ~pp_sep:pp_comma_sep pp_bind_typ_paren) private_typs
        (pp_print_list ~pp_sep:pp_comma_sep pp_typ) arg_typs
        pp_typ ret_typ

    | Term_definition {name; typ; content; _} ->
      fprintf fmt "val %a =@ %a"
        pp_bind (name, typ)
        pp_value content

    | Term_declaration {name; typ; _} ->
      fprintf fmt "decl val %a"
        pp_bind (name, Some typ)

    | Cmd_execution {name; content; typ; cont; _} ->
      match name with
      | Some name ->
        fprintf fmt "cmd %a ret %a =@ %a"
          pp_var name
          (fun fmt (a,t) -> pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_covar a pp_typ t) (cont,typ)
          pp_cmd content
      | _ ->
        fprintf fmt "cmd ret %a do@ %a"
          pp_covar cont
          pp_cmd content
  end;
  pp_close_box fmt ()

let pp_program fmt prog =
  pp_open_vbox fmt 0;
  pp_print_list ~pp_sep:pp_print_cut pp_prog_item fmt prog;
  pp_close_box fmt ()
