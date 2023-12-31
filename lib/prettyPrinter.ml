open Vars
open Types
open Constructors
open Ast
open Format

open FullAst

let pp_comma_sep fmt () =
  fprintf fmt ",@ "

let pp_var fmt v = pp_print_string fmt (Var.to_string v)

let pp_tyvar fmt v = pp_print_string fmt (TyVar.to_string v)

let pp_consvar fmt v = pp_print_string fmt (ConsVar.to_string v)

let pp_destrvar fmt v = pp_print_string fmt (DestrVar.to_string v)

let pp_tyconsvar fmt v = pp_print_string fmt (TyConsVar.to_string v)

let pp_defvar fmt v = pp_print_string fmt (DefVar.to_string v)

let pp_pol fmt = function
  | Positive -> pp_print_string fmt "+"
  | Negative -> pp_print_string fmt "-"

let rec pp_sort fmt sort =
  begin match sort with
  | Base p -> pp_pol fmt p
  | Dep (a,b) -> fprintf fmt "@[<hov 2>(%a) ->@ %a@]" pp_sort a pp_sort b
  end

let rec pp_returned_sort fmt = function
  | Dep (_, x) -> pp_returned_sort fmt x
  | Base _ as x -> pp_sort fmt x

let rec pp_typ fmt t =
  match t with
  | TPos t -> fprintf fmt "+%a" pp_typ t
  | TNeg t -> fprintf fmt "-%a" pp_typ t
  | TVar v -> pp_tyvar fmt v.node
  | TInternal v -> fprintf fmt "<%a>" pp_tyvar v
  | TBox b -> fprintf fmt "@[<hov 2>(%s@ %a)@]" (string_of_box_kind b.kind) pp_typ b.node
  | TCons c -> match c.node with
    | Unit -> pp_print_string fmt "unit"
    | Zero -> pp_print_string fmt "zero"
    | Top -> pp_print_string fmt "top"
    | Bottom -> pp_print_string fmt "bottom"
    | ShiftPos a -> fprintf fmt "@[<hov 2>(shift+@ %a)@]" pp_typ a
    | ShiftNeg a -> fprintf fmt "@[<hov 2>(shift-@ %a)@]" pp_typ a
    | Prod (a,b) -> fprintf fmt "@[<hov 2>(prod@ %a@ %a)@]" pp_typ a pp_typ b
    | Sum (a,b) -> fprintf fmt "@[<hov 2>(sum@ %a@ %a)@]" pp_typ a pp_typ b
    | Fun (a,b) -> fprintf fmt "@[<hov 2>(fun@ %a@ %a)@]" pp_typ a pp_typ b
    | Choice (a,b) -> fprintf fmt "@[<hov 2>(choice@ %a@ %a)@]" pp_typ a pp_typ b
    | Cons (c,args) ->
      if List.length args = 0 then
        pp_tyconsvar fmt c
      else
        fprintf fmt "@[<hov 2>(%a@ %a)@]"
          pp_tyconsvar c
          (pp_print_list ~pp_sep:pp_print_space pp_typ) args

let pp_constructor pp_k fmt cons =
  match cons with
  | Unit -> pp_print_string fmt "unit()"
  | Pair (a,b) -> fprintf fmt "@[<hov 2>pair(@,%a,@ %a)@]" pp_k a pp_k b
  | Left x -> fprintf fmt "left(%a)" pp_k x
  | Right x -> fprintf fmt "right(%a)" pp_k x
  | ShiftPos x -> fprintf fmt "shift+(%a)" pp_k x
  | PosCons (c, args) ->
    fprintf fmt ":%a(@[<hov 2>%a@])"
      pp_consvar c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args

let pp_destructor pp_k pp_ka fmt destr =
  match destr with
  | Call (x,a) -> fprintf fmt ".call(%a)%a" pp_k x pp_ka a
  | Yes a -> fprintf fmt ".yes()%a" pp_ka a
  | No a -> fprintf fmt ".no()%a" pp_ka a
  | ShiftNeg a -> fprintf fmt ".shift-()%a" pp_ka a
  | NegCons (c, args, a) ->
    fprintf fmt ".%a(@[<hov 2>%a@])%a"
      pp_destrvar c
      (pp_print_list ~pp_sep:pp_comma_sep pp_k) args
      pp_ka a


let pp_custom_binding ~prefix ~suffix fmt pp_v v pp_t t =
  fprintf fmt "@[<hov 2>%s%a@ : %a%s@]" prefix pp_v v pp_t t suffix

let pp_bind fmt (v, t) =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_var v pp_typ t

let pp_bind_def fmt (v, t) =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_defvar v pp_typ t

let pp_bind_def_with_cont fmt (v, t, cont) =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_defvar v pp_typ t;
  fprintf fmt "@ returns %a" pp_typ cont

let pp_bind_paren fmt (v, t) =
  pp_custom_binding ~prefix:"(" ~suffix:")" fmt pp_var v pp_typ t

let pp_bind_copatt fmt t =
  pp_custom_binding ~prefix:"" ~suffix:"" fmt pp_print_string ".ret()" pp_typ t

let pp_bind_bindcc fmt t =
  fprintf fmt " @[<hov 2>(ret()@ : %a)@]" pp_typ t

let pp_bind_typ_paren fmt (t, so) =
  pp_custom_binding  ~prefix:"(" ~suffix:")" fmt pp_tyvar t pp_sort so


let pp_pattern fmt p =
  pp_constructor pp_bind fmt p

let pp_copattern fmt p =
  pp_destructor pp_bind pp_bind_copatt fmt p

let pp_pol_annot fmt (pol : FullAst.polarity) = pp_pol fmt pol

let pp_meta_pol_annot fmt (pol : FullAst.polarity) =
  fprintf fmt "<%a>" pp_pol pol

let rec pp_value fmt = function
  | MetaVal {node; _} -> pp_pre_value fmt node


and pp_pre_value fmt = function

  | Var v -> pp_var fmt v

  | Bindcc {pol; bind=typ; cmd; _} ->
    fprintf fmt "@[<hov 2>bind/cc%a%a ->@ %a@]"
      pp_pol_annot pol
      pp_bind_bindcc typ
      pp_cmd cmd

  | Box {kind; bind=typ; cmd; _} ->
    fprintf fmt "@[<hov 2>box(%a)%a ->@ %a@]"
      pp_print_string (string_of_box_kind kind)
      pp_bind_bindcc typ
      pp_cmd cmd

  | Cons c -> pp_constructor pp_value fmt c

  | Destr patts ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>case this%a ->@ %a@]" pp_copattern p pp_cmd c in
    fprintf fmt "@[<v 0>@[<v 2>match@,%a@]@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) patts

and pp_stack fmt (MetaStack s) = pp_pre_stack fmt s.node

and pp_pre_stack fmt s =
  pp_print_string fmt "this";
  pp_open_hbox fmt ();
  pp_pre_stack_trail fmt s;
  pp_close_box fmt ()

and pp_stack_trail fmt (MetaStack s) = pp_pre_stack_trail fmt s.node

and pp_pre_stack_trail fmt s =
  match s with

  | Ret -> fprintf fmt "@,.ret()"

  | CoBind {pol; bind = (name, typ); cmd; _} ->
    fprintf fmt "@,@[<hov 2>.bind%a %a ->@ %a@]"
      pp_pol_annot pol
      pp_bind_paren (name, typ)
      pp_cmd cmd

  | CoBox {kind; stk; _} ->
    fprintf fmt "@,.unbox(%a)%a"
      pp_print_string (string_of_box_kind kind)
      pp_stack_trail stk

  | CoDestr d ->
    pp_print_cut fmt ();
    pp_destructor pp_value pp_stack_trail fmt d

  | CoCons patts ->
    let pp_case fmt (p,c) =
      fprintf fmt "@[<hov 2>case %a ->@ %a@]" pp_pattern p pp_cmd c in
    fprintf fmt "@[<v 0>@[<v 2>.match@,%a@]@,end@]"
      (pp_print_list ~pp_sep:pp_print_space pp_case) patts

and pp_cmd fmt cmd =
  let Command {pol; valu; mid_typ; stk; _} = cmd in
  let pp_annot fmt typ =
    fprintf fmt "@ : %a" pp_typ typ in
  let MetaVal {node = pre_valu; _} = valu in
  match pre_valu with
  | Var _ | Cons _ ->
    fprintf fmt "%a%a"
      pp_value valu
      pp_stack_trail stk
  | _ ->
    fprintf fmt "@[<v 0>step%a@;<1 2>%a%a@ into@;<1 2>%a@ end@]"
      pp_pol_annot pol
      pp_value valu
      pp_annot mid_typ
      pp_stack stk

let pp_typ_lhs ?sort () fmt (name, args) =
  if args = [] then
    pp_tyconsvar fmt name
  else
    fprintf fmt "@[<hov 2>%a %a@]"
      pp_tyconsvar name
      (pp_print_list ~pp_sep:pp_print_space pp_bind_typ_paren) args;
  match sort with
  | Some sort -> fprintf fmt " : %a" pp_returned_sort sort
  | None -> ()

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


let pp_tycons_def fmt (name, def) =
  let {full_sort; ret_sort; args; content; _} = def in
  match content with
  | Declared ->
    fprintf fmt "decl type %a : %a"
      pp_tyconsvar name
      pp_sort full_sort
  | Defined content ->
    fprintf fmt "@[<hov 2>type %a =@ %a@]"
      (pp_typ_lhs ~sort:ret_sort ()) (name, args)
      pp_typ content
  | Data content ->
    fprintf fmt "@[<v 2>data %a =@,%a@]"
      (pp_typ_lhs ()) (name, args)
      (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content

  | Codata content ->
    fprintf fmt "@[<v 2>codata %a =@,%a@]"
      (pp_typ_lhs ()) (name, args)
      (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

let pp_quantified_cons_args pp_k fmt args =
  if List.length args = 0 then
    ()
  else
    fprintf fmt "forall %a. " (pp_print_list ~pp_sep:pp_print_space pp_k) args

let pp_cons_def fmt (cons, def) =
  let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
  let Consdef {args; content; resulting_type} = def in
  fprintf fmt "@[<hov 4>/* constructor \"%a\" is@ %a%a : %a*/@]"
    pp_consvar cons
    (pp_quantified_cons_args pp_aux) args
    (pp_constructor pp_typ) content
    pp_typ resulting_type

let pp_destr_def fmt (cons, def) =
  let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
  let pp_tail fmt typ = fprintf fmt ".ret(%a)" pp_typ typ in
  let Destrdef {args; content; resulting_type} = def in
  fprintf fmt "@[<hov 4>/* destructor \"%a\" is %a%a : %a*/@]"
    pp_destrvar cons
    (pp_quantified_cons_args pp_aux) args
    (pp_destructor pp_typ pp_tail) content
    pp_typ resulting_type

let pp_var_typ fmt (var, typ) =
  fprintf fmt "@[<hv 2>/* var %a : %a */@]" pp_var var pp_typ typ

let pp_tyvar_sort fmt (var, so) =
  fprintf fmt "@[<hv 2>/* tyvar %a : %a */@]" pp_tyvar var pp_sort so

  let pp_definition fmt def =
    let Definition {name; typ; cont; content; pol; _} = def in
    pp_open_box fmt 2;
    begin
      match content with
      | Value_definition content ->
        fprintf fmt "term%a %a =@ %a"
          pp_meta_pol_annot pol
          pp_bind_def (name, typ)
          pp_value content

      | Stack_definition content ->
        fprintf fmt "env%a %a =@ %a"
          pp_meta_pol_annot pol
          pp_bind_def_with_cont (name, typ, cont)
          pp_stack content

      | Command_definition content ->
        fprintf fmt "cmd%a %a =@ %a"
          pp_meta_pol_annot pol
          pp_bind_def_with_cont (name, typ, cont)
          pp_cmd content
    end;
    pp_close_box fmt ()

let pp_program fmt (prelude, prog) =
  let is_empty = function [] -> true | _ -> false in
  pp_open_vbox fmt 0;

  let {tycons; cons; destr; vars; sorts} = prelude in

  let typs = TyConsEnv.bindings tycons in
  pp_print_list ~pp_sep:pp_print_cut pp_tycons_def fmt typs;
  if not (is_empty typs) then pp_print_cut fmt ();

  let conses = ConsEnv.bindings cons in
  pp_print_list ~pp_sep:pp_print_cut pp_cons_def fmt conses;
  if not (is_empty conses) then pp_print_cut fmt ();

  let destrs = DestrEnv.bindings destr in
  pp_print_list ~pp_sep:pp_print_cut pp_destr_def fmt destrs;
  if not (is_empty destrs) then pp_print_cut fmt ();

  let vars = VarEnv.bindings vars in
  pp_print_list ~pp_sep:pp_print_cut pp_var_typ fmt vars;
  if not (is_empty vars) then pp_print_cut fmt ();

  let sorts = TyVarEnv.bindings sorts in
  pp_print_list ~pp_sep:pp_print_cut pp_tyvar_sort fmt sorts;
  if not (is_empty vars) then pp_print_cut fmt ();

  
  pp_print_list ~pp_sep:pp_print_cut pp_definition fmt prog;

  pp_close_box fmt ()
