open Vars
open Types
open Constructors
open Ast
open Format
open Prelude
open FirstOrder.FullFOL

let pp_comma_sep fmt () =
  fprintf fmt ",@ "

let pp_custom_binding ~prefix ~suffix pp_v pp_t fmt (v, t) =
  fprintf fmt "@[<hov 2>%s%a@ : %a%s@]" prefix pp_v v pp_t t suffix

let pp_sort ?debug = Types.pp_sort (SortVar.to_string ?debug)

let pp_typ ?debug fmt t = begin
  pp_open_hbox fmt ();
    Types.pp_typ (TyConsVar.pp ?debug) (TyVar.pp ?debug) fmt t;
  pp_close_box fmt ();
end

let pp_bind_def ?debug =
  pp_custom_binding ~prefix:"" ~suffix:"" (Var.pp ?debug) (pp_typ ?debug)

let pp_bind_def_with_cont ?debug =
  pp_custom_binding ~prefix:"" ~suffix:"" (Var.pp ?debug) (pp_typ ?debug)


module type PP_Params = sig

  type polarity

  type val_bind

  type cont_bind

  type type_bind

  type cmd_annot

  type toplevel_bind_annot

  val pp_pol : formatter -> polarity -> unit

  val pp_bind : formatter -> val_bind -> unit

  val pp_type_bind : formatter -> type_bind -> unit

  val pp_bind_paren : formatter -> val_bind -> unit

  val pp_bind_cc : formatter -> cont_bind -> unit

  val pp_cmd_annot : formatter -> cmd_annot -> unit

  val pp_toplevel_bind_annot : ?debug:bool -> formatter -> toplevel_bind_annot -> unit

  val print_debug_names : bool

end

module PP_FullAst = struct

  type polarity = FullAstParams.polarity

  type val_bind = FullAstParams.val_bind

  type type_bind = FullAstParams.type_bind

  type cont_bind = FullAstParams.cont_bind

  type cmd_annot = FullAst.typ

  type toplevel_bind_annot = FullAstParams.toplevel_bind_annot

  let pp_pol fmt = function
    | Positive -> pp_print_string fmt "+"
    | Negative -> pp_print_string fmt "-"

  let pp_bind =
    pp_custom_binding ~prefix:"" ~suffix:"" (Var.pp ~debug:true) (pp_typ ~debug:true)

  let pp_type_bind =
    pp_custom_binding ~prefix:"" ~suffix:"" (TyVar.pp ~debug:true) (pp_sort ~debug:true)

  let pp_bind_cc =
      pp_custom_binding ~prefix:"" ~suffix:"" (CoVar.pp ~debug:true) (pp_typ ~debug:true)

  let pp_bind_paren =
  pp_custom_binding ~prefix:"(" ~suffix:")" (Var.pp ~debug:true) (pp_typ ~debug:true)

  let pp_toplevel_bind_annot = pp_typ

  let print_debug_names = true

  let pp_cmd_annot  fmt typ = fprintf fmt " : %a" (pp_typ ~debug:true) typ

end

module PP_Params_NoTypes = struct

  type polarity = FullAstParams.polarity

  type val_bind = FullAstParams.val_bind

  type type_bind = FullAstParams.type_bind

  type cont_bind = FullAstParams.cont_bind

  type cmd_annot = FullAst.typ

  type toplevel_bind_annot = FullAstParams.toplevel_bind_annot

  let pp_pol fmt = function
    | Positive -> pp_print_string fmt "+"
    | Negative -> pp_print_string fmt "-"

  let pp_bind fmt (x,_) = (Var.pp ~debug:false) fmt x

  let pp_type_bind =
    pp_custom_binding ~prefix:"" ~suffix:"" (TyVar.pp ~debug:false) (pp_sort ~debug:false)

  let pp_bind_cc fmt (a,_) = CoVar.pp ~debug:false fmt a

  let pp_bind_paren fmt (x,_) = Var.pp ~debug:false fmt x

  let pp_toplevel_bind_annot ?debug:_ fmt _ = pp_print_string fmt "_"

  let print_debug_names = false

  let pp_cmd_annot _ _ = ()

end


module Make
    (PP_Params : PP_Params)
    (AstParams : AstParams with type polarity = PP_Params.polarity
                            and type val_bind = PP_Params.val_bind
                            and type cont_bind = PP_Params.cont_bind
                            and type type_bind = PP_Params.type_bind
                            and type cmd_annot = PP_Params.cmd_annot
                            and type toplevel_bind_annot = PP_Params.toplevel_bind_annot)
= struct

  include PP_Params
  include Ast(AstParams)

  let debug = print_debug_names

  let pp_typ = pp_typ ~debug

  let pp_sort = pp_sort ~debug

  let pp_toplevel_bind_annot = pp_toplevel_bind_annot ~debug

  let pp_bind_cc_ret fmt bind =
    fprintf fmt ".ret(%a)" pp_bind_cc bind

  let pp_pol_annot fmt pol =
    fprintf fmt "%a" pp_pol pol

  let rec pp_value fmt = function
    | MetaVal {node; _} -> pp_pre_value fmt node

  and  pp_cons_val_aux = {
    pp_var = ConsVar.pp ~debug;
    pp_idx = pp_typ;
    pp_arg = pp_value;
    pp_cont = pp_stack_trail
  }

  and  pp_cons_patt_aux = {
    pp_var = ConsVar.pp ~debug;
    pp_idx = pp_type_bind;
    pp_arg = pp_bind;
    pp_cont = pp_bind_cc_ret
  }

  and pp_pattern fmt p = begin
    pp_open_hbox fmt ();
    pp_constructor pp_cons_patt_aux fmt p;
    pp_close_box fmt ();
  end

  and pp_copattern fmt p = begin
    pp_open_hbox fmt ();
    pp_destructor pp_cons_patt_aux fmt p;
    pp_close_box fmt ();
  end

  and pp_pre_value fmt = function

    | Var v -> Var.pp ~debug fmt v

    | CoTop -> fprintf fmt "GOT_TOP"

    | Bindcc {pol; bind; cmd; _} ->
      fprintf fmt "@[<v 0>bind/cc%a %a ->@,%a@]"
        pp_pol_annot pol
        pp_bind_cc bind
        pp_cmd cmd

    | Box {kind; bind; cmd; _} ->
      fprintf fmt "@[<v 2>box(%a) %a ->@,%a@]"
        pp_print_string (string_of_box_kind kind)
        pp_bind_cc bind
        pp_cmd cmd

    | Cons c -> pp_constructor pp_cons_val_aux fmt c

    | Destr {cases; default; _} ->
      let pp_case ~prefix fmt (p,c) =
        fprintf fmt "@[<v 0>%sthis%a ->@;<1 2>%a@]" prefix pp_copattern p pp_cmd c in
      let pp_default fmt = function
        | None -> ()
        | Some (a,c) ->
          fprintf fmt "@ @[<hov 2>| %a ->@ %a@]" pp_bind_cc a pp_cmd c in
      begin match cases,default with
        | [p,c], None ->
          fprintf fmt "@[<v 0>match this%a ->@,%a@]"
            pp_copattern p pp_cmd c
        | _ ->
          fprintf fmt "@[<v 2>match@,%a%a@]@,end"
            (pp_print_list ~pp_sep:pp_print_space (pp_case ~prefix:"| ")) cases
            pp_default default
      end

    | Fix {bind; stk} ->
      fprintf fmt "@[<v 2>match this.fix()%a -> self.%a@]"
        pp_bind_cc_ret bind
        pp_stack stk

  and pp_stack fmt (MetaStack s) =
    pp_pre_stack fmt s.node

  and pp_pre_stack fmt s =
    fprintf fmt "this%a" pp_pre_stack_trail s

  and pp_stack_trail fmt (MetaStack s) =
    pp_pre_stack_trail fmt s.node

  and pp_pre_stack_trail fmt s =
    match s with

    | Ret a -> fprintf fmt "@,.ret(%a)@]" (CoVar.pp ~debug) a

    | CoZero -> fprintf fmt "@,.GOT_ZERO()@]"

    | CoBind {pol; bind; cmd; _} ->
      (* fprintf fmt "@,.bind%a %a ->@,@[<v 2>%a@]@]@]" *)
      fprintf fmt ".bind%a %a ->@]@;<1 2>@[%a@]"
        pp_pol_annot pol
        pp_bind bind
        pp_cmd cmd

    | CoBox {kind; stk; _} ->
      fprintf fmt "@,.unbox(%a)%a"
        pp_print_string (string_of_box_kind kind)
        pp_stack_trail stk

    | CoDestr d -> fprintf fmt "@,%a" (pp_destructor pp_cons_val_aux) d

    | CoCons {default; cases; _} ->
      let pp_case ~prefix fmt (p,c) =
        fprintf fmt "@[<v 2>%s%a ->@ %a@]" prefix pp_pattern p pp_cmd c in
      let pp_default fmt = function
        | None -> ()
        | Some (x,c) ->
          fprintf fmt "@ @[<v 2>| %a ->@ %a@]" pp_bind x pp_cmd c in
      begin match cases, default with
        | [p,c], None ->
          fprintf fmt ".match@] %a ->@;<1 0>%a"
            pp_pattern p pp_cmd c
        | _ ->
          fprintf fmt ".match@]@,%a%a@,end"
            (pp_print_list ~pp_sep:pp_print_cut (pp_case ~prefix:"| ")) cases
            pp_default default
      end

    | CoFix stk ->
      fprintf fmt "@,.fix()%a" pp_stack_trail stk

  and pp_cmd fmt cmd =
    let Command {pol; node; _} = cmd in
    match node with

    | Interact {valu; stk; mid_typ} ->
      let MetaVal {node = pre_valu; _} = valu in
      begin match pre_valu with
        (* | Var _ | Cons _ -> *)
        (*   fprintf fmt "@[<v 0>@[%a%a@]" *)
        (*     pp_value valu *)
        (*     pp_stack_trail stk *)
        | _ ->
          fprintf fmt "@[<v 0>cmd%a%a val =@;<1 2>@[%a@]@,stk =@;<1 2>@[%a@]"
            pp_pol_annot pol
            pp_cmd_annot mid_typ
            pp_value valu
            pp_stack stk
      end

    | Trace {dump; comment; cmd} ->
      fprintf fmt "@[<v 0>trace \"%a\" %a@,in %a@]"
        (pp_print_option pp_print_string) comment
        (pp_print_option (fun fmt v -> fprintf fmt "spy = %a@," pp_value v)) dump
        pp_cmd cmd

    | Struct {valu; binds; cmd} ->
      fprintf fmt "@[<v 0>copy %a@,as @[(%a)@]@,in %a@]"
        pp_value valu
        (pp_print_list ~pp_sep:pp_comma_sep pp_bind) binds
        pp_cmd cmd

    | Pack {name; cmd; stk} ->
      fprintf fmt "@[<v 0>pack %a = %a@;<1 2>@[<v 2>%a@]@]"
        (CoVar.pp ~debug) name
        pp_stack stk
        pp_cmd cmd
    | Spec {name; cmd; valu} ->
      fprintf fmt "@[<v 0>spec %a = %a@;<1 2>@[<v 2>%a@]@]"
        (Var.pp ~debug) name
        pp_value valu
        pp_cmd cmd


  let pp_type_bind_def fmt (t,so) =
    fprintf fmt "%a : %a" (TyVar.pp ~debug) t pp_sort so

  let pp_type_bind_def_paren fmt bind = fprintf fmt "(%a)" pp_type_bind_def bind

  let pp_typ_lhs ?sort () fmt (name, args) =
    if args = [] then
      TyConsVar.pp ~debug fmt name
    else
      fprintf fmt "@[<hov 2>%a %a@]"
        (TyConsVar.pp ~debug) name
        (pp_print_list ~pp_sep:pp_print_space pp_type_bind_def_paren) args;
    match sort with
    | Some sort -> fprintf fmt " : %a" pp_sort sort
    | None -> ()

  let pp_eqn fmt = function
    | Eq (a, b, _) -> fprintf fmt "%a = %a" pp_typ a pp_typ b
    | Rel (r, xs) -> fprintf fmt "%a(%a)" pp_rel r (pp_print_list ~pp_sep:pp_comma_sep pp_typ) xs

  let pp_eqns_def fmt eqns =
    if eqns != [] then
      fprintf fmt " with %a" (pp_print_list ~pp_sep:pp_comma_sep pp_eqn) eqns

  let pp_cons_def_aux = {
    pp_var = ConsVar.pp ~debug;
    pp_idx = pp_type_bind_def;
    pp_arg = pp_typ;
    pp_cont = fun fmt typ -> fprintf fmt ".ret(%a)" pp_typ typ
  }

  let pp_data_decl_item fmt (_,item,eqns) =
    fprintf fmt "@[<hov 2>| %a%a@]"
      (pp_constructor pp_cons_def_aux) item
      pp_eqns_def eqns

  let pp_codata_decl_item fmt (_,item,eqns) =
    fprintf fmt "@[<hov 2>| this%a%a@]"
      (pp_destructor pp_cons_def_aux) item
      pp_eqns_def eqns

  let pp_tycons_def fmt (name, def) =
    let {sort; args; content; _} = def in
    let sort = snd (unmk_arrow sort) in
    if not (TyConsVar.is_primitive name) then
      match content with
      | Declared ->
        fprintf fmt "decl type %a : %a@." (TyConsVar.pp ~debug) name pp_sort sort
      | Predefined ->
        fprintf fmt "/*primitive type %a : %a*/@." (TyConsVar.pp ~debug) name pp_sort sort
      | Defined content ->
        fprintf fmt "@[<hov 2>type %a =@ %a@]@."
          (pp_typ_lhs ~sort:sort ()) (name, args)
          pp_typ content
      | Data content ->
        fprintf fmt "@[<v 2>data %a =@,%a@]@."
          (pp_typ_lhs ()) (name, args)
          (pp_print_list ~pp_sep:pp_print_cut pp_data_decl_item) content
      | Codata content ->
        fprintf fmt "@[<v 2>comput %a =@,%a@]@."
          (pp_typ_lhs ()) (name, args)
          (pp_print_list ~pp_sep:pp_print_cut pp_codata_decl_item) content

  let pp_quantified_cons_args pp_k fmt args =
    if List.length args = 0 then
      ()
    else
      fprintf fmt "forall %a. " (pp_print_list ~pp_sep:pp_print_space pp_k) args

  let pp_existential_cons_args pp_k fmt args =
    if List.length args = 0 then
      ()
    else
      fprintf fmt "exists %a. " (pp_print_list ~pp_sep:pp_print_space pp_k) args

  let pp_cons_def fmt (cons, def) =
    let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
    let Consdef { typ_args; constructor; resulting_type; equations} = def in
    fprintf fmt "@[<hov 4>/* constructor \"%a\" is@ %a%a : %a%a*/@]@."
      (ConsVar.pp ~debug) cons
      (pp_quantified_cons_args pp_aux) typ_args
      (pp_constructor pp_cons_def_aux) constructor
      pp_typ resulting_type
      pp_eqns_def equations

  let pp_destr_def fmt (cons, def) =
    let pp_aux fmt (var, sort) = fprintf fmt "(%a : %a)" pp_tyvar var pp_sort sort in
    let Destrdef {destructor; typ_args; resulting_type; equations} = def in
    fprintf fmt "@[<hov 4>/* destructor \"%a\" is %a%a : %a%a*/@]@."
      (DestrVar.pp ~debug) cons
      (pp_quantified_cons_args pp_aux) typ_args
      (pp_destructor pp_cons_def_aux) destructor
      pp_typ resulting_type
      pp_eqns_def equations

  let pp_var_typ fmt (var, typ) =
    fprintf fmt "@[<hv 2>/* var %a : %a */@]" (Var.pp ~debug) var pp_typ typ

  let pp_tyvar_sort fmt (var, so) =
    fprintf fmt "@[<hv 2>/* tyvar %a : %a */@]@." pp_tyvar var pp_sort so

  let pp_sort_def fmt (so,()) =
    if not (SortVar.is_primitive so) then
      fprintf fmt "decl sort %a@." (SortVar.pp ~debug) so

  let pp_rel_def fmt (rel, args) =
    if not (RelVar.is_primitive rel) then
      let pp_sep fmt () = pp_print_string fmt " * " in
      fprintf fmt "rel %a : %a@."
        (RelVar.pp ~debug) rel
        (pp_print_list ~pp_sep pp_sort) args

  let pp_definition fmt def =
    match def with
    | Value_declaration {bind = (x,t); pol; _} ->
      if not (Var.is_primitive x) then
        fprintf fmt "@[<v 2>decl val%a %a : %a@]@."
          pp_pol_annot pol
          (Var.pp ~debug) x
          pp_toplevel_bind_annot t

    | Value_definition {bind; pol; content; _} ->
      fprintf fmt "@[<v 2>val%a %a =@ %a@]@."
        pp_pol_annot pol
        pp_bind  bind
        pp_value content

  let pp_execution fmt exec = match exec with
    | Command_execution {name; pol; content; cont; conttyp; _} ->
      fprintf fmt "@[<v 2>cmd%a %a ret %a : %a =@ %a@]@."
        pp_pol_annot pol
        (Var.pp ~debug) name
        (CoVar.pp ~debug) cont
        pp_toplevel_bind_annot conttyp
        pp_cmd content

  let pp_goal fmt (Goal {polynomial; degree; _}) =
    fprintf fmt "goal %a degree %d@." (TyConsVar.pp ~debug) polynomial degree

  let pp_program fmt ?debug:(debug=false) prog =
    let pp_print_list pp = pp_print_list ~pp_sep:(fun _ () -> ()) pp in
    pp_set_geometry ~max_indent:200 ~margin:320 fmt;
    pp_open_vbox fmt 0;

    let {sort_defs; tycons; cons; destr; sorts; relations;
         vars = _; covars = _} = !(prog.prelude) in

    let sort_defs = SortVar.Env.bindings sort_defs in
    pp_print_list pp_sort_def fmt sort_defs;

    let relations = RelVar.Env.bindings relations in
    pp_print_list pp_rel_def fmt relations;

    let typs = TyConsVar.Env.bindings tycons in
    pp_print_list pp_tycons_def fmt typs;

    if debug then begin
      let conses = ConsVar.Env.bindings cons in
      pp_print_list pp_cons_def fmt conses;

      let destrs = DestrVar.Env.bindings destr in
      pp_print_list pp_destr_def fmt destrs;

      let sorts = TyVar.Env.bindings sorts in
      pp_print_list pp_tyvar_sort fmt sorts;
    end;

    pp_print_list pp_definition fmt prog.declarations;
    pp_print_option pp_execution fmt prog.command;
    pp_print_option pp_goal fmt prog.goal;
    pp_close_box fmt ()

end

module PP = Make (PP_FullAst) (FullAstParams)

module PP_NoTypes = Make (PP_Params_NoTypes) (FullAstParams)
