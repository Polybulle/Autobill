open Util
open Vars
open Intern_common
open Types
open Ast
open InternAst
open SortInfer

let export_ast env item =

  let prelude = ref env.prelude in

  let rec export_usort ?loc = function
    | Litt p -> p
    | Loc (loc', x) ->
      let loc = if loc' <> dummy_pos then Some loc' else loc in
      export_usort ?loc x
    | Redirect var ->
      try export_usort ?loc (SortInfer.get env var)
      with Not_found -> fail_ambiguous_sort (Option.value loc ~default:dummy_pos)

  and export_upol ?loc uso =
    match export_usort ?loc uso with
    | Base p -> p
    | Arrow _
    | Index _ -> fail_ambiguous_sort (Option.value loc ~default:dummy_pos)

  and export_bind pol (var, typ) =
    let pol = export_upol pol in
    let typ = export_typ (Base pol) typ in
    prelude := {!prelude with vars = Var.Env.add var typ !prelude.vars};
    (var, typ)

  and export_cobind pol (covar, typ) =
    let pol = export_upol pol in
    let typ = export_typ (Base pol) typ in
    prelude := {!prelude with covars = CoVar.Env.add covar typ !prelude.covars};
    (covar, typ)

  and export_typ (sort : sort) typ = match typ with
    | TVar {node;_} | TInternal node ->
      prelude := {!prelude with sorts = TyVar.Env.add node sort !prelude.sorts};
      typ
    | TPos typ -> export_typ sort_postype typ
    | TNeg typ -> export_typ sort_negtype typ
    | TFix t -> TFix (export_typ sort_negtype t)
    | TBox {kind;node;loc} -> TBox {kind; loc; node = export_typ sort_postype node}
    | TCons {node;loc} ->
      let node = match node with
        | Unit | Zero | Top | Bottom -> node
        | Thunk t -> Thunk (export_typ sort_negtype t)
        | Closure t -> Closure (export_typ sort_postype t)
        | Prod xs -> Prod (List.map (export_typ sort_postype) xs)
        | Sum xs -> Sum (List.map (export_typ sort_postype) xs)
        | Choice xs -> Choice (List.map (export_typ sort_negtype) xs)
        | Fun (xs,b) -> Fun (List.map (export_typ sort_postype) xs, export_typ sort_negtype b)
        | Cons (cons, args) ->
          let args_so, _ = unmk_arrow (TyConsVar.Env.find cons env.tycons_sort) in
          let args = List.map2 export_typ args_so args in
          Cons (cons, args)
      in
      TCons {node;loc}

  and export_meta_val (MetaVal v) = FullAst.MetaVal {
      node = export_val v.loc v.node;
      val_typ = v.val_typ;
      loc = v.loc
    }

  and export_meta_stk (MetaStack stk) = FullAst.MetaStack {
      node = export_stk stk.loc stk.node;
      cont_typ = stk.cont_typ;
      final_typ = stk.final_typ;
      loc = stk.loc
    }

  and export_cmd (Command cmd) = FullAst.Command {
      valu = export_meta_val cmd.valu;
      pol = export_upol ~loc:cmd.loc cmd.pol;
      stk = export_meta_stk cmd.stk;
      mid_typ = cmd.mid_typ;
      final_typ = cmd.final_typ;
      loc = cmd.loc
    }

  and export_val loc = function
    | Var v -> FullAst.Var v
    | CoTop -> FullAst.CoTop
    | Bindcc {bind; pol; cmd} ->
      let upol = export_upol ~loc pol in
      let bind = export_cobind pol bind in
      FullAst.Bindcc {bind = bind; pol = upol; cmd = export_cmd cmd}
    | Box {kind; bind; cmd} ->
      let bind = export_cobind neg_uso bind in
      FullAst.Box {kind; bind; cmd = export_cmd cmd}
    | Cons cons -> FullAst.Cons (export_cons cons)
    | Destr copatts ->
      FullAst.Destr
        (List.map (fun (copatt, cmd) -> (export_copatt copatt, export_cmd cmd))
           copatts)
    | Fix {self; cmd; cont} ->
      let self = export_bind pos_uso self in
      let cmd = export_cmd cmd in
      let cont = export_cobind neg_uso cont in
      Fix {self; cmd; cont}
    | Pack (cons, typs, v) ->
      let Consdef {private_typs; _} = def_of_cons !prelude cons in
      let typs = List.map2 (fun t (_,so) -> export_typ so t) typs private_typs in
      let v = export_meta_val v in
      Pack (cons, typs, v)
    | Spec { destr; bind; spec_vars; cmd } ->
      let go (x, so) =
        prelude := {!prelude with
                    sorts = TyVar.Env.add x so !prelude.sorts} in
      List.iter go spec_vars;
      let bind = export_cobind neg_uso bind in
      let cmd = export_cmd cmd in
      Spec {destr; bind; spec_vars; cmd}


  and export_stk loc = function
    | Ret a -> FullAst.Ret a
    | CoZero -> FullAst.CoZero
    | CoBind {bind; pol; cmd} ->
      let bind = export_bind pol bind in
      let pol = export_upol ~loc pol in
      FullAst.CoBind {bind; pol; cmd = export_cmd cmd}
    | CoBox {kind; stk} -> FullAst.CoBox {kind; stk = export_meta_stk stk}
    | CoDestr destr -> FullAst.CoDestr (export_destr destr)
    | CoCons patts ->
      FullAst.CoCons
        (List.map (fun (patt, cmd) -> (export_patt patt, export_cmd cmd))
        patts)
    | CoFix stk -> CoFix (export_meta_stk stk)
    | CoSpec (destr, typs, stk) ->
      let Destrdef {private_typs; _} = def_of_destr !prelude destr in
      let typs = List.map2 (fun t (_,so) -> export_typ so t) typs private_typs in
      let stk = export_meta_stk stk in
      CoSpec (destr, typs, stk)
    | CoPack { cons; bind; pack_vars; cmd } ->
      let go (x, so) =
        prelude := {!prelude with
                    sorts = TyVar.Env.add x so !prelude.sorts} in
      List.iter go pack_vars;
      let bind = export_bind neg_uso bind in
      let cmd = export_cmd cmd in
      CoPack {cons; bind; pack_vars; cmd}



  and export_cons cons = match cons with
    | Unit -> Unit
    | Inj (i,n,x) -> Inj (i,n,export_meta_val x)
    | Thunk x -> Thunk (export_meta_val x)
    | Tupple xs -> Tupple (List.map export_meta_val xs)
    | PosCons (cons, args) -> PosCons (cons, List.map export_meta_val args)

  and export_destr destr = match destr with
    | Call (xs,a) -> Call (List.map export_meta_val xs, export_meta_stk a)
    | Proj (i,n,a) -> Proj (i,n,export_meta_stk a)
    | Closure a -> Closure (export_meta_stk a)
    | NegCons (cons, args, cont) ->
      NegCons (cons, List.map export_meta_val args, export_meta_stk cont)

  and export_patt = function
    | Unit -> Unit
    | Inj(i,n,x) -> Inj (i,n,export_bind pos_uso x)
    | Thunk x -> Thunk (export_bind pos_uso x)
    | Tupple xs -> Tupple (List.map (export_bind pos_uso) xs)
    | PosCons (cons, args) -> PosCons (cons, List.map (export_bind pos_uso) args)

  and export_copatt = function
    | Call (xs,a) -> Call (List.map (export_bind pos_uso) xs,
                           export_cobind neg_uso a)
    | Proj (i,n,a) -> Proj (i,n,export_cobind neg_uso a)
    | Closure a -> (Closure (export_cobind pos_uso a))
    | NegCons (cons, args, cont) ->
      NegCons (cons,
               List.map (export_bind pos_uso) args,
               export_cobind neg_uso cont)
  in

  let def = match item with
    | InternAst.Value_declaration {name; typ; pol; loc} ->
      FullAst.Value_declaration {name; typ; pol = export_upol ~loc pol; loc}
    | InternAst.Value_definition {name; typ; pol; content; loc} ->
      FullAst.Value_definition {name; typ; pol = export_upol ~loc pol;
                                content = (export_meta_val content); loc}
    | Command_execution {name; pol; content; cont; conttyp; loc} ->
      Command_execution {pol = export_upol ~loc pol;
                         content = export_cmd content;
                         name; conttyp; cont; loc} in

  let env = {env with prelude = !prelude} in
  def, env
