open Util
open Intern_common
open Types
open Ast
open InternAst

let export_ast env item =

  let prelude = ref env.prelude in

  let rec export_upol ?loc = function
    | Litt p -> p
    | Loc (loc', x) -> export_upol ~loc:loc' x
    | Redirect var ->
      try export_upol ?loc (SortInfer.get env var)
      with Not_found -> fail_ambiguous_polarity (Option.value loc ~default:dummy_pos)

  and export_bind pol (var, typ) =
    let pol = export_upol pol in
    let typ = export_typ (sort_base pol) typ in
    prelude := {!prelude with vars = VarEnv.add var typ !prelude.vars};
    (var, typ)

  and export_cobind (pol, typ) =
    let pol = export_upol pol in
    let typ = export_typ (sort_base pol) typ in
    typ

  and export_typ sort typ = match typ with
    | TVar {node;_} | TInternal node ->
      prelude := {!prelude with sorts = TyVarEnv.add node sort !prelude.sorts};
      typ
    | TPos typ -> export_typ sort_postype typ
    | TNeg typ -> export_typ sort_negtype typ
    | TBox {kind;node;loc} -> TBox {kind; loc; node = export_typ sort_postype node}
    | TCons {node;loc} ->
      let node = match node with
        | Unit | Zero | Top | Bottom -> node
        | ShiftPos t -> ShiftPos (export_typ sort_negtype t)
        | ShiftNeg t -> ShiftNeg (export_typ sort_postype t)
        | Prod xs -> Prod (List.map (export_typ sort_postype) xs)
        | Sum xs -> Sum (List.map (export_typ sort_postype) xs)
        | Choice xs -> Choice (List.map (export_typ sort_negtype) xs)
        | Fun (xs,b) -> Fun (List.map (export_typ sort_postype) xs, export_typ sort_negtype b)
        | Cons (cons, args) ->
          let sort = TyConsEnv.find cons env.tycons_sort in
          let rec aux a b = match a,b with
            | arg::args, Dep (arg_so, args_so) ->
              (export_typ arg_so arg) :: aux args args_so
            | _ -> [] in
          let args = aux args sort in
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
      loc = cmd.loc
    }

  and export_val loc = function
    | Var v -> FullAst.Var v
    | CoTop -> FullAst.CoTop
    | Bindcc {bind=(upol2,_) as bind ;pol=upol1; cmd} ->
      let pol1 = export_upol ~loc upol1 in
      let pol2 = export_upol ~loc upol2 in
      if pol1 <> pol2 then fail_polarity_mismatch upol1 upol2 loc loc;
      let bind = export_cobind bind in
      FullAst.Bindcc {bind = bind; pol = pol1; cmd = export_cmd cmd}
    | Box {kind; bind; cmd} ->
      let bind = export_cobind bind in
      FullAst.Box {kind; bind; cmd = export_cmd cmd}
    | Cons cons -> FullAst.Cons (export_cons cons)
    | Destr copatts ->
      FullAst.Destr
        (List.map (fun (copatt, cmd) -> (export_copatt copatt, export_cmd cmd))
           copatts)

  and export_stk loc = function
    | Ret -> FullAst.Ret
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

  and export_cons cons = match cons with
    | Unit -> Unit
    | Inj (i,n,x) -> Inj (i,n,export_meta_val x)
    | ShiftPos x -> ShiftPos (export_meta_val x)
    | Tupple xs -> Tupple (List.map export_meta_val xs)
    | PosCons (cons, args) -> PosCons (cons, List.map export_meta_val args)

  and export_destr destr = match destr with
    | Call (xs,a) -> Call (List.map export_meta_val xs, export_meta_stk a)
    | Proj (i,n,a) -> Proj (i,n,export_meta_stk a)
    | ShiftNeg a -> ShiftNeg (export_meta_stk a)
    | NegCons (cons, args, cont) ->
      NegCons (cons, List.map export_meta_val args, export_meta_stk cont)

  and export_patt = function
    | Unit -> Unit
    | Inj(i,n,x) -> Inj (i,n,export_bind (Litt positive) x)
    | ShiftPos x -> ShiftPos (export_bind (Litt positive) x)
    | Tupple xs -> Tupple (List.map (export_bind (Litt positive)) xs)
    | PosCons (cons, args) -> PosCons (cons, List.map (export_bind (Litt positive)) args)

  and export_copatt = function
    | Call (xs,a) -> Call (List.map (export_bind (Litt positive)) xs,
                           export_cobind  (Litt negative, a))
    | Proj (i,n,a) -> Proj (i,n,export_cobind (Litt negative, a))
    | ShiftNeg a -> (ShiftNeg (export_cobind (Litt positive, a)))
    | NegCons (cons, args, cont) ->
      NegCons (cons,
               List.map (export_bind (Litt positive)) args,
               export_cobind (Litt negative, cont))
  in

  let def = match item with
    | InternAst.Value_declaration {name; typ; pol; loc} ->
      FullAst.Value_declaration {name; typ; pol = export_upol ~loc pol; loc}
    | InternAst.Value_definition {name; typ; pol; content; loc} ->
      FullAst.Value_definition {name; typ; pol = export_upol ~loc pol;
                                content = (export_meta_val content); loc}
    | Command_execution {name; typ; pol; content; cont; loc} ->
      Command_execution {name; typ; pol = export_upol ~loc pol;
                         content = export_cmd content; cont; loc} in

  let env = {env with prelude = !prelude} in
  def, env
