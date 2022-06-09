open Util
open Vars
open Types
open Intern_common
open Ast
open InternAst

let export_ast env (Definition item) =

  let rec export_upol ?loc = function
    | Litt p -> p
    | Loc (loc', x) -> export_upol ~loc:loc' x
    | Redirect var ->
      try export_upol ?loc (Intern_pol_inference.get env var)
      with Not_found -> fail_ambiguous_polarity (Option.value loc ~default:dummy_pos)

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
    | Bindcc {bind=(pol2,typ);pol=pol1;cmd} ->
      let pol1 = export_upol ~loc pol1 in
      let pol2 = export_upol ~loc pol2 in
      if pol1 <> pol2 then fail_polarity_mismatch loc loc;
      FullAst.Bindcc {bind = typ; pol = pol1; cmd = export_cmd cmd}
    | Box {kind; bind=(_,typ); cmd} ->
      FullAst.Box {kind; bind=typ; cmd = export_cmd cmd}
    | Cons cons -> FullAst.Cons (export_cons cons)
    | Destr copatts ->
      FullAst.Destr
        (List.map (fun (copatt, cmd) -> (export_copatt copatt, export_cmd cmd))
           copatts)

  and export_stk loc = function
    | Ret -> FullAst.Ret
    | CoBind {bind; pol; cmd} ->
      FullAst.CoBind {bind; pol = export_upol ~loc pol; cmd = export_cmd cmd}
    | CoBox {kind; stk} -> FullAst.CoBox {kind; stk = export_meta_stk stk}
    | CoDestr destr -> FullAst.CoDestr (export_destr destr)
    | CoCons patts ->
      FullAst.CoCons
        (List.map (fun (patt, cmd) -> (export_patt patt, export_cmd cmd))
        patts)

  and export_cons cons = match cons with
    | Unit -> Unit
    | Left x -> Left (export_meta_val x)
    | Right x -> Right (export_meta_val x)
    | ShiftPos x -> ShiftPos (export_meta_val x)
    | Pair (a,b) -> Pair (export_meta_val a, export_meta_val b)
    | PosCons (cons, args) -> PosCons (cons, List.map export_meta_val args)

  and export_destr destr = match destr with
    | Call (x,a) -> Call (export_meta_val x, export_meta_stk a)
    | Yes a -> Yes (export_meta_stk a)
    | No a -> No (export_meta_stk a)
    | ShiftNeg a -> ShiftNeg (export_meta_stk a)
    | NegCons (cons, args, cont) ->
      NegCons (cons, List.map export_meta_val args, export_meta_stk cont)

  and export_patt patt = patt

  and export_copatt copatt = copatt

  in

  let content = match item.content with
    | InternAst.Value_definition valu -> FullAst.Value_definition (export_meta_val valu)
    | Stack_definition stk -> Stack_definition (export_meta_stk stk)
    | Command_definition cmd -> Command_definition (export_cmd cmd) in

  FullAst.Definition {
    name = item.name;
    typ = item.typ;
    pol = export_upol item.pol;
    cont = item.cont;
    loc = item.loc;
    content = content
  }
