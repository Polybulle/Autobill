open Vars
open Ast
open FullAst
open HeadNormalForm
open NormalForm

let visit_prog
    run_command
    (prelude, prog_items) =

  let do_once declared env prog_item = match prog_item with

    | Value_declaration {name; _} ->
      (Var.Env.add name () declared, env, prog_item)

    | Value_definition def ->
      let a = CoVar.fresh () in
      let cmd = FullAst.Command
          {pol = def.pol;
           loc = def.loc;
           mid_typ = def.typ;
           final_typ = def.typ;
           valu = def.content;
           stk = S.ret a;
          } in
      let Command cmd = run_command declared env cmd in
      let valu = MetaVal {
          node = Bindcc {bind = (a, cmd.final_typ); pol = def.pol; cmd = Command cmd};
          val_typ = cmd.final_typ;
          loc = def.loc} in
      (declared,
       Var.Env.add def.name valu env,
       Value_definition {def with content = valu})

    | Command_execution exec ->
      let cmd = run_command declared env exec.content in
      (declared,
       env,
       Command_execution {exec with content = cmd}) in

  let rec loop declared env prog_items =
    match prog_items with
    | [] -> (declared, env, [])
    | h :: t ->
      let declared, env, h = do_once declared env h in
      let declared, env, t = loop declared env t in
      declared, env, h::t in

  let declared = Var.Env.empty in
  let env = Var.Env.empty in
  let _, _, prog_items = loop declared env prog_items in
  (prelude, prog_items)

let normal_form_visitor
    ?reduce_fixpoints:(fixpoints=false)
    ?reduce_shareing:(share=false)
    declared env cmd =
  let prog = cmd_nf {declared;
                     env; cont = CoVar.Env.empty;
                     curr = cmd;
                     reduce_sharing = share;
                     reduce_fixpoints = fixpoints} in
  prog.curr

let head_normal_form_visitor
    ?reduce_fixpoints:(fixpoints=false)
    ?reduce_sharing:(share=false)
    declared env cmd =
  let prog = head_normal_form {declared;
                     env; cont = CoVar.Env.empty;
                     curr = cmd;
                     reduce_sharing = share;
                     reduce_fixpoints = fixpoints} in
  prog.curr

let simplify_untyped_prog prog =
  visit_prog normal_form_visitor prog

let interpret_prog prog =
  visit_prog (head_normal_form_visitor ~reduce_fixpoints:true ~reduce_sharing:true) prog
