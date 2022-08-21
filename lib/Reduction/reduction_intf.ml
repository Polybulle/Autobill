open Vars
open Ast
open FullAst
open HeadNormalForm
open NormalForm

let interpret_prog (prelude, prog_items) =

  let do_once declared env prog_item = match prog_item with

    | Value_declaration {name; _} ->
      (Var.Env.add name () declared, env, prog_item)

    | Value_definition def ->
      let cmd = FullAst.Command
          {pol = def.pol;
           loc = def.loc;
           mid_typ = def.typ;
           final_typ = def.typ;
           valu = def.content;
           stk = S.ret;
          } in
      let Command cmd = (cmd_nf {
          curr = cmd;
          cont = [];
          declared;
          env;
        }).curr in
      (declared,
       Var.Env.add def.name cmd.valu env,
       Value_definition {def with content = cmd.valu})

    | Command_execution exec ->
      let cmd = (cmd_nf {
          curr = exec.content;
          cont = [];
          declared;
          env;
        }).curr in
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
