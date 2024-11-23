open Vars
open Constructors
open Ast
open FullAst
open Format


let constraint_as_string prog =
  let open Elaborate.Make(struct let it = prog.prelude end) in
  let x,_ = elab_prog prog in
  let x = compress_and x in
  pp_set_geometry str_formatter ~max_indent:300 ~margin:400;
  pp_constraint str_formatter x;
  pp_print_newline str_formatter ();
  pp_print_newline str_formatter ();
  pp_subst str_formatter !_state;
  flush_str_formatter ()

let post_contraint_as_string (_, post) =
  let post = FirstOrder.FullFOL.compress_logic ~remove_loc:true post in
  pp_set_geometry str_formatter ~margin:180 ~max_indent:170;
  FirstOrder.FullFOL.pp_formula str_formatter post;
  pp_print_newline str_formatter ();
  flush_str_formatter ()

let aara_constraint_as_string (_, post) =
  let post = AaraCompress.compress_unification post in
  pp_set_geometry str_formatter ~margin:180 ~max_indent:170;
  FirstOrder.FullFOL.pp_formula str_formatter post;
  pp_print_newline str_formatter ();
  flush_str_formatter ()



let fill_out_types prog =

  let vars = ref (Var.Env.empty) in

  let covars = ref (CoVar.Env.empty) in

  let bind_var (v,t) = vars := Var.Env.add v t !vars in

  let bind_covar (a,t) = covars := CoVar.Env.add a t !covars in

  let rec goval (MetaVal v) = gopreval v.node

  and gostk (MetaStack s) = goprestk s.node

  and gocmd (Command c) = goprecmd c.loc c.node

  and goprecmd loc = function
    | Interact {valu; stk; _} -> goval valu; gostk stk
    | Trace {dump; cmd; _} -> Option.iter goval dump; gocmd cmd
    | Struct {valu; binds; cmd} ->
      goval valu;
      List.iter bind_var binds;
      gocmd cmd
    | Pack _ | Spec _ ->
      Misc.fail_invariant_break ~loc
        "Internal type-checker syntax node found during typed-IR elaboration"

  and gopreval = function
    | Var _ | CoTop -> ()
    | Bindcc {bind; cmd; _} | Box {bind; cmd; _} ->
      bind_covar bind; gocmd cmd
    | Fix {bind; stk} ->
      bind_covar bind;
      gostk stk
    | Cons (Raw_Cons cons) -> List.iter goval cons.args
    | Destr {default; cases; _} ->
      List.iter (fun (Raw_Destr patt, cmd) ->
          List.iter bind_var patt.args;
          bind_covar patt.cont;
          gocmd cmd
        ) cases;
      Option.iter (fun (a,cmd) -> bind_covar a; gocmd cmd) default


  and goprestk = function
    | Ret _  | CoZero -> ()
    | CoBind {bind; cmd; _} -> bind_var bind; gocmd cmd
    | CoBox {stk;_} -> gostk stk
    | CoFix stk -> gostk stk
    | CoDestr (Raw_Destr destr) -> List.iter goval destr.args; gostk destr.cont
    | CoCons {cases; default; _} ->
      List.iter (fun (Raw_Cons patt,cmd) ->
          List.iter bind_var patt.args;
          gocmd cmd
        ) cases;
      Option.iter (fun (a,cmd) -> bind_var a; gocmd cmd) default
  in

  let goitem = function
    | Value_declaration {bind; _} -> bind_var bind
    | Value_definition {bind; content; _} -> goval content; bind_var bind in

  let goexec = function
    | Some (Command_execution {content; _}) -> gocmd content
    | None -> ()

  in
  List.iter goitem prog.declarations;
  goexec prog.command;
  !vars, !covars


let type_infer ~trace:trace prog =
  try
    let module P = struct let it = prog.prelude end in
    let open Elaborate.Make (P) in
    let prog, post = go ~trace prog in
    let vars, covars = fill_out_types prog in
    prog.prelude := {!(prog.prelude) with vars; covars};
    let post : FirstOrder.FullFOL.formula = Obj.magic post in
    prog, post
  with

  | Constraint.Type_error (info, loc) ->
    Misc.fatal_error "Type inference" ?loc info
