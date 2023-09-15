open Ast
open Constructors
open FullAst
open Prelude
open HeadNormalForm
open Types
open Vars

let rec typ_nf env (t:typ) = match t with
  | TCons _ -> t
  | TVar {node=x;_} | TInternal x -> begin
      try typenv_get env x with _ -> t
    end
  | TApp {tfun; args; loc} ->
    let tfun = typ_nf env tfun in
    let args = List.map (typ_nf env) args in
    match tfun with
    | TCons {node;_} ->
      let def = def_of_tycons env.prelude node in
      begin match def.content with
      | Defined typ ->
        let env = List.fold_left2
            (fun env (x,_) t -> typenv_add env x t)
            env def.args args in
        typ_nf env typ
      | _ -> TApp {tfun; args; loc}
      end
    | _ -> TApp {tfun; args; loc}

let bind_nf env (x,t) =
  let env = env_declare env x in
  (env, (x, typ_nf env t))

let cobind_nf env (a,t) =
  let env = coenv_declare env a in
  (env, (a, typ_nf env t))

let typbind_nf env (t,so) =
  let env = typenv_declare env t in
  (env, (t, so))

let rec val_nf env v = match v with
  | Var x ->
    if Vars.Var.Env.mem x env.declared_vars || (env_is_shared env x && not env.reduce_sharing) then
      v
    else begin
      match (try Some (let MetaVal v = env_get env x in v.node) with Not_found -> None) with
      | None -> v
      | Some v -> val_nf env v
    end
  | CoTop -> CoTop
  | Bindcc {bind; pol; cmd} ->
    let env, bind = cobind_nf env bind in
    let cmd' = cmd_nf env cmd in
    eta_reduce_bindcc (Bindcc {bind; pol; cmd = cmd'})
  | Box {bind; kind; cmd} ->
    let env, bind = cobind_nf env bind in
    Box {bind; kind; cmd = cmd_nf env cmd}
  | Cons cons -> Cons (cons_nf env cons)
  | Destr {cases; default; for_type} -> Destr {
      for_type;
      cases = List.map (copatt_nf env) cases;
      default = Option.map (fun (a,cmd) ->
          let env, a =  cobind_nf env a in (a, cmd_nf env cmd))
          default;
    }
  | Fix {bind; stk} ->
    let env, bind = cobind_nf env bind in
    Fix{bind; stk = metastack_nf env stk}

and stack_nf env stk = match stk with
  | Ret a ->
    (try stack_nf env (let MetaStack s = coenv_get env a in s.node)
     with Not_found -> stk)
  | CoZero -> CoZero
  | CoBind {bind; pol; cmd} ->
    let env, bind = bind_nf env bind in
    let cmd = cmd_nf env cmd in
    eta_reduce_bind (CoBind {bind; pol; cmd})
  | CoBox {kind; stk} -> CoBox {kind; stk = metastack_nf env stk}
  | CoDestr destr -> CoDestr (destr_nf env destr)
  | CoCons {cases; default; for_type} -> CoCons {
      for_type;
      cases = List.map (patt_nf env) cases;
      default = Option.map (fun (x,cmd) ->
          let env, x = bind_nf env x in (x, cmd_nf env cmd))
          default;
    }
  | CoFix stk -> CoFix (metastack_nf env stk)

and cons_nf prog (Raw_Cons cons) = Raw_Cons {
    tag = cons.tag;
    idxs = List.map (typ_nf prog) cons.idxs;
    args = List.map (metaval_nf prog) cons.args;
  }

and destr_nf prog (Raw_Destr cons) = Raw_Destr {
    tag = cons.tag;
    idxs = List.map (typ_nf prog) cons.idxs;
    args = List.map (metaval_nf prog) cons.args;
    cont = metastack_nf prog cons.cont
  }

and patt_nf env (patt, cmd) =
  let Raw_Cons { tag; idxs; args } = patt in
  let env, idxs = List.fold_left_map
      (fun env (x,t) -> typenv_declare env x, (x,t)) env idxs in
  let env, args = List.fold_left_map
      (fun env (x,t) -> (env_declare env x, (x, typ_nf env t))) env args in
  let cmd = cmd_nf env cmd in
  (Raw_Cons {tag; idxs; args}, cmd)

and copatt_nf env (copatt, cmd) =
  let Raw_Destr { tag; idxs; args; cont } = copatt in
  let env, idxs = List.fold_left_map typbind_nf env idxs in
  let env, args = List.fold_left_map bind_nf env args in
  let env, cont = cobind_nf env cont in
  let cmd = cmd_nf env cmd in
  (Raw_Destr {tag; idxs; args; cont}, cmd)

and metaval_nf prog (MetaVal v) =
  MetaVal {
    node = val_nf prog v.node;
    val_typ = typ_nf prog v.val_typ;
    loc = v.loc}

and metastack_nf prog (MetaStack s) =
  MetaStack {
    node = stack_nf prog s.node;
    cont_typ = typ_nf prog s.cont_typ;
    loc = s.loc}

and cmd_nf env cmd =
  let (env, Command cmd) =
    if env.reduce_commands then head_normal_form (env, cmd) else (env, cmd) in
  Command
    {loc = cmd.loc;
     pol = cmd.pol;
     node = precmd_nf env cmd.node;
    }

and precmd_nf env = function
  | Interact {valu; stk; mid_typ}
    -> Interact {
        mid_typ = typ_nf env mid_typ;
        valu = metaval_nf env valu;
        stk = metastack_nf env stk
      }
  | Trace {dump; comment; cmd}
    -> Trace {
        dump = Option.map (metaval_nf env) dump;
        comment;
        cmd = cmd_nf env cmd
      }
  | Struct {valu; binds; cmd}
    ->
    let valu = metaval_nf env valu in
    let env, binds = List.fold_left_map bind_nf env binds in
    let cmd = cmd_nf env cmd in
    Struct {valu; binds; cmd}

  | Pack {stk; name; cmd}
    ->
    let stk = metastack_nf env stk in
    let env = coenv_declare env name in
    let cmd = cmd_nf env cmd in
    Pack {stk; name; cmd}

  | Spec {valu; name; cmd}
    ->
    let valu = metaval_nf env valu in
    let env = env_declare env name in
    let cmd = cmd_nf env cmd in
    Spec {valu; name; cmd}

and eta_reduce_bindcc valu = match valu with
  | Bindcc {
      bind = (a,_);
      cmd = Command {
          node = Interact {
              valu = MetaVal {node = valu'; _};
              stk = MetaStack {node = Ret b; _};
              _
            };_
        };_
    } -> if a = b && not (free_in_preval a valu') then valu' else valu
  | _ -> valu

and eta_reduce_bind stk = match stk with
  | CoBind {
      bind = (x,_);
      cmd = Command {
          node = Interact {
              valu = MetaVal {node = Var y; _};
              stk = MetaStack {node = stk'; _};
              _
            };_
        };_
    } -> if x = y && not (free_in_prestk x stk') then stk' else stk
  | _ -> stk


and empty = (Var.Env.empty, CoVar.Env.empty)

and single_var v = (Var.Env.singleton v (), CoVar.Env.empty)

and single_covar a = (Var.Env.empty, CoVar.Env.singleton a ())

and remove_var x (xs,bs) = (Var.Env.remove x xs, bs)

and remove_args x y = List.fold_left (fun vs (v,_) -> remove_var v vs) y x

and remove_covar b (xs,bs) = (xs, CoVar.Env.remove b bs)

and concat l =
  let rec go (xs,bs) = function
    | [] -> (xs,bs)
    | (ys,cs)::t ->
      let acc = (Var.Env.union (fun _ _ _ -> Some ()) xs ys,
                 CoVar.Env.union (fun _ _ _ -> Some ()) bs cs) in
      go acc t in
  match l with
  | [] -> empty
  | [vs] -> vs
  | vs::vss -> go vs vss

and free_in_preval a v =
  let (_,bs) = fv_of_preval v in
  CoVar.Env.mem a bs

and free_in_prestk x s =
  let (xs,_) = fv_of_prestk s in
  Var.Env.mem x xs

and fv_of_val (MetaVal {node;_}) = fv_of_preval node

and fv_of_stk (MetaStack {node;_}) = fv_of_prestk node

and fv_of_cmd (Command {node;_}) = fv_of_precmd node

and fv_of_preval v = match v with
  | Var v -> single_var v
  | CoTop -> empty
  | Bindcc { bind = (a, _); cmd; _} | Box {bind = (a, _); cmd; _ } ->
    remove_covar a (fv_of_cmd cmd)
  | Fix { bind = (a,_); stk } -> remove_covar a (fv_of_stk stk)
  | Cons (Raw_Cons {args; _}) -> concat (List.map fv_of_val args)
  | Destr { cases; default; _} ->
    let x = begin match default with
      | None -> empty
      | Some ((a, _), cmd) -> remove_covar a (fv_of_cmd cmd)
    end in
    concat (x :: List.map fv_of_destr_patt cases)

and fv_of_destr_patt (Raw_Destr {args;cont = (a,_);_}, cmd) =
  remove_covar a (remove_args args (fv_of_cmd cmd))

and fv_of_prestk s = match s with
  | Ret a -> single_covar a
  | CoZero -> empty
  | CoBind { bind = (x,_); cmd; _} -> remove_var x (fv_of_cmd cmd)
  | CoBox {stk=s; _} | CoFix s -> fv_of_stk s
  | CoDestr (Raw_Destr {args; cont; _}) -> concat (fv_of_stk cont :: List.map fv_of_val args)
  | CoCons { cases; default; _} ->
    let x = begin match default with
      | None -> empty
      | Some ((x, _), cmd) -> remove_var x (fv_of_cmd cmd)
    end in
    concat (x :: List.map fv_of_cons_patt cases)

and fv_of_cons_patt (Raw_Cons {args; _}, cmd) = remove_args args (fv_of_cmd cmd)

and fv_of_precmd c = match c with
  | Interact {valu; stk; _} -> concat [fv_of_val valu; fv_of_stk stk]
  | Trace {dump; cmd; _} ->
    let dump = match dump with
      | Some dump -> fv_of_val dump
      | None -> empty in
    concat [dump; fv_of_cmd cmd]
  | Struct { valu; binds; cmd } ->
    concat [fv_of_val valu; remove_args binds (fv_of_cmd cmd) ]
  | Pack {stk; name; cmd} ->
    concat [fv_of_stk stk; remove_covar name (fv_of_cmd cmd)]
  | Spec {valu; name; cmd} ->
    concat [fv_of_val valu; remove_var name (fv_of_cmd cmd)]
