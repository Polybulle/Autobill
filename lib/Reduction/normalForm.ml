open Vars
open Ast
open Constructors
open FullAst
open HeadNormalForm

let rec val_nf prog v = match v with
  | Var x ->
    begin match Var.Env.find_opt x prog.env with
      | Some (MetaVal v) -> val_nf prog v.node
      | None -> v
      end
  | CoTop -> CoTop
  | Bindcc {bind; pol; cmd} ->
    let prog' = cmd_nf {prog with curr = cmd} in
    eta_reduce_bindcc (Bindcc {bind; pol; cmd = prog'.curr})
  | Box {bind; kind; cmd} ->
    let prog' = cmd_nf {prog with curr = cmd} in
    Box {bind; kind; cmd = prog'.curr}
  | Cons cons -> Cons (cons_nf prog cons)
  | Destr copatts -> Destr (List.map (copatt_nf prog) copatts)
  | Fix {cmd; self=(x,t); cont} ->
    let prog' = cmd_nf {prog with curr = cmd;
                                  declared = Var.Env.add x () prog.declared} in
    Fix{self=(x,t); cmd = prog'.curr; cont}
  | Pack (cons, typs, valu) ->
    Pack (cons, typs, metaval_nf prog valu)
  | Spec s -> Spec {s with cmd = (cmd_nf {prog with curr = s.cmd}).curr}

and stack_nf prog stk = match stk with
  | Ret a ->
    begin try
        let MetaStack stk' = CoVar.Env.find a prog.cont in
        stack_nf prog stk'.node
      with _ -> stk
    end
  | CoZero -> CoZero
  | CoBind {bind; pol; cmd} ->
    let prog' =
      cmd_nf {prog with
              curr = cmd;
              declared = Var.Env.add (fst bind) () prog.declared} in
    eta_reduce_bind (CoBind {bind; pol; cmd = prog'.curr})
  | CoBox {kind; stk} -> CoBox {kind; stk = metastack_nf prog stk}
  | CoDestr destr -> CoDestr (destr_nf prog destr)
  | CoCons patts -> CoCons (List.map (patt_nf prog) patts)
  | CoFix stk -> CoFix (metastack_nf prog stk)
  | CoSpec (destr, typs, stk) -> CoSpec (destr, typs, metastack_nf prog stk)
  | CoPack p -> CoPack {p with cmd = (cmd_nf {prog with curr = p.cmd}).curr}

and cons_nf prog cons = match cons with
  | Unit -> Unit
  | Thunk v -> Thunk (metaval_nf prog v)
  | Tupple vs ->
    Tupple (List.map (metaval_nf prog) vs)
  | Inj (i, n, v) -> Inj (i, n, metaval_nf prog v)
  | PosCons (cons, args) -> PosCons (cons, List.map (metaval_nf prog) args)

and destr_nf prog destr = match destr with
  | Call (vs, s) ->
    Call (List.map (metaval_nf prog) vs, metastack_nf prog s)
  | Proj (i, n, s) -> Proj (i, n, metastack_nf prog s)
  | Closure s -> Closure (metastack_nf prog s)
  | NegCons (destr, vs, s) ->
    NegCons (destr, List.map (metaval_nf prog) vs, metastack_nf prog s)

and patt_nf prog (patt, cmd) =
  let binds = match patt with
    | Unit -> []
    | Thunk b | Inj (_,_,b) -> [b]
    | Tupple bs | PosCons (_, bs)-> bs in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let prog' = cmd_nf {prog with curr = cmd; declared} in
  patt, prog'.curr

and copatt_nf prog (copatt, cmd) =
  let binds = match copatt with
    | Call (xs, _) | NegCons (_, xs, _) -> xs
    | Proj _ | Closure _ -> []
    in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let prog' = cmd_nf {prog with curr = cmd; declared} in
  copatt, prog'.curr

and metaval_nf prog (MetaVal v) =
  MetaVal {v with node = val_nf prog v.node}

and metastack_nf prog (MetaStack s) =
  MetaStack {s with node = stack_nf prog s.node}

and cmd_nf prog =
  let prog = head_normal_form prog in
  let (Command cmd) = prog.curr in
  let cmd = Command
      {cmd with
       valu = metaval_nf prog cmd.valu;
       stk = metastack_nf prog cmd.stk} in
  let prog = replace_ret_with_cont {prog with curr = cmd} in
  prog


and eta_reduce_bindcc valu = match valu with
  | Bindcc { cmd = Command cmd; bind = (a,_); _} ->
    begin match cmd.stk with
      | MetaStack {node = Ret b; _} when a = b->
        let MetaVal v = cmd.valu in v.node
      | _ -> valu
    end
  | _ -> valu

and eta_reduce_bind stk = match stk with
  | CoBind {bind = (x,_); cmd = Command cmd; _} ->
    begin match cmd.valu with
      | MetaVal {node = Var y; _} ->
        if x = y then let MetaStack s = cmd.stk in s.node
        else stk
      | _ -> stk
    end
  | _ -> stk

and replace_ret_with_cont prog =

  let rec go_cmd (Command cmd) =
    match cmd.pol with
    | Positive -> Command {cmd with stk = go_stk cmd.stk}
    | Negative -> Command {cmd with valu = go_val cmd.valu}

  and go_stk (MetaStack s) = MetaStack {s with node = go_stk' s.node}

  and go_val (MetaVal v) = MetaVal {v with node = go_val' v.node}

  and go_val' v = match v with
    | Var _ | CoTop | Bindcc _ | Box _ | Fix _ | Spec _ | Destr _ -> v
    | Cons c -> Cons (go_cons c)
    | Pack (c,t,v) -> Pack (c,t,go_val v)

  and go_stk' s = match s with
    | Ret a ->
      begin try
        let s = CoVar.Env.find a prog.cont in
        let MetaStack s = go_stk s in
        s.node
      with _ -> Ret a
    end
    | CoZero -> s
    | CoBind s -> CoBind {s with cmd = go_cmd s.cmd}
    | CoBox s -> CoBox {s with stk = go_stk s.stk}
    | CoFix s -> CoFix (go_stk s)
    | CoPack s -> CoPack {s with cmd = go_cmd s.cmd}
    | CoSpec (d,t,s) -> CoSpec (d, t, go_stk s)
    | CoDestr d -> CoDestr (go_destr d)
    | CoCons patts ->
      let go_one (p,cmd) = (p, go_cmd cmd) in
      CoCons (List.map go_one patts)

  and go_cons c = match c with
    | Unit -> Unit
    | Thunk v -> Thunk (go_val v)
    | Tupple vs -> Tupple (List.map go_val vs)
    | Inj (i,n,v) -> Inj (i, n, go_val v)
    | PosCons (c, vs) -> PosCons (c, List.map go_val vs)

  and go_destr d = match d with
    | Call (vs,s) -> Call (vs, go_stk s)
    | Proj (i, n, s) -> Proj (i, n, go_stk s)
    | Closure s -> Closure (go_stk s)
    | NegCons (d, vs, s) -> NegCons (d, vs, go_stk s)

  in

  {prog with curr = go_cmd prog.curr}
