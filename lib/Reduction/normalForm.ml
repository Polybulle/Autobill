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
    let prog' = cmd_nf {prog with cont = []; curr = cmd} in
    Bindcc {bind; pol; cmd = prog'.curr}
  | Box {bind; kind; cmd} ->
    let prog' = cmd_nf {prog with cont = []; curr = cmd} in
    Box {bind; kind; cmd = prog'.curr}
  | Cons cons -> Cons (cons_nf prog cons)
  | Destr copatts -> Destr (List.map (copatt_nf prog) copatts)
  | Fix {cmd; self=(x,t); cont} ->
    let prog' = cmd_nf {prog with cont = [];
                                  curr = cmd;
                                  declared = Var.Env.add x () prog.declared} in
    Fix{self=(x,t); cmd = prog'.curr; cont}
  | Pack (cons, typs, valu) ->
    Pack (cons, typs, metaval_nf prog valu)
  | Spec s -> Spec {s with cmd = (cmd_nf {prog with curr = s.cmd}).curr}

and stack_nf prog stk = match stk with
  | Ret -> begin match prog.cont with
      | [] -> Ret
      | (MetaStack h) :: t -> stack_nf {prog with cont = t} h.node
    end
  | CoZero -> CoZero
  | CoBind {bind; pol; cmd} ->
    let prog' =
      cmd_nf {prog with
              curr = cmd;
              declared = Var.Env.add (fst bind) () prog.declared} in
    CoBind {bind; pol; cmd = prog'.curr}
  | CoBox {kind; stk} -> CoBox {kind; stk = metastack_nf prog stk}
  | CoDestr destr -> CoDestr (destr_nf prog destr)
  | CoCons patts -> CoCons (List.map (patt_nf prog) patts)
  | CoFix stk -> CoFix (metastack_nf prog stk)
  | CoSpec (destr, typs, stk) -> CoSpec (destr, typs, metastack_nf prog stk)
  | CoPack p -> CoPack {p with cmd = (cmd_nf {prog with curr = p.cmd}).curr}

and cons_nf prog cons = match cons with
  | Unit -> Unit
  | ShiftPos v -> ShiftPos (metaval_nf prog v)
  | Tupple vs ->
    Tupple (List.map (metaval_nf prog) vs)
  | Inj (i, n, v) -> Inj (i, n, metaval_nf prog v)
  | PosCons (cons, args) -> PosCons (cons, List.map (metaval_nf prog) args)

and destr_nf prog destr = match destr with
  | Call (vs, s) ->
    Call (List.map (metaval_nf prog) vs, metastack_nf prog s)
  | Proj (i, n, s) -> Proj (i, n, metastack_nf prog s)
  | ShiftNeg s -> ShiftNeg (metastack_nf prog s)
  | NegCons (destr, vs, s) ->
    NegCons (destr, List.map (metaval_nf prog) vs, metastack_nf prog s)

and patt_nf prog (patt, cmd) =
  let binds = match patt with
    | Unit -> []
    | ShiftPos b | Inj (_,_,b) -> [b]
    | Tupple bs | PosCons (_, bs)-> bs in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let prog' = cmd_nf {prog with curr = cmd; declared} in
  patt, prog'.curr

and copatt_nf prog (copatt, cmd) =
  let binds = match copatt with
    | Call (xs, _) | NegCons (_, xs, _) -> xs
    | Proj _ | ShiftNeg _ -> []
    in
  let declared =
    List.fold_right (fun (x,_) decl -> Var.Env.add x () decl) binds prog.declared in
  let prog' = cmd_nf {prog with curr = cmd; cont = []; declared} in
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



and replace_ret_with_cont prog =

  let rec go_cmd cont (Command cmd) =
    match cmd.pol with
    | Positive -> Command {cmd with stk = go_stk cont cmd.stk}
    | Negative -> Command {cmd with valu = go_val cont cmd.valu}

  and go_stk cont (MetaStack s) = MetaStack {s with node = go_stk' cont s.node}

  and go_val cont (MetaVal v) = MetaVal {v with node = go_val' cont v.node}

  and go_val' cont v = match v with
    | Var _ | CoTop | Bindcc _ | Box _ | Fix _ | Spec _ | Destr _ -> v
    | Cons c -> Cons (go_cons cont c)
    | Pack (c,t,v) -> Pack (c,t,go_val cont v)

  and go_stk' cont s = match s with
    | Ret -> begin match cont with
        | [] -> Ret
        | h::t -> let MetaStack s = go_stk t h in s.node
      end
    | CoZero -> s
    | CoBind s -> CoBind {s with cmd = go_cmd cont s.cmd}
    | CoBox s -> CoBox {s with stk = go_stk cont s.stk}
    | CoFix s -> CoFix (go_stk cont s)
    | CoPack s -> CoPack {s with cmd = go_cmd cont s.cmd}
    | CoSpec (d,t,s) -> CoSpec (d, t, go_stk cont s)
    | CoDestr d -> CoDestr (go_destr cont d)
    | CoCons patts ->
      let go_one (p,cmd) = (p, go_cmd cont cmd) in
      CoCons (List.map go_one patts)

  and go_cons cont c = match c with
    | Unit -> Unit
    | ShiftPos v -> ShiftPos (go_val cont v)
    | Tupple vs -> Tupple (List.map (go_val cont) vs)
    | Inj (i,n,v) -> Inj (i,n,go_val cont v)
    | PosCons (c, vs) -> PosCons (c, List.map (go_val cont) vs)

  and go_destr cont d = match d with
    | Call (vs,s) -> Call (vs, go_stk cont s)
    | Proj (i, n, s) -> Proj (i, n, go_stk cont s)
    | ShiftNeg s -> ShiftNeg (go_stk cont s)
    | NegCons (d, vs, s) -> NegCons (d, vs, go_stk cont s)

  in

  {prog with curr = go_cmd prog.cont prog.curr; cont = []}
