open Ast
open Constructors
open FullAst

type runtime_prog =  {
  cont : S.t list;
  env : V.t VarEnv.t;
  declared : unit VarEnv.t;
  curr : command;
}

exception Internal_No_root_reduction

exception Box_kind_mismatch of runtime_prog

exception Malformed_program of runtime_prog

exception Malformed_case of runtime_prog

let initial_runtime_env curr = {
  cont = [];
  env = VarEnv.empty;
  declared = VarEnv.empty;
  curr;
}

let env_get env var = VarEnv.find var env

let env_add_subst env var valu = VarEnv.add var valu env

let cont_subst cont stk = stk :: cont

let fail_box_kind_mistatch cmd = raise (Box_kind_mismatch cmd)

let fail_malformed_program cmd = raise (Malformed_program cmd)

let fail_malformed_case prog = raise (Malformed_case prog)


let rec reduct_match prog cons patts = match cons, patts with

  | Unit, (Unit, cmd)::_ ->
    {prog with curr = cmd}

  | ShiftPos v, (ShiftPos (x,_), cmd)::_ ->
    {prog with curr = cmd; env =  env_add_subst prog.env x v}

  | Tupple vs, (Tupple vars, cmd)::_ ->
    let env = List.fold_left2 env_add_subst prog.env (List.map fst vars) vs in
    {prog with env; curr = cmd}

  | Inj (i1,n1,v), (Inj (i2,n2,(x,_)), cmd)::_ when  (i1,n1) = (i2,n2) ->
    {prog with curr = cmd; env = env_add_subst prog.env x v}

  | PosCons (cons, args), (PosCons (cons', vars), cmd)::_ when cons = cons' ->
    let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) prog.env vars args in
    {prog with env; curr = cmd}

  | _, _::t -> reduct_match prog cons t

  | _, [] -> raise Not_found


let rec reduct_comatch prog copatts destr = match destr, copatts with
  | Call (vs,s), (Call (vars,_),cmd)::_ ->
    let env = List.fold_left2 env_add_subst prog.env (List.map fst vars) vs in
    {prog with env; curr = cmd; cont = cont_subst prog.cont s}

  | Proj (i1,n1,s), (Proj (i2,n2,_), cmd)::_ when (i1,n1) = (i2,n2) ->
    {prog with curr = cmd; cont = cont_subst prog.cont s}

  | ShiftNeg s, (ShiftNeg _, cmd)::_ ->
    {prog with curr = cmd; cont = cont_subst prog.cont s}

  | NegCons (cons, args, s), (NegCons (cons', vars, _), cmd)::_ when cons = cons' ->
    let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) prog.env vars args in
    {prog with env; curr = cmd; cont = cont_subst prog.cont s}

  | _, _::t -> reduct_comatch prog t destr

  | _, [] -> raise Not_found


let reduct_head_once prog : runtime_prog =

  let (Command cmd) = prog.curr in
  let (MetaVal v) = cmd.valu in
  let (MetaStack s) = cmd.stk in
  let v = v.node and s = s.node in
  match v,s with

  | Box {kind = kind1; bind = _; cmd = mcmd1},
    CoBox {kind = kind2; stk = cont2} ->
    if kind1 <> kind2 then fail_box_kind_mistatch prog;
    { prog with cont = cont_subst prog.cont cont2; curr = mcmd1}


  | Cons cons1, CoCons patts2 ->
    begin try reduct_match prog cons1 patts2
      with Not_found -> fail_malformed_case prog
    end

  | Destr copatts1, CoDestr destr2 ->
    begin try reduct_comatch prog copatts1 destr2
      with Not_found -> fail_malformed_case prog
    end

  | Bindcc {pol = _; bind = _; cmd = mcmd1},
    CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    begin match cmd.pol with
    | Positive ->
      {prog with cont = cont_subst prog.cont cmd.stk; curr = mcmd1}
    | Negative ->
      {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}
    end

  | Bindcc {pol = _; bind = _; cmd = mcmd1}, _ ->
    {prog with cont = cont_subst prog.cont cmd.stk; curr = mcmd1}

  | _, CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}

  | _, Ret ->
    begin match prog.cont with
    | stk :: cont ->
      {prog with cont = cont;
                 env = prog.env;
                 curr = Command {cmd with stk = stk}}
    | [] -> raise Internal_No_root_reduction
    end

  | Var var, _ ->
    begin
      try
        {prog with curr = Command {cmd with valu = env_get prog.env var}}
      with
        Not_found ->
        if VarEnv.mem var prog.declared
        then raise Internal_No_root_reduction
        else fail_malformed_program prog
    end

  | CoTop, _ | _, CoZero -> raise Internal_No_root_reduction

  | _ -> fail_malformed_program prog

let head_normal_form prog =
  let prog = ref prog in
  let rec loop () =
    prog := reduct_head_once !prog;
    loop () in
  try loop () with Internal_No_root_reduction -> !prog