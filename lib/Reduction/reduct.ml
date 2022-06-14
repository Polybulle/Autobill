open Ast
open Constructors
open FullAst

type runtime_prog = S.t list * V.t VarEnv.t * command

exception Internal_No_root_reduction

exception Box_kind_mismatch of runtime_prog

exception Malformed_program of runtime_prog

exception Malformed_case of runtime_prog

let initial_runtime_env = ([], VarEnv.empty)

let env_get env var = VarEnv.find var env

let env_add_subst env var valu = VarEnv.add var valu env

let cont_subst cont stk = stk :: cont

let fail_box_kind_mistatch cmd = raise (Box_kind_mismatch cmd)

let fail_malformed_program cmd = raise (Malformed_program cmd)

let fail_malformed_case prog = raise (Malformed_case prog)


let rec reduct_match cont env cons patts = match cons, patts with

  | Unit, (Unit, cmd)::_ -> cont, env, cmd

  | ShiftPos v, (ShiftPos (x,_), cmd)::_ -> cont, env_add_subst env x v, cmd

  | Pair (v,w), (Pair ((x,_),(y,_)), cmd)::_ ->
    cont, env_add_subst (env_add_subst env x v) y w, cmd

  | Left v, (Left (x,_), cmd)::_ -> cont, env_add_subst env x v, cmd

  | Right v, (Right (x,_), cmd)::_ -> cont, env_add_subst env x v, cmd

  | PosCons (cons, args), (PosCons (cons', vars), cmd)::_ when cons = cons' ->
    let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) env vars args in
    cont, env, cmd

  | _, _::t -> reduct_match cont env cons t

  | _, [] -> raise Not_found


let rec reduct_comatch cont env copatts destr = match destr, copatts with
  | Call (v,s), (Call ((x,_),_),cmd)::_ -> cont_subst cont s, env_add_subst env x v, cmd

  | Yes s, (Yes _, cmd)::_ -> cont_subst cont s, env, cmd

  | No s, (No _, cmd)::_ -> cont_subst cont s, env, cmd

  | ShiftNeg s, (ShiftNeg _, cmd)::_ -> cont_subst cont s, env, cmd

  | NegCons (cons, args, s), (NegCons (cons', vars, _), cmd)::_ when cons = cons' ->
    let env = List.fold_left2
        (fun env (x,_) v -> env_add_subst env x v) env vars args in
    cont_subst cont s, env, cmd

  | _, _::t -> reduct_comatch cont env t destr

  | _, [] -> raise Not_found


let reduct_root_once prog : runtime_prog =

  let (cont, env, mcmd) = prog in
  let (Command cmd) = mcmd in
  let (MetaVal v) = cmd.valu in
  let (MetaStack s) = cmd.stk in
  let v = v.node and s = s.node in
  match v,s with

  | Box {kind = kind1; bind = _; cmd = mcmd1},
    CoBox {kind = kind2; stk = cont2} ->
    if kind1 <> kind2 then fail_box_kind_mistatch prog;
    (cont_subst cont cont2, env, mcmd1)

  | Cons cons1, CoCons patts2 ->
    begin try reduct_match cont env cons1 patts2
      with Not_found -> fail_malformed_case prog
    end

  | Destr copatts1, CoDestr destr2 ->
    begin try reduct_comatch cont env copatts1 destr2
      with Not_found -> fail_malformed_case prog
    end

  | Bindcc {pol = _; bind = _; cmd = mcmd1},
    CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    begin match cmd.pol with
    | Positive -> (cont_subst cont cmd.stk, env, mcmd1)
    | Negative -> (cont, env_add_subst env var cmd.valu, mcmd2)
    end

  | Bindcc {pol = _; bind = _; cmd = mcmd1}, _ ->
    (cont_subst cont cmd.stk, env, mcmd1)

  | _, CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    (cont, env_add_subst env var cmd.valu, mcmd2)

  | _, Ret ->
    begin match cont with
    | stk :: cont -> (cont, env, Command {cmd with stk = stk})
    | [] -> raise Internal_No_root_reduction
    end

  | Var var, _ ->
    begin try (cont, env, Command {cmd with valu = env_get env var})
      with Not_found -> raise Internal_No_root_reduction
    end

  | _ -> fail_malformed_program prog
