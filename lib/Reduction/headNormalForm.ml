open Vars
open Ast
open Constructors
open FullAst
open Prelude

type runtime_prog =  {
  cont : S.t CoVar.Env.t;
  env : V.t Var.Env.t;
  typs : typ TyVar.Env.t;
  declared : unit Var.Env.t;
  declared_cont : unit CoVar.Env.t;
  prelude : prelude;
  curr : command;
  reduce_fixpoints : bool;
  reduce_sharing : bool;
}

exception Internal_No_root_reduction

exception Box_kind_mismatch of runtime_prog

exception Malformed_program of runtime_prog

exception Malformed_case of runtime_prog

let env_get env var = Var.Env.find var env

let env_add_subst env var valu = Var.Env.add var valu env

let typ_get env var = TyVar.Env.find var env

let typ_add_subst env var valu = TyVar.Env.add var valu env

let coenv_get env covar = CoVar.Env.find covar env

let coenv_add_subst env covar stk = CoVar.Env.add covar stk env

let fail_box_kind_mistatch cmd = raise (Box_kind_mismatch cmd)

let fail_malformed_program cmd mess =
  Format.print_string mess;
  Format.print_newline ();
  PrettyPrinter.PP.pp_cmd Format.err_formatter cmd.curr;
  raise (Malformed_program cmd)

let fail_malformed_case prog =raise (Malformed_case prog)

  let rec reduct_match
      prog
      ((Raw_Cons cons) as c)
      (patts : (FullAst.pattern * FullAst.command) list) =
    match patts with
    | [] -> raise Not_found
    | (Raw_Cons patt, cmd) :: patts ->
      let matches = match cons.tag, patt.tag with
        | Unit, Unit -> true
        | Thunk, Thunk -> true
        | Tupple n, Tupple m when n = m -> true
        | Inj (i1,n1), Inj (i2,n2) when (i1,n1) = (i2,n2) -> true
        | PosCons c1, PosCons c2 when c1 = c2 -> true
        | _ ->  false in
      if matches then
        let typs = List.fold_left2
            (fun env (x,_) v -> typ_add_subst env x v) prog.typs patt.typs cons.typs in
        let typs = List.fold_left2
            (fun env (x,_) v -> typ_add_subst env x v) typs patt.idxs cons.idxs in
        let env = List.fold_left2
            (fun env (x,_) v -> env_add_subst env x v) prog.env patt.args cons.args in
        {prog with env; typs; curr = cmd}
      else
        reduct_match prog c patts

  let rec reduct_comatch
      prog
      (copatts : (FullAst.copattern * FullAst.command) list)
      ((Raw_Destr destr) as d) =
    match copatts with
    | [] -> raise Not_found
    | (Raw_Destr copatt, cmd) :: copatts ->
      let matches = match copatt.tag, destr.tag with
        | Call n, Call m when n = m -> true
        | Proj (i1,n1), Proj (i2,n2) when i1=i2 && n1 = n2 -> true
        | Closure q, Closure p when p = q -> true
        | NegCons d, NegCons d' when d = d' -> true
        | _ ->  false in
      if matches then
        let typs = List.fold_left2
            (fun env (x,_) v -> typ_add_subst env x v) prog.typs copatt.typs destr.typs in
        let typs = List.fold_left2
            (fun env (x,_) v -> typ_add_subst env x v) typs copatt.idxs destr.idxs in
        let env = List.fold_left2
            (fun env (x,_) v -> env_add_subst env x v) prog.env copatt.args destr.args in
        let cont = coenv_add_subst prog.cont (fst copatt.cont) destr.cont in
        {prog with env; typs; cont; curr = cmd}
      else
        reduct_comatch prog copatts d



let reduct_head_once prog : runtime_prog =

  let (Command cmd) = prog.curr in
  let (MetaVal v) = cmd.valu in
  let (MetaStack s) = cmd.stk in
  let v = v.node and s = s.node in
  match v,s with

  | Box {kind = kind1; bind = (a,_); cmd = mcmd1},
    CoBox {kind = kind2; stk = cont2}
    when prog.reduce_sharing
    ->
    if kind1 <> kind2 then fail_box_kind_mistatch prog;
    { prog with cont = coenv_add_subst prog.cont a cont2; curr = mcmd1}


  | Cons cons1, CoCons patts2 ->
    begin try reduct_match prog cons1 patts2
      with Not_found -> fail_malformed_case prog
    end

  | Destr copatts1, CoDestr destr2 ->
    begin try reduct_comatch prog copatts1 destr2
      with Not_found -> fail_malformed_case prog
    end

  | Bindcc {pol = _; bind = (covar, _); cmd = mcmd1},
    CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    begin match cmd.pol with
    | Positive ->
      {prog with cont = coenv_add_subst prog.cont covar cmd.stk; curr = mcmd1}
    | Negative ->
      {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}
    end

  | (Fix {self=(x,t); cmd = curr'; cont=(a,t')}),
    CoFix stk
    when prog.reduce_fixpoints ->
    let self = V.box Types.exp (a,t')
        (Command {pol = Types.Negative;
                  valu = cmd.valu;
                  stk = S.ret a;
                  mid_typ = t;
                  final_typ = t;
                  loc = Misc.dummy_pos}) in
    {prog with env = env_add_subst prog.env x self;
               curr = curr';
               cont = coenv_add_subst prog.cont a stk}


  | Bindcc {pol = _; bind = (a,_); cmd = mcmd1}, _ ->
    {prog with cont = coenv_add_subst prog.cont a cmd.stk; curr = mcmd1}

  | _, CoBind {pol = _; bind = (var, _); cmd = mcmd2} ->
    {prog with env = env_add_subst prog.env var cmd.valu; curr = mcmd2}

  | Var var, _ ->
    begin try
        {prog with curr = Command {cmd with valu = env_get prog.env var}}
      with
        Not_found ->
        if Var.Env.mem var prog.declared
        then raise Internal_No_root_reduction
        else fail_malformed_program prog "undefined var"
    end

  | _, Ret a ->
    begin try
        {prog with curr = Command {cmd with stk = coenv_get prog.cont a}}
      with
        Not_found ->
        if CoVar.Env.mem a prog.declared_cont
        then raise Internal_No_root_reduction
        else fail_malformed_program prog "undefined continuation"
    end

  | CoTop, _ | _, CoZero -> raise Internal_No_root_reduction

  | _ -> fail_malformed_program prog "incompatible val and stk"

let head_normal_form prog =
  let prog = ref prog in
  let rec loop () =
    prog := reduct_head_once !prog;
    loop () in
  try loop () with Internal_No_root_reduction -> !prog
