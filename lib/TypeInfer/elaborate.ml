open Constraint
open Constructors
open Types
open Vars
open Ast
open Prelude
open FullAst


module type Prelude = sig
  val it : prelude
end

module Make (Prelude : Prelude) = struct

  module Params = Constraints_params.Params(Prelude)
  include Constraint.Make (Params)

  let elab_typ : uvar -> typ elaboration = fun u typ ->
    let v, fvs = of_rank1_typ ~sort:(get_sort u) typ in
    exists fvs (eq u v) >>> fun env -> env.u v


  let rec elab_cmd : uvar -> command elaboration = fun u cmd ->
    let Command {pol; valu; stk; mid_typ; final_typ; loc} = cmd in
    let v = fresh_u (Base pol) in
    let cvalu, gvalu = elab_metaval v valu in
    let cstk, gstk = elab_metastack v u stk in
    let cmid, gmid = elab_typ v mid_typ in
    let cfinal, gfinal = elab_typ u final_typ in
    CLoc (loc, exists [v] (cvalu @+ cstk @+ cmid @+ cfinal))
    >>> fun env -> Command {pol ; loc;
                                    valu = gvalu env;
                                    stk = gstk env;
                                    mid_typ = gmid env;
                                    final_typ = gfinal env}


  and elab_metaval : uvar -> meta_value elaboration = fun u mval ->
    let MetaVal {node; val_typ; loc} = mval in
    let cnode, gnode = elab_val u node in
    let ctyp, gtyp = elab_typ u val_typ in
    CLoc (loc, cnode @+ ctyp)
    >>> fun env ->
    MetaVal {node = gnode env; val_typ = gtyp env; loc}


  and elab_metastack : uvar -> uvar -> meta_stack elaboration =
    fun ucont ufinal mstk ->
    let MetaStack {node; cont_typ; final_typ; loc} = mstk in
    let cnode, gnode = elab_stack ucont ufinal node in
    let ccont, gcont = elab_typ ucont cont_typ in
    let cfinal, gfinal = elab_typ ufinal final_typ in
    CLoc (loc, cnode @+ ccont @+ cfinal) >>> fun env ->
    MetaStack {node = gnode env;
               cont_typ = gcont env;
               final_typ = gfinal env;
               loc}

  and elab_var spec u var =
    let con = cvar (Var.to_string var) u spec in
    let con =
      match Var.Env.find var !(Prelude.it).var_multiplicities with
      | MulZero | MulMany ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Box Exponential, [v])) in
        exists [v;u'] (eq u u' @+ con)
      | MulOne -> con
    in
    con, fun _ -> var

  and elab_covar spec u var =
    let con = cvar (CoVar.to_string var) u spec in
    con, fun _ -> var

  and elab_val : uvar -> pre_value elaboration =
    fun u valu -> match valu with

      | Var x ->
        (* TODO especialize here *)
        let spec = new_spec () in
        let con, gvar = elab_var spec u x in
        con, fun env -> Var (gvar env)

      | CoTop ->
        let v,fvs = of_rank1_typ ~sort:(Base Negative) top in
        exists fvs (eq u v) >>> fun _ -> CoTop

      | Bindcc { bind=(a,t); pol; cmd } ->
        (* TODO generalize here *)
        let ct, gt = elab_typ u t in
        let ccmd, gcmd = elab_cmd u cmd in
        ct @+ CDef (CoVar.to_string a, u, ccmd)
        >>> fun env -> Bindcc {
          pol;
          bind = (a, gt env);
          cmd = gcmd env
    }

      | Box { kind; bind=(a,t); cmd } ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Box kind, [v])) in
        let cbind, gbind = elab_typ v t in
        let ccmd, gcmd = elab_cmd v cmd in
        exists [v;u'] (CDef (CoVar.to_string a, v, cbind @+ ccmd @+ eq u u'))
        >>> fun env -> Box {
          kind;
          bind = (a, gbind env);
          cmd = gcmd env
        }

      | Fix {self=(x,t); cmd; cont=(a,t')} ->
        let w = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fix, [w])) in
        let v = shallow ~sort:(Base Positive) (Shallow (Box Exponential, [u'])) in
        let ccmd, gcmd = elab_cmd w cmd in
        let cbind, gbind = elab_typ v t in
        let ccont, gcont = elab_typ w t' in
        exists [u';v;w] (CDef (Var.to_string x, v,
                               CDef (CoVar.to_string a, w,
                                    eq u u' @+ cbind @+ ccont @+ ccmd)))
        >>> fun env ->
        Fix { self = (x, gbind env);
              cmd = gcmd env;
              cont = (a, gcont env)}

      | Cons cons ->
        let ccons, gcons = elab_cons u cons in
        ccons >>> fun env -> Cons (gcons env)

      | Destr copatts ->
        let cpatts, gpatts = List.split @@ List.map (elab_copatt u) copatts in
        CAnd cpatts >>> fun env -> Destr (List.map (fun f -> f env) gpatts)

      | Pack (cons, typs, content) ->
        let Consdef { typ_args; private_typs; val_args; resulting_type; equations }
          = def_of_cons Prelude.it cons in
        let val_arg = match val_args with
          | [val_arg] -> val_arg
          | _ -> raise (Failure "Unsupported") in
        let args_u = of_tvars typ_args in
        let private_u = of_tvars private_typs in
        (* By definition, freevars in equations are all already in scope *)
        let _, equations = of_eqns equations in
        let ctyps, gtyps = List.map2 elab_typ private_u typs |> List.split in
        let v = fresh_u sort_postype in
        let ccont, gcont = elab_metaval v content in
        let cu, _ = elab_typ u resulting_type in
        let carg, _ = elab_typ v val_arg in
        exists ~st:equations (v :: args_u @ private_u)
          (CAnd ctyps @+ ccont @+ cu @+ carg)
        >>> fun env ->
        Pack (cons, List.map (fun g -> g env) gtyps, gcont env)


      | Spec { destr; bind=(a,t); spec_vars; cmd } ->
        let Destrdef { typ_args; private_typs; val_args; ret_arg; resulting_type; equations }
          = def_of_destr Prelude.it destr in
        if val_args <> [] then raise (Failure "Unsupported");

        enter ();
        let args_u = of_tvars typ_args in
        let private_u = of_tvars private_typs in
        let spec_u = of_tvars spec_vars in
        (* By definition, freevars in equations are all already in scope *)
        let _, equations = of_eqns equations in
        let v = fresh_u sort_negtype in
        let cbind, gbind = elab_typ v t in
        let tret, ret_fvs = of_rank1_typ ~sort:sort_negtype ret_arg in
        let ccmd, gcmd = elab_cmd v cmd in
        let cres, _ = elab_typ u resulting_type in
        let ceq = CAnd (List.map2 eq spec_u private_u) in
        leave ();

        cres @+ CGoal {
            var = CoVar.to_string a;
            typ = v;
            accumulated = spec_u;
            inner = exists (v::ret_fvs@args_u)
                (CDef (CoVar.to_string a, v, (cbind @+ ccmd @+ ceq @+ eq v tret)));
            outer = CTrue;
            existentials = [];
            quantification_duty = private_u;
            univ_eqns = equations;
            exist_eqns = [];
            gen = new_gen ();
          }
        >>> fun env -> Spec {
          destr;
          bind = (a, gbind env);
          spec_vars = List.map2 (fun v (_,so) -> (env.get v, so)) spec_u spec_vars;
          cmd = gcmd env
        }

 and elab_stack : uvar -> uvar -> pre_stack elaboration =
    fun ucont ufinal stk -> match stk with

      (* TODO spectialize here *)
      | Ret a ->
        let spec = new_spec () in
        let con, gvar = elab_covar spec ucont a in
        con @+ eq ucont ufinal
        >>> fun env -> Ret (gvar env)

      | CoZero ->
        let v,fvs = of_rank1_typ ~sort:(Base Positive) zero in
        exists fvs (eq ucont v) >>> fun _ -> CoZero

      (* TODO generalize here *)
      | CoBind { bind=(x,t); pol; cmd } ->
        let ccmd, gcmd = elab_cmd ufinal cmd in
        let cbind, gbind = elab_typ ucont t in
        CDef (Var.to_string x, ucont, cbind @+ ccmd)
        >>> fun env -> CoBind {
          bind = (x, gbind env);
          cmd = gcmd env;
          pol
        }

      | CoBox { kind; stk } ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Box kind, [v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [v;u'] (eq ucont u' @+ cstk)
        >>> fun env -> CoBox {
          stk = gstk env;
          kind
        }

      | CoFix stk ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fix, [v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [v;u'] (eq ucont u' @+ cstk)
        >>> fun env -> CoFix (gstk env)

      | CoDestr destr ->
        let cdestr, gdestr = elab_destr ucont ufinal destr in
        cdestr >>> fun env -> CoDestr (gdestr env)

      | CoCons patts ->
        let cpatts, gpatts = List.split @@ List.map (elab_patt ucont ufinal) patts in
        CAnd cpatts
        >>> fun env ->
        CoCons (List.map (fun f -> f env) gpatts)

      | CoPack { cons; bind=(x,t); pack_vars; cmd } ->
        let Consdef { typ_args; private_typs; val_args; resulting_type; equations }
          = def_of_cons Prelude.it cons in
        let val_arg = match val_args with
          | [val_arg] -> val_arg
          | _ -> raise (Failure "Unsupported") in
        enter ();
        let args_u = of_tvars typ_args in
        let pack_u = of_tvars pack_vars in
        let private_u = of_tvars private_typs in
        (* By definition, freevars in equations are all already in scope *)
        let _, equations = of_eqns equations in
        let v = fresh_u sort_postype in
        let cbind, gbind = elab_typ v t in
        let targ, arg_fvs = of_rank1_typ ~sort:sort_postype val_arg in
        let ccmd, gcmd = elab_cmd ufinal cmd in
        let cres, _ = elab_typ ucont resulting_type in
        let ceq = CAnd (List.map2 eq pack_u private_u) in
        leave ();
        cres @+ CGoal {
            var = Var.to_string x;
            typ = v;
            accumulated = pack_u;
            existentials = [];
            exist_eqns = [];
            univ_eqns = equations;
            inner = exists (arg_fvs @ args_u)
                (CDef ( Var.to_string x, v, (cbind @+ ccmd @+ ceq @+ eq v targ)));
            outer = CTrue;
            quantification_duty = private_u;
            gen = new_gen ();
          }
        (* exists fvs (cres @+ CScheme ( (v::[],v), (vs'@ret_fvs, tret), *)
        (*                               cbind @+ CDef (Var.to_string x, v, ccmd) *)
        (*                               @+ ceq @+ eq v tret)) *)
        >>> fun env -> CoPack {
          cons;
          bind = (x, gbind env);
          pack_vars = List.map2 (fun v (_,so) -> (env.get v, so)) pack_u pack_vars;
          cmd = gcmd env
        }

      | CoSpec (destr, typs, content) ->
        let Destrdef { typ_args; private_typs; val_args; ret_arg; resulting_type; equations }
          = def_of_destr Prelude.it destr in
        if val_args <> [] then raise (Failure "Unsupported");
        let args_u = of_tvars typ_args in
        let private_u = of_tvars private_typs in
        (* By definition, freevars in equations are all already in scope *)
        let _, equations = of_eqns equations in
        let ctyps, gtyps = List.map2 elab_typ private_u typs |> List.split in
        let v = fresh_u sort_negtype in
        let ccont, gcont = elab_metastack v ufinal content in
        let cret, _ = elab_typ v ret_arg in
        let cu, _ = elab_typ ucont resulting_type in
        exists ~st:equations (v :: args_u @ private_u) (CAnd ctyps @+ ccont @+ cret @+ cu)
        >>> fun env ->
        CoSpec (destr, List.map (fun g -> g env) gtyps, gcont env)

  and elab_cons : uvar -> (ConsVar.t, meta_value) constructor elaboration =
    fun u cons -> match cons with

      | Unit ->
        let u', fvs = of_rank1_typ ~sort:(Base Positive) unit_t in
        exists fvs (eq u u') >>> fun _ -> Constructors.Unit

      | Thunk mv ->
        let v = fresh_u (Base Positive) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Thunk, [v])) in
        let cmv, gmv = elab_metaval v mv in
        exists [v;u'] (eq u u' @+ cmv)
        >>> fun env -> Constructors.Thunk (gmv env)

      | Tupple mvs ->
        let n = List.length mvs in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cmvs, gmvs = List.split @@ List.map2 elab_metaval vs mvs in
        let u' = shallow ~sort:(Base Positive) (Shallow (Prod n, vs)) in
        exists (u'::vs) (eq u u' @+ CAnd cmvs) >>> fun env ->
        Tupple (List.map (fun f -> f env) gmvs)

      | Inj (i, n, mv) ->
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let v = List.nth vs i in
        let u' = shallow ~sort:(Base Positive) (Shallow (Sum n, vs)) in
        let cmv, gmv = elab_metaval v mv in
        exists (u'::vs) (eq u u' @+ cmv)
        >>> fun env -> Inj (i, n, gmv env)

      | PosCons (cons, args) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let Consdef { val_args; resulting_type; private_typs; _ } =
          def_of_cons Prelude.it cons in
        if private_typs <> [] then
          raise (Failure "Unsupported");
        let u', fvs = of_rank1_typ ~sort:(Base Positive) resulting_type in
        let val_args, fvss = List.split
            (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
        exists (List.concat (vs :: fvs :: fvss))
          (eq u u' @+ CAnd (List.map2 eq vs val_args) @+ CAnd cargs)
        >>> fun env -> PosCons (cons, List.map (fun f -> f env) gargs)


  and elab_destr : uvar -> uvar -> (DestrVar.t, 'x, 'a) destructor elaboration =
    fun ucont ufinal destr -> match destr with

      | Call (args, ret) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let w = fresh_u (Base Negative) in
        let cret, gret = elab_metastack w ufinal ret in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fun n, w :: vs)) in
        exists (u'::w::vs) (eq ucont u' @+ cret @+ CAnd cargs) >>> fun env ->
        Call (List.map (fun f -> f env) gargs, gret env)

      | Proj (i, n, ret) ->
        let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
        let v = List.nth vs i in
        let u' = shallow ~sort:(Base Negative) (Shallow (Choice n, vs)) in
        let cret, gret = elab_metastack v ufinal ret in
        exists (u'::vs) (eq ucont u' @+ cret) >>> fun env ->
        Proj (i, n, gret env)

      | Closure ret ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Closure, [v])) in
        let cret, gret = elab_metastack v ufinal ret in
        exists [v;u'] (eq ucont u' @+ cret)
        >>> fun env -> Constructors.Closure (gret env)

      | NegCons (destr, args, ret) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let w = fresh_u (Base Negative) in
        let cret, gret = elab_metastack w ufinal ret in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let Destrdef { val_args; ret_arg; resulting_type; private_typs; _ } =
          def_of_destr Prelude.it destr in
        if private_typs <> [] then
          raise (Failure "Unsupported");
        let u', fvs = of_rank1_typ ~sort:(Base Negative) resulting_type in
        let vs', fvss = List.split
            (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
        let w', fvs' = of_rank1_typ ~sort:(Base Negative) ret_arg in
        exists (w :: List.concat (vs :: fvs :: fvs' :: fvss))
          (eq ucont u' @+ eq w w' @+ CAnd (List.map2 eq vs vs') @+ CAnd cargs @+ cret)
        >>> fun env ->
        NegCons (destr, List.map (fun f -> f env) gargs, gret env)


  and elab_patt : uvar -> uvar -> (pattern * command) elaboration =
    fun ucont ufinal (patt, cmd) ->
    let ccmd, gcmd = elab_cmd ufinal cmd in
    match patt with

    | Unit ->
      let u', fvs = of_rank1_typ ~sort:(Base Positive) unit_t in
      exists fvs (eq ucont u' @+ ccmd)
      >>> fun env -> (Constructors.Unit, gcmd env)

    | Thunk (x,t) ->
      let v, fvs = of_rank1_typ ~sort:(Base Positive) t in
      let u' = shallow ~sort:(Base Negative) (Shallow (Thunk, [v])) in
      exists (u'::fvs) (eq ucont u' @+ CDef (Var.to_string x, v, ccmd))
      >>> fun env -> (Constructors.Thunk (x, env.u v), gcmd env)

    | Tupple binds ->
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let u' = shallow ~sort:(Base Positive) (Shallow (Prod n, vs)) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_string x, v, c)) in
      let c = List.fold_left2 go ccmd vs binds in
      exists vs (eq ucont u' @+ c)
      >>> fun env ->
      let binds = List.map2 (fun (x,_) v -> x, env.u v) binds vs in
      (Tupple binds, gcmd env)

    | Inj (i, n, (x, t)) ->
      let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
      let v = List.nth vs i in
      let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
      let u' = shallow ~sort:(Base Positive) (Shallow (Sum n, vs)) in
      exists fvs (eq ucont u' @+ eq v v' @+ CDef (Var.to_string x, v, ccmd))
      >>> fun env ->
      (Inj (i, n, (x, env.u v)), gcmd env)

    | PosCons (cons, binds) ->
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let Consdef { val_args; resulting_type; private_typs; _ } =
        def_of_cons (Prelude.it) cons in
      if private_typs <> [] then raise (Failure "Unsupported");
      let u', fvs = of_rank1_typ ~sort:(Base Positive) resulting_type in
      let val_args, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
      let fvs = List.concat (fvs :: fvss) in
      let c = exists fvs (CAnd (ccmd :: eq ucont u' :: List.map2 eq vs val_args)) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_string x, v, c)) in
      let c = List.fold_left2 go c vs binds in
      c >>> fun env ->
      let binds = List.map2 (fun (x,_) v -> x, env.u v) binds vs in
      (PosCons (cons, binds), gcmd env)

  and elab_copatt : uvar -> (copattern * command) elaboration =
    fun ucont (copatt, cmd) ->
    match copatt with

    | Call (binds, (a,t)) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let u' = shallow ~sort:(Base Negative) (Shallow (Fun n, ufinal::vs)) in
      let v', fvs = of_rank1_typ ~sort:(Base Negative) t in
      let go c w (x,t) =
        let w', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq w w' @+ CDef (Var.to_string x, w, c)) in
      let c = List.fold_left2 go ccmd vs binds in
      let c = CDef (CoVar.to_string a, v', c) in
      exists fvs (exists vs (eq ucont u' @+ eq ufinal v' @+ c))
      >>> fun env ->
      let binds = List.map2 (fun (x,_) v -> x, env.u v) binds vs in
      (Call (binds, (a, env.u ufinal)), gcmd env)

    | Proj (i, n, (a,t)) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
      let w = List.nth vs i in
      let w', fvs = of_rank1_typ ~sort:(Base Negative) t in
      let u' = shallow ~sort:(Base Negative) (Shallow (Choice n, vs)) in
      exists fvs (CDef(CoVar.to_string a, w, eq ucont u' @+ eq w w' @+ ccmd))
      >>> fun env ->
      (Proj (i, n, (a, env.u ufinal)), gcmd env)

    | Closure (a,t) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let w, fvs = of_rank1_typ ~sort:(Base Negative) t in
      let u' = shallow ~sort:(Base Positive) (Shallow (Closure, [w])) in
      exists fvs (CDef (CoVar.to_string a, w, eq ucont u' @+ eq ufinal w @+ ccmd))
      >>> fun env -> (Constructors.Closure (a, env.u w), gcmd env)

    | NegCons (destr, binds, (a, ret)) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let Destrdef { val_args; resulting_type; ret_arg; private_typs; _ }
        = def_of_destr Prelude.it destr in
      if private_typs <> [] then
          raise (Failure "Unsupported");

      let u', fvs = of_rank1_typ ~sort:(Base Negative) resulting_type in
      let val_args, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
      let v', fvs' = of_rank1_typ ~sort:(Base Negative) ret in
      let v'', fvs'' = of_rank1_typ ~sort:(Base Negative) ret_arg in
      let fvs = List.concat (fvs :: fvs' :: fvs'' :: fvss) in
      let c = CAnd (eq ucont u'
                    :: eq ufinal v'
                    :: eq v' v''
                    :: List.map2 eq vs val_args) in

      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+
                    CDef (CoVar.to_string a, v',
                          CDef (Var.to_string x, v, c))) in
      let c' = List.fold_left2 go ccmd vs binds in
      exists fvs (c @+ c') >>> fun env ->
      let binds = List.map2 (fun (x,_) v -> x, env.u v) binds vs in
      (NegCons (destr, binds, (a, env.u v')), gcmd env)



  let elab_prog_items items =

    let go (con, gen) item = match item with

      | Value_declaration {name; typ; pol; loc} ->
        let u,fvs = of_rank1_typ ~sort:(Base pol) typ in
        exists fvs (CDef (Var.to_string name, u, con))
        >>> fun env ->
        Value_declaration {name; pol; loc; typ = env.u u} :: gen env

      | Value_definition {name; typ; pol; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) typ in
        let cc, cgen = elab_metaval u content in
        exists fvs (cc @+ CDef (Var.to_string name, u, con))
        >>> fun env ->
        Value_definition {
          name; pol; loc;
          typ = env.u u;
          content = cgen env} :: gen env

      | Command_execution {name; pol; cont; conttyp; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) conttyp in
        let cc, cgen = elab_cmd u content in
        exists fvs (CDef (CoVar.to_string cont, u, cc @+ con))
        >>> fun env ->
        Command_execution {
          name; pol; loc;
          cont = cont;
          conttyp = env.u u;
          content = cgen env} :: gen env in

    List.fold_left go (CTrue, fun _ -> []) (List.rev items)

  let go ~trace:trace items =
    solve ~trace elab_prog_items items
end
