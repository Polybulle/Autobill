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

module Make (P : Prelude) = struct

  module Params = Constraints_params.Params(P)
  include Constraint.Make (Params)

  let elab_typ : uvar -> typ elaboration = fun u typ ->
    let v, fvs = of_rank1_typ ~sort:(get_sort u) typ in
    exists fvs (eq u v) >>> fun env -> env.u v

  let of_eqns eqns =
    let of_eqn = function
      | Eq (a,b,sort) ->
        let a,fvsa = of_rank1_typ ~sort a in
        let b,fvsb = of_rank1_typ ~sort b in
        Eq (a, b, sort), fvsa @ fvsb
      | Rel (rel, args) ->
        let aux = List.map2 (fun sort t -> of_rank1_typ ~sort t) (sort_of_rel rel) args in
        let args, fvss = List.split aux in
        let fvs = List.concat fvss in
        Rel (rel, args), fvs in
    List.fold_left_map
      (fun fvs eqn -> let eqn, fvs' = of_eqn eqn in (fvs@fvs',eqn))
      []
      eqns

  let rec elab_cmd : command elaboration = fun cmd ->
    let Command {pol; node; loc} = cmd in
    let ccmd, gcmd = elab_precmd pol node in
    CLoc (loc, ccmd)
    >>> fun env -> Command {pol ; loc; node = gcmd env}

  and elab_metaval : uvar -> meta_value elaboration = fun u mval ->
    let MetaVal {node; val_typ; loc} = mval in
    let cnode, gnode = elab_val u node loc in
    let ctyp, gtyp = elab_typ u val_typ in
    CLoc (loc, cnode @+ ctyp )
    >>> fun env ->
    MetaVal {node = gnode env; val_typ = gtyp env; loc}


  and elab_metastack : uvar -> meta_stack elaboration =
    fun ucont mstk ->
    let MetaStack {node; cont_typ; loc} = mstk in
    let cnode, gnode = elab_stack ucont node loc in
    let ccont, gcont = elab_typ ucont cont_typ in
    CLoc (loc, cnode @+ ccont) >>> fun env ->
    MetaStack {node = gnode env;
               cont_typ = gcont env;
               loc}

  and elab_var u var = cvar (Var.to_int var) u >>> fun _ -> var

  and elab_covar u var = cvar (CoVar.to_int var) u >>> fun _ -> var

  and elab_precmd pol cmd = match cmd with

    | Interact {valu; stk; mid_typ}
      ->
      let u = fresh_u (Base pol) in
      let cvalu, gvalu = elab_metaval u valu in
      let cstk, gstk = elab_metastack u stk in
      let ctyp, gtyp = elab_typ u mid_typ in
      cvalu @+ cstk @+ ctyp
      >>> fun env -> Interact {valu = gvalu env; stk = gstk env; mid_typ = gtyp env}

    | Trace {dump; comment; cmd}
      ->
      let u = fresh_u (Base pol) in
      let cdump, gdump = match dump with
        | Some dump  -> let c,g = elab_metaval u dump in c, (fun env -> Some (g env))
        | None -> CTrue, fun _ -> None in
      let ccmd, gcmd = elab_cmd cmd in
      cdump @+ ccmd
      >>> fun env ->
      Trace {dump = gdump env; comment; cmd = gcmd env}

    | Struct {valu; binds; cmd} ->
      let u = fresh_u sort_postype in
      let cvalu, gvalu = elab_metaval u valu in
      let go (fvss,cbinds) (x,t) =
        let v, fvs = of_rank1_typ ~sort:(Base Positive) t in
        (fvs :: fvss, fun c -> eq u v @+ CDef (Var.to_int x, Var.to_string x, v, cbinds c)) in
      let fvss, cbinds = List.fold_left go ([], fun c -> c) binds in
      let ccmd, gcmd = elab_cmd cmd in
      cvalu @+ exists (List.concat fvss) (cbinds ccmd)
      >>> fun env ->
      Struct {
        valu = gvalu env;
        binds = List.map (fun (x,_) -> (x, env.u u)) binds;
        cmd = gcmd env;
      }

    | Spec {valu; name; cmd} ->
      let u = fresh_u sort_negtype in
      let cvalu, gvalu = elab_metaval u valu in
      let ccmd, gcmd = elab_cmd cmd in
      cvalu @+ CDef (Var.to_int name, Var.to_string name, u, ccmd)
      >>>
      fun env -> Interact {
        valu = gvalu env;
        mid_typ = env.u u;
        stk = MetaStack {
            node = CoBind {
                bind = (name, env.u u);
                cmd = gcmd env;
                pol = Negative
              };
            cont_typ = env.u u;
            loc = Misc.dummy_pos
          }
      }

    | Pack {stk; name; cmd} ->
      let u = fresh_u sort_postype in
      let cstk, gstk = elab_metastack u stk in
      let ccmd, gcmd = elab_cmd cmd in
      cstk @+ CDef (CoVar.to_int name, CoVar.to_string name, u, ccmd)
      >>>
      fun env -> Interact {
        stk = gstk env;
        mid_typ = env.u u;
        valu = MetaVal {
            node = Bindcc {
                bind = (name, env.u u);
                cmd = gcmd env;
                pol = Negative
              };
            val_typ = env.u u;
            loc = Misc.dummy_pos
          }
      }

  and elab_val u valu loc = match valu with

    | Var x ->
      let con, gvar = elab_var u x in
      con, fun env -> Var (gvar env)

    | CoTop ->
      let v,fvs = of_rank1_typ ~sort:(Base Negative) top in
      exists fvs (eq u v) >>> fun _ -> CoTop

    | Bindcc { bind=(a,t); pol; cmd } ->
      let ct, gt = elab_typ u t in
      let ccmd, gcmd = elab_cmd cmd in
      ct @+ CDef (CoVar.to_int a, CoVar.to_string a, u, ccmd)
      >>> fun env -> Bindcc {
        pol;
        bind = (a, gt env);
        cmd = gcmd env
      }

    | Box { kind; bind=(a,t); cmd } ->
      let v = fresh_u (Base Negative) in
      let u' = shallow ~sort:(Base Positive) (Shallow (Cons (Box kind), [v])) in
      let cbind, gbind = elab_typ v t in
      let ccmd, gcmd = elab_cmd cmd in
      exists [v;u'] (CDef (CoVar.to_int a, CoVar.to_string a, v, cbind @+ ccmd @+ eq u u'))
      >>> fun env -> Box {
        kind;
        bind = (a, gbind env);
        cmd = gcmd env
      }

    | Fix {bind=(a,t'); stk} ->
      let v = fresh_u (Base Negative) in
      let w = shallow ~sort:(Base Positive) (Shallow (Cons Fix, [v])) in
      let cstk, gstk = elab_metastack w stk in
      let cbind, gbind = elab_typ v t' in
      exists [v;w]
        (CDef (CoVar.to_int a, CoVar.to_string a, v, eq u w @+ cbind @+ cstk))
      >>> fun env -> Fix { stk = gstk env; bind = (a, gbind env)}

    | Cons (Raw_Cons cons) ->
      begin match cons.tag with
        | Int _ ->
          let u', fvs = of_rank1_typ ~sort:sort_postype int in
          exists fvs (eq u u') >>> fun _ -> Cons (Raw_Cons cons)
        | _ ->
          let ccons, gcons = elab_cons u (Raw_Cons cons) in
          ccons >>> fun env -> Cons (gcons env)
      end

    | Destr {default; cases; for_type} ->
      let {args; sort; content; _} = def_of_tycons P.it for_type in
      let sort = snd (unmk_arrow sort) in
      let defs = match content with
        | Codata defs -> defs
        | _ -> Misc.fail_invariant_break ~loc
                 "This computation litteral has a non-computation type" in
      let u_typ_args = of_tvars args in
      let ures, fvs_res =
        of_rank1_typ ~sort (app (cons for_type) (List.map (fun (x,_) -> tvar x) args)) in

      let find_def (Raw_Destr c, _) =
        List.find (fun (tag, _ ,_) -> tag = c.tag) defs in
      let cases = List.map (fun x -> (x, find_def x)) cases in
      let ccases, gcases = List.split @@ List.map elab_copatt cases in
      let cdefault, gdefault = match default with
        | None -> CTrue, fun _ -> None
        | Some ((a,t), cmd) ->
          let ct, gt = elab_typ u t in
          let ccmd, gcmd = elab_cmd cmd in
          ct @+ CDef (CoVar.to_int a, CoVar.to_string a, u, ccmd)
          >>> fun env -> Some ((a, gt env), gcmd env)
      in
      exists (u_typ_args @ fvs_res) (eq u ures @+ cdefault @+ CCases ccases)
      >>> fun env -> Destr {cases = List.map (fun f -> f env) gcases;
                            default = gdefault env;
                            for_type}


  and elab_stack ucont stk loc = match stk with

    | Ret a ->
      let con, gvar = elab_covar ucont a in
      con >>> fun env -> Ret (gvar env)

    | CoZero ->
      let v,fvs = of_rank1_typ ~sort:(Base Positive) zero in
      exists fvs (eq ucont v) >>> fun _ -> CoZero

    | CoBind { bind=(x,t); pol; cmd } ->
      let ccmd, gcmd = elab_cmd cmd in
      let cbind, gbind = elab_typ ucont t in
      CDef (Var.to_int x, Var.to_string x, ucont, cbind @+ ccmd)
      >>> fun env -> CoBind {
        bind = (x, gbind env);
        cmd = gcmd env;
        pol
      }

    | CoBox { kind; stk } ->
      let v = fresh_u (Base Negative) in
      let u' = shallow ~sort:(Base Positive) (Shallow (Cons (Box kind), [v])) in
      let cstk, gstk = elab_metastack v stk in
      exists [v;u'] (eq ucont u' @+ cstk)
      >>> fun env -> CoBox {
        stk = gstk env;
        kind
      }

    | CoFix stk ->
      let v = fresh_u (Base Negative) in
      let u' = shallow ~sort:(Base Positive) (Shallow (Cons Fix, [v])) in
      let cstk, gstk = elab_metastack v stk in
      exists [v;u'] (eq ucont u' @+ cstk)
      >>> fun env -> CoFix (gstk env)

    | CoDestr destr ->
      let cdestr, gdestr = elab_destr ucont destr in
      cdestr >>> fun env -> CoDestr (gdestr env)

    | CoCons {cases; default; for_type} ->

      let {args; sort; content; _} = def_of_tycons P.it for_type in
      let sort = snd (unmk_arrow sort) in
      let defs =
        begin match content with
          | Data defs -> defs
          | _ -> Misc.fail_invariant_break ~loc "This datatype litteral has a non-data type"
        end in
      let u_typ_args = of_tvars args in
      let ures, fvs_res =
        of_rank1_typ ~sort (app (cons for_type) (List.map (fun (x,_) -> tvar x) args)) in

      let find_def (Raw_Cons c, _) =
        List.find (fun (tag, _, _) -> tag = c.tag) defs in
      let cases = List.map (fun x -> (x, find_def x)) cases in
      let ccases, gcases = List.split @@ List.map elab_patt cases in
      let cdefault, gdefault =
        begin match default with
          | None -> CTrue, fun _ -> None
          | Some ((x,t), cmd) ->
            let ct, gt = elab_typ ucont t in
            let ccmd, gcmd = elab_cmd cmd in
            ct @+ CDef (Var.to_int x, Var.to_string x, ucont, ccmd)
            >>> fun env -> Some ((x, gt env), gcmd env)
        end in
      exists (u_typ_args @ fvs_res) (eq ucont ures @+ cdefault @+ CCases ccases)
      >>> fun env ->
      CoCons {cases = List.map (fun f -> f env) gcases;
              default = gdefault env;
              for_type}



  and elab_cons u (Raw_Cons cons) =

    let Consdef {resulting_type; typ_args; equations; constructor = Raw_Cons def} =
      def_of_cons P.it cons.tag in

    let u_typ_args = of_tvars typ_args in


    let so = match cons.tag with Closure -> sort_negtype | _ -> sort_postype in
    let u_res, fvs_res = of_rank1_typ ~sort:sort_postype resulting_type in
    let fvs_def = u_typ_args @ fvs_res in

    enter ();
    let u_args = List.init (List.length def.args) (fun _ -> fresh_u so) in
    let cargs, gargs = List.split @@ List.map2 elab_metaval u_args cons.args in
    let u_def_args, fvss = List.split (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
    let fvs_args = u_args @ List.concat fvss in

    let u_def_idxs = of_tvars def.idxs in
    let u_idxs = List.map (fun (_,so) -> fresh_u so) def.idxs in
    let fve, equations = of_eqns equations in
    let cidxs, gidxs = List.map2 elab_typ u_idxs cons.idxs |> List.split in
    let fvs_idxs = u_idxs @ u_def_idxs @ fve in

    leave ();

    let fvs = fvs_def @ fvs_args @ fvs_idxs in

    exists fvs ~st:equations
      (eq u u_res
       @+ CAnd (List.map2 eq u_def_args u_args)
       @+ CAnd (List.map2 eq u_def_idxs u_idxs)
       @+ CAnd cargs
       @+ CAnd cidxs)
    >>> fun env -> Raw_Cons {
      tag = def.tag;
      idxs = List.map (fun f -> f env) gidxs;
      args = List.map (fun f -> f env) gargs
    }


  and elab_destr ucont (Raw_Destr destr) =

    let Destrdef {resulting_type; typ_args; equations; destructor = Raw_Destr def} =
      def_of_destr P.it destr.tag in

    let u_typ_args = of_tvars typ_args in
    let so = match destr.tag with Thunk  -> sort_postype | _ -> sort_negtype in
    let u_res, fvs_res = of_rank1_typ ~sort:sort_negtype resulting_type in
    let fvs_def = u_typ_args @ fvs_res in

    enter ();
    let u_args = List.init (List.length def.args) (fun _ -> fresh_u so) in
    let cargs, gargs = List.split @@ List.map2 elab_metaval u_args destr.args in
    let u_def_args, fvss = List.split (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
    let fvs_args = u_args @ List.concat fvss in

    let u_def_idxs = of_tvars def.idxs in
    let u_idxs = List.map (fun (_,so) -> fresh_u so) def.idxs in
    let fve, equations = of_eqns equations in
    let cidxs, gidxs = List.map2 elab_typ u_idxs destr.idxs |> List.split in
    let fvs_idxs = u_idxs @ u_def_idxs @ fve in

    let u_final = fresh_u (Base Negative) in
    let cfinal, gfinal = elab_metastack u_final destr.cont in
    let u_def_final, fvs_final = of_rank1_typ ~sort:(Base negative) def.cont in
    let fvs_final = u_final :: fvs_final in

    leave ();

    let fvs = fvs_def @ fvs_args @ fvs_idxs @ fvs_final in

    exists fvs ~st:equations
      (eq ucont u_res
       @+ CAnd (List.map2 eq u_def_args u_args)
       @+ CAnd (List.map2 eq u_def_idxs u_idxs)
       @+ eq u_final u_def_final
       @+ CAnd cargs
       @+ CAnd cidxs
       @+ cfinal)
    >>> fun env -> Raw_Destr {
      tag = def.tag;
      idxs = List.map (fun f -> f env) gidxs;
      args = List.map (fun f -> f env) gargs;
      cont = gfinal env
    }

  and mk_model_eqns loc us defs binds = match (us, defs, binds) with
    | x::xs, y::ys, (_,so)::sos -> Eq (x,y,so) :: mk_model_eqns loc xs ys sos
    | [],[],[] -> []
    | _ -> Misc.fail_invariant_break ~loc "Paramter equations are bad-sorted at type inference"

  and elab_patt ((Raw_Cons patt, cmd), (_, (Raw_Cons def : Prelude.cons_for_def), equations)) =

    let Command {loc; _} = cmd in

    match patt.tag with
    | Int _ ->
      let ccmd, gcmd = elab_cmd cmd in
      ccmd >>> fun env -> (Raw_Cons patt, gcmd env)
    | _ ->

      enter ();
      let u_idxs = of_tvars patt.idxs in
      let u_def_idxs = of_tvars def.idxs in
      let fvs_eqns, equations = of_eqns equations in
      let fvs_idxs = u_def_idxs @ fvs_eqns in

      let args_so = match patt.tag with Constructors.Closure -> sort_negtype | _ -> sort_postype in
      let u_args = List.init (List.length patt.args) (fun _ -> fresh_u args_so) in
      let u_def_args, fvss = List.split (List.map (of_rank1_typ ~sort:args_so) def.args) in
      let go (fvss,cbinds) v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        (fvs :: fvss, fun c -> eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, cbinds c)) in
      let fvss', cbinds = List.fold_left2 go ([], fun c -> c) u_args patt.args in
      let fvs_args = List.concat (fvss @ fvss') in
      let ccmd, gcmd = elab_cmd cmd in


      let idx_fvs, typ_fvs =
        List.partition (fun x -> is_index_sort (get_sort x)) (fvs_idxs @ fvs_args) in

      let con = CUniv {
          typs = u_args;
          duty = (List.filter (fun x -> rank x = !_rank) (u_idxs @ idx_fvs));
          assume = equations @ mk_model_eqns loc u_idxs u_def_idxs def.idxs;
          exists = typ_fvs;
          witness = [];
          inner = (cbinds (CAnd (List.map2 eq u_args u_def_args) @+ ccmd))
        } in

      leave ();

      con >>> fun env -> (Raw_Cons {
          tag = patt.tag;
          idxs = List.map (fun v -> (env.get v, get_sort v)) u_idxs;
          args = List.map2 (fun (x,_) v -> x, env.u v) patt.args u_args;
        }, gcmd env)


  and elab_copatt ((Raw_Destr copatt, cmd), (_, (Raw_Destr def), equations)) =

    let Command {loc;_} = cmd in

    enter ();
    let u_idxs = of_tvars copatt.idxs in
    let u_def_idxs = of_tvars def.idxs in
    let fvs_eqns, equations = of_eqns equations in
    let fvs_idxs = u_def_idxs @ fvs_eqns in

    let u_args = List.init (List.length copatt.args) (fun _ -> fresh_u (Base Positive)) in
    let u_def_args, fvss = List.split (List.map (of_rank1_typ ~sort:(Base Positive)) def.args) in
    let go (fvss,cbinds) v (x,t) =
      let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
      (fvs :: fvss, fun c -> eq v v' @+ CDef (Var.to_int x, Var.to_string x, v, cbinds c)) in
    let fvss', cbinds = List.fold_left2 go ([], fun c -> c) u_args copatt.args in
    let fvs_args = u_args @ List.concat (fvss @ fvss') in

    let cont_so = match copatt.tag with Constructors.Thunk -> sort_postype | _ -> sort_negtype in
    let a, typ_final = copatt.cont in
    let u_def_final, fvs = of_rank1_typ ~sort:cont_so def.cont in
    let u_final, fvs' = of_rank1_typ ~sort:cont_so typ_final in
    let c_cont_bind c = CDef (CoVar.to_int a, CoVar.to_string a, u_final, c ) in
    let fvs_final = fvs @ fvs' in

    let idx_fvs, typ_fvs =
      List.partition (fun x -> is_index_sort (get_sort x)) (fvs_idxs @ fvs_args @ fvs_final) in

    let ccmd, gcmd = elab_cmd cmd in

    let con = CUniv {
        typs = u_args;
        duty = (List.filter (fun x -> rank x = !_rank) u_idxs @ idx_fvs);
        assume = equations @ mk_model_eqns loc u_idxs u_def_idxs def.idxs;
        exists = typ_fvs;
        witness = [];
        inner = cbinds (c_cont_bind
                          (CAnd (List.map2 eq u_args u_def_args)
                           @+ ccmd
                           @+ eq u_final u_def_final));
      } in
    leave ();
    con >>> fun env -> (Raw_Destr {
        tag = copatt.tag;
        idxs = List.map (fun v -> (env.get v, get_sort v)) u_idxs;
        args = List.map2 (fun (x,_) v -> x, env.u v) copatt.args u_args;
        cont = (a, env.u u_final)
      }, gcmd env)


  let elab_prog_items (cexec, gexec) items =

    let go (con, gen) item = match item with

      | Value_declaration {bind = (name, typ); pol; loc} ->
        let u,fvs = of_rank1_typ ~sort:(Base pol) typ in
        exists fvs (CDef (Var.to_int name, Var.to_string name, u, con))
        >>> fun env ->
        Value_declaration {bind = (name, env.u u); pol; loc;} :: gen env

      | Value_definition {bind = (name, typ); pol; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) typ in
        let cc, cgen = elab_metaval u content in
        exists fvs (cc @+ CDef (Var.to_int name, Var.to_string name, u, con))
        >>> fun env ->
        Value_definition {
          bind = (name, env.u u);
          pol; loc;
          content = cgen env} :: gen env in

    let con, gen = List.fold_left go (cexec, fun _ -> []) (List.rev items) in
    con >>> fun env -> (gexec env, gen env)


  let elab_exec exec = match exec with
    | Some (Command_execution {name; pol; cont; conttyp; loc; content}) ->
      let u, fvs = of_rank1_typ ~sort:(Base pol) conttyp in
      let cc, cgen = elab_cmd content in
      exists fvs (CDef (CoVar.to_int cont, CoVar.to_string cont, u, cc))
      >>> fun env ->
      Some (Command_execution {
        name; pol; loc;
        cont = cont;
        conttyp = env.u u;
        content = cgen env
      } )
    | None -> CTrue, fun _ -> None


  let elab_prog prog =
    try
      enter ();
      let c,g = elab_prog_items (elab_exec prog.command) prog.declarations in
      leave ();
      c >>> fun env ->
      let command, declarations = g env in
      {prog with declarations; command}
    with
    | InvalidSort sort ->
      Misc.fail_invariant_break ("this sort of type is unsupported and should \
                                  already have been rejected " ^ sort)
    | Constraints_params.Undefined_type_variable (info, loc) ->
      Misc.fail_invariant_break ~loc ("Scoping was broken: " ^ info)
    | Constraints_params.Unsupported_type_inference (info, loc) ->
      raise (Type_error (info, Some loc))
    | SortConflict (thing, sort, expected_sort) ->
      let info = Printf.sprintf
                   "the type %s has sort %s, but was used with sort %s"
                   thing sort expected_sort in
      raise (Type_error (info, None))
    | Cycle u -> (*TODO*)
      let info = "Found a cyclic type with identifier " ^ string_of_int u in
      raise (Type_error (info, None))
    | UnboundUVar u ->
      Misc.fail_invariant_break ("unbound unification variable: " ^ string_of_int u)
    | UnboundNVar n ->
      Misc.fail_invariant_break ("unbound instanciation placeholder: " ^ string_of_int n)

  let go ~trace:trace prog =
    solve ~trace elab_prog prog

end
