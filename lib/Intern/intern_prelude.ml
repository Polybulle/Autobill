open Vars
open Types
open Constructors
open Prelude
open Intern_common


let internalize_all_sortvar env prog =
  let go env = function
    | Cst.Sort_declaration {name; _} ->
      {env with sort_vars = StringEnv.add name (SortVar.of_string name) env.sort_vars}
    | _ -> env in
  List.fold_left go env prog


let internalize_all_typcons env prog =

  let internalize_one_typecons = function
    | Cst.Type_definition {name; args; sort; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, sort), loc)
    | Cst.Data_definition {name; args; loc; _}
    | Cst.Pack_definition {name; args; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, Base Positive), loc)
    | Cst.Codata_definition {name; args; loc; _}
    | Cst.Spec_definition {name; args; loc; _} ->
      let args = List.map snd args in
      Some (name, (args, Base Negative), loc)
    | Cst.Type_declaration {name; sort; loc; _} -> Some (name, ([], sort), loc)
    | _ -> None in

  let go env item  =
    match internalize_one_typecons item with
    | None -> env
    | Some (cons, (args, ret_sort), loc) ->
      let args = List.map (intern_sort env) args in
      let ret_sort = intern_sort env ret_sort in
      let sort = sort_arrow args ret_sort in
      if List.mem cons type_cons_names then
        fail_double_def ("type constructor " ^ cons) loc
      else
        match StringEnv.find_opt cons env.tycons_vars with
        | Some _ -> fail_double_def ("type constructor " ^ cons) loc
        | None ->
          let real_cons = (TyConsVar.of_string cons) in
          {env with
           tycons_vars = StringEnv.add cons real_cons env.tycons_vars;
           tycons_sort = TyConsVar.Env.add real_cons sort env.tycons_sort;
          } in

  List.fold_left go env prog


let rec sort_check_type loc env expected_sort (typ : InternAst.typ) =
  let aux typ sort =
    if sort <> expected_sort then
      fail_bad_sort (Misc.string_of_position loc) sort expected_sort
    else
      typ in

  match typ with
  | TBox {kind;node;loc} ->
    TBox {kind;node=aux (sort_check_type loc env sort_negtype node) sort_postype ;loc}
  | TFix t -> TFix (aux (sort_check_type loc env sort_negtype t) sort_negtype)
  | TPos t -> aux (sort_check_type loc env sort_postype t) sort_postype
  | TNeg t -> aux (sort_check_type loc env sort_negtype t) sort_negtype
  | TApp _ | TCons _ | TVar _ | TInternal _ ->
    let typ, sort = sort_infer_type loc env typ in
    aux typ sort

and sort_infer_type loc env typ = match typ with

  | TVar {node; loc} ->
     begin try
        TVar {node = node; loc}, TyVar.Env.find node env.prelude_typevar_sort
      with
        Not_found -> fail_undefined_type (TyVar.to_string node) loc
    end
  | TInternal var -> sort_infer_type loc env (TVar {node = var; loc = Misc.dummy_pos})

  | TCons {node; loc} ->
    let rec arr n arg ret =
      if n = 0 then ret else arr (n-1) arg (Arrow (arg, ret)) in
    let aux sort output = (TCons {node=output;loc}, sort) in
    begin match node with
      | Unit -> aux sort_postype Types.Unit
      | Zero -> aux sort_postype Zero
      | Top -> aux sort_negtype Top
      | Bottom -> aux sort_negtype Bottom
      | Thunk -> aux (arr 1 sort_postype sort_negtype) Types.Thunk
      | Closure -> aux (arr 1 sort_negtype sort_postype) Types.Closure
      | Prod n -> aux (arr n sort_postype sort_postype) (Prod n)
      | Sum n -> aux (arr n sort_postype sort_postype) (Sum n)
      | Fun n ->
        aux (arr 1 sort_negtype (arr n sort_postype sort_negtype)) (Fun n)
      | Choice n -> aux (arr n sort_negtype sort_negtype) (Choice n)
      | Cons cons ->
        let sort =
          try TyConsVar.Env.find cons env.tycons_sort
          with _ -> fail_undefined_type (TyConsVar.to_string cons) loc in
        aux sort (Cons cons)
    end

  | TApp {tfun; args; loc} ->
    let tfun, sort = sort_infer_type loc env tfun in
    let go sort arg = match sort with
      | Arrow (sort, sort') -> sort', sort_check_type loc env sort arg
      | _ -> fail_bad_arity "HACK" loc in
    let ret, args = List.fold_left_map go sort args in
    TApp {tfun; args; loc}, ret

  | TBox {node; kind; loc} ->
    TBox {node = sort_check_type loc env sort_postype node; kind; loc}, sort_postype
  | TFix t -> TFix (sort_check_type loc env sort_negtype t), sort_postype
  | TPos t -> sort_check_type loc env sort_postype t, sort_postype
  | TNeg t -> sort_check_type loc env sort_postype t, sort_postype


let sort_check_tycons_args env scope ?priv:(private_typ=[]) args  =
    let scope, new_args = List.fold_left_map
        (fun scope (x,s) ->
           let scope = add_tyvar scope x in
           (scope, (get_tyvar scope x, intern_sort env s)))
        scope
        args in
    let scope, new_private = List.fold_left_map
        (fun scope (x,s) ->
           let scope = add_tyvar scope x in
           (scope, (get_tyvar scope x, intern_sort env s)))
        scope
        private_typ in
    let env = List.fold_left
        (fun env (tyvar,sort) ->
           {env with prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
        env
        (new_private @ new_args) in
    env, scope, new_private, new_args



let sort_check_one_item env item =

  let scope = empty_scope in

  match item with

  | Cst.Type_declaration {name; sort; loc} ->
    let name = StringEnv.find name env.tycons_vars in
    let typdef = {
      sort = intern_sort env sort;
      loc = loc;
      args = [];
      content = Declared} in
    env.prelude := { !(env.prelude) with
                     tycons = TyConsVar.Env.add name typdef !(env.prelude).tycons};
    env

  | Cst.Type_definition {name; args; content; loc; sort} ->

    let sort = intern_sort env sort in
    let name = StringEnv.find name env.tycons_vars in
    let new_env, new_scope, _, new_args = sort_check_tycons_args env scope args in
    let typdef = {
      sort = TyConsVar.Env.find name env.tycons_sort;
      loc = loc;
      args = new_args;
      content = Defined (sort_check_type loc new_env sort
                           (intern_type new_env new_scope content))} in
    env.prelude :=
      {!(env.prelude) with tycons = TyConsVar.Env.add name typdef !(env.prelude).tycons};
    env

  | Cst.Data_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_env, new_scope, _, new_args = sort_check_tycons_args env scope args in
    let go_one = function
      | PosCons (cons, typs) ->
        if StringEnv.mem cons env.conses then
          fail_double_def ("constructor " ^ cons) loc;
        let typs = List.map
            (fun typ ->
               sort_check_type loc new_env sort_postype
                 (intern_type new_env new_scope typ))
            typs in
        let new_cons = ConsVar.of_string cons in
        let new_content = PosCons (new_cons, typs) in
        let cons_def = Consdef {
            typ_args = new_args;
            val_args = typs;
            private_typs = [];
            resulting_type =
              typecons new_name (List.map (fun (x,_) -> tvar x) new_args)} in
        (cons, new_cons, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let consdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let conses = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = sort_arrow (List.map snd new_args) sort_postype in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      conses = StringEnv.add_seq (List.to_seq conses) env.conses } in
    let tyconsdef = {
      sort;
      loc;
      args = new_args;
      content = Data (List.map (fun (_,_,_,x) -> x) res)} in
    env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      cons = ConsVar.Env.add_seq (List.to_seq consdefs) !(env.prelude).cons};
    env

| Cst.Codata_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_env, new_scope, _, new_args = sort_check_tycons_args env scope args in
    let go_one = function
      | NegCons (destr, typs, conttyp) ->
        if StringEnv.mem destr env.destrs then
          fail_double_def ("destructor" ^ destr) loc;
        let typs = List.map
            (fun typ -> sort_check_type loc new_env sort_postype
                (intern_type new_env new_scope typ))
             typs in
        let conttyp = intern_type env scope conttyp in
        let conttyp = sort_check_type loc new_env sort_negtype conttyp in
        let new_destr = DestrVar.of_string destr in
        let new_content = NegCons (new_destr, typs, conttyp) in
        let cons_def = Destrdef {
            typ_args = new_args;
            val_args = typs;
            private_typs = [];
            ret_arg = conttyp;
            resulting_type =
              typecons new_name (List.map (fun (x,_) -> tvar x) new_args);
          } in
        (destr, new_destr, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let destrdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let destrs = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = sort_arrow (List.map snd new_args) (Base Negative) in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      destrs = StringEnv.add_seq (List.to_seq destrs) env.destrs} in
    let tyconsdef = {
      sort;
      loc;
      args = new_args;
      content = Codata (List.map (fun (_,_,_,x) -> x) res)} in
    env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      destr = DestrVar.Env.add_seq (List.to_seq destrdefs) !(env.prelude).destr};
    env


| Pack_definition { name; args; cons; private_typs; arg_typs; loc } ->
  let new_name = StringEnv.find name env.tycons_vars in
    let new_env, new_scope, new_private, new_args =
      sort_check_tycons_args env scope ~priv:private_typs args in
  if StringEnv.mem cons env.conses then
     fail_double_def ("constructor " ^ cons) loc;
   let arg_typs = List.map
       (fun typ -> sort_check_type loc new_env sort_postype
           (intern_type new_env new_scope typ))
       arg_typs in
   let new_cons = ConsVar.of_string cons in
   let cons_def = Consdef {
       typ_args = new_args;
       val_args = arg_typs;
       private_typs = new_private;
       resulting_type =
         typecons new_name (List.map (fun (x,_) -> tvar x) new_args)} in

   let sort = sort_arrow (List.map snd new_args) (Base Positive) in
   let env = {
     env with
     tycons_vars = StringEnv.add name new_name env.tycons_vars;
     tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
     conses = StringEnv.add cons new_cons env.conses } in
   let tyconsdef = {
     sort;
     loc;
     args = new_args;
     content = Pack (new_cons, cons_def)} in
   env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      cons = ConsVar.Env.add new_cons cons_def !(env.prelude).cons};
    env


| Spec_definition { name; args; destr; private_typs; arg_typs; ret_typ; loc } ->

   let new_name = StringEnv.find name env.tycons_vars in
    let env, new_scope, new_private, new_args =
      sort_check_tycons_args env scope ~priv:private_typs args in
      if StringEnv.mem destr env.destrs then
     fail_double_def ("destructor " ^ destr) loc;
   let arg_typs = List.map
       (fun typ -> sort_check_type loc env sort_postype
           (intern_type env new_scope typ))
       arg_typs in
   let ret_typ = sort_check_type loc env sort_negtype
       (intern_type env new_scope ret_typ) in
   let new_destr = DestrVar.of_string destr in
   let destr_def = Destrdef {
       typ_args = new_args;
       val_args = arg_typs;
       ret_arg = ret_typ;
       private_typs = new_private;
       resulting_type =
         typecons new_name (List.map (fun (x,_) -> tvar x) new_args)} in

   let sort = sort_arrow (List.map snd new_args) (Base Negative) in
   let env = {
     env with
     tycons_vars = StringEnv.add name new_name env.tycons_vars;
     tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
     destrs = StringEnv.add destr new_destr env.destrs } in
   let tyconsdef = {
     sort;
     loc;
     args = new_args;
     content = Spec (new_destr, destr_def)} in
   env.prelude := {
      !(env.prelude) with
      tycons = TyConsVar.Env.add new_name tyconsdef !(env.prelude).tycons;
      destr = DestrVar.Env.add new_destr destr_def !(env.prelude).destr};
    env

| _ -> env
