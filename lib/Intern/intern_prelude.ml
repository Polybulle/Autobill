open Util
open Vars
open Types
open Constructors
open Ast
open Intern_common

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

  | Cst.Type_declaration {name; sort; loc; _} ->
    Some (name, ([], sort), loc)


  | _ -> None


let internalize_typcons prog =

  let go (st, env) item  =
    match internalize_one_typecons item with
    | None -> (st, env)
    | Some (cons, sort, loc) ->
      if List.mem cons type_cons_names then
        fail_double_def ("type constructor " ^ cons) loc
      else
        match StringEnv.find_opt cons st with
        | Some _ -> fail_double_def ("type constructor " ^ cons) loc
        | None ->
          let real_cons = (TyConsVar.of_string cons) in
          let st = StringEnv.add cons real_cons st in
          let env = TyConsVar.Env.add real_cons sort env in
          (st, env) in

  List.fold_left go (StringEnv.empty, TyConsVar.Env.empty) prog


let rec sort_check_type env expected_sort (typ : InternAst.typ) =
  match typ with
  | TVar {node; loc} ->
    begin
        try
          let actual_sort = TyVar.Env.find node env.prelude_typevar_sort in
          if expected_sort <> actual_sort then
            fail_bad_sort loc expected_sort actual_sort
          else
            TVar {node = node; loc}
        with
          Not_found -> fail_undefined_type (TyVar.to_string node) loc
    end

  | TCons {node; loc} ->
      let aux sort output =
        if expected_sort <> sort then
          fail_bad_sort loc expected_sort sort
        else
          TCons {loc; node = output} in

      begin match node with
        | Unit -> aux sort_postype unit_t
        | Zero -> aux sort_postype zero
        | Top -> aux sort_negtype top
        | Bottom -> aux sort_negtype bottom
        | ShiftPos a ->
          let a = sort_check_type env sort_negtype a in
          aux sort_negtype (shift_pos_t a)
        | ShiftNeg a ->
          let a = sort_check_type env sort_postype a in
          aux sort_negtype (shift_neg_t a)
        | Prod bs ->
          let bs = List.map (sort_check_type env sort_postype) bs in
          aux sort_postype (Prod bs)
        | Sum bs ->
          let bs = List.map (sort_check_type env sort_postype) bs in
          aux sort_postype (Sum bs)
        | Fun (a,b) ->
          let a = List.map (sort_check_type env sort_postype) a in
          let b = sort_check_type env sort_negtype b in
          aux sort_negtype (Fun (a, b))
        | Choice bs ->
          let bs = List.map (sort_check_type env sort_postype) bs in
          aux sort_negtype (Choice bs)
        | Cons (cons, args) ->
          let args_sorts, sort =
            try TyConsVar.Env.find cons env.tycons_sort
            with _ -> fail_undefined_type (TyConsVar.to_string cons) loc in
          if expected_sort <> sort then
            fail_bad_sort loc expected_sort sort;
          let args = try
              List.map2 (sort_check_type env) args_sorts args
            with
              Invalid_argument _ -> fail_bad_arity (TyConsVar.to_string cons) loc in
          TCons {node = Cons (cons, args); loc}
      end

  | TInternal var -> sort_check_type env expected_sort (TVar {node = var; loc = dummy_pos})

  | TPos t -> sort_check_type env sort_postype t

  | TNeg t -> sort_check_type env sort_negtype t

  | TBox {node; _} -> sort_check_type env sort_postype node

  | TFix t ->
    if expected_sort <> sort_negtype then
      fail_bad_sort dummy_pos expected_sort sort_negtype
    else
      TFix (sort_check_type env sort_negtype t)





let sort_check_one_item (prelude, env) item =

  match item with

  | Cst.Type_declaration {name; sort; loc} ->

    let typdef = {
      full_sort = [], sort;
      ret_sort = sort;
      loc = loc;
      args = [];
      content = Declared} in
    let name = StringEnv.find name env.tycons_vars in
    let prelude = {prelude with tycons = TyConsVar.Env.add name typdef prelude.tycons} in
    (prelude, env)

  | Cst.Type_definition {name; args; content; loc; sort} ->

    let name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left
        (fun env (str,tyvar,sort) ->
           {env with
            type_vars = StringEnv.add str tyvar env.type_vars;
            prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
        env
        new_args in
    let ret_sort = TyConsVar.Env.find name env.tycons_sort in
    let typdef = {
      full_sort = ret_sort;
      ret_sort = snd ret_sort;
      loc = loc;
      args = List.map (fun (_,a,b) -> (a,b)) new_args;
      content =Defined (sort_check_type inner_env sort (intern_type inner_env content))} in
    let prelude = {prelude with tycons = TyConsVar.Env.add name typdef prelude.tycons} in
    (prelude, env)


  | Cst.Data_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left
        (fun env (str,tyvar,sort) ->
           {env with
            type_vars = StringEnv.add str tyvar env.type_vars;
            prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
        env
        new_args in
    let go_one = function
      | PosCons (cons, typs) ->
        if StringEnv.mem cons env.conses then
          fail_double_def ("constructor " ^ cons) loc;
        let typs = List.map
            (fun typ -> sort_check_type inner_env sort_postype (intern_type inner_env typ))
            typs in
        let new_cons = ConsVar.of_string cons in
        let new_content = PosCons (new_cons, typs) in
        let cons_def = Consdef {
            typ_args = List.map (fun (_,x,so) -> (x, so)) new_args;
            val_args = typs;
            private_typs = [];
            resulting_type =
              Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args))} in
        (cons, new_cons, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let consdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let conses = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = (List.map snd args, Base Positive) in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      conses = StringEnv.add_seq (List.to_seq conses) env.conses } in
    let tyconsdef = {
      full_sort = (List.map snd args), Base Positive;
      ret_sort = sort_postype;
      loc = loc;
      args = List.map (fun (_,x,s) -> (x,s)) new_args;
      content = Data (List.map (fun (_,_,_,x) -> x) res)} in
    let prelude = {
      prelude with
      tycons = TyConsVar.Env.add new_name tyconsdef prelude.tycons;
      cons = ConsVar.Env.add_seq (List.to_seq consdefs) prelude.cons} in
    (prelude, env)

| Cst.Codata_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left (fun env (str,tyvar,sort) ->
        {env with
         type_vars = StringEnv.add str tyvar env.type_vars;
         prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
        env new_args in

    let go_one = function
      | NegCons (destr, typs, conttyp) ->
        if StringEnv.mem destr env.destrs then
          fail_double_def ("destructor" ^ destr) loc;
        let typs = List.map
            (fun typ -> sort_check_type inner_env sort_postype (intern_type env typ))
             typs in
        let conttyp = intern_type env conttyp in
        let conttyp = sort_check_type inner_env sort_negtype conttyp in
        let new_destr = DestrVar.of_string destr in
        let new_content = NegCons (new_destr, typs, conttyp) in
        let cons_def = Destrdef {
            typ_args = List.map (fun (_,x,so) -> (x, so)) new_args;
            val_args = typs;
            private_typs = [];
            ret_arg = conttyp;
            resulting_type =
              Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args));
          } in
        (destr, new_destr, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let destrdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let destrs = List.map (fun (x,y,_,_) -> (x,y)) res in
    let sort = List.map snd args, Base Negative in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
      destrs = StringEnv.add_seq (List.to_seq destrs) env.destrs} in
    let tyconsdef = {
      full_sort = (List.map snd args), Base Negative;
      ret_sort = sort_postype;
      loc = loc;
      args = List.map (fun (_,x,s) -> (x,s)) new_args;
      content = Codata (List.map (fun (_,_,_,x) -> x) res)} in
    let prelude = {
      prelude with
      tycons = TyConsVar.Env.add new_name tyconsdef prelude.tycons;
      destr = DestrVar.Env.add_seq (List.to_seq destrdefs) prelude.destr} in
    (prelude, env)

| Pack_definition { name; args; cons; private_typs; arg_typs; loc } ->

   let new_name = StringEnv.find name env.tycons_vars in
   let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
   let new_private = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) private_typs in
   let inner_env = List.fold_left
       (fun env (str,tyvar,sort) ->
          {env with
           type_vars = StringEnv.add str tyvar env.type_vars;
           prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
       env
       (new_args @ new_private) in

   if StringEnv.mem cons env.conses then
     fail_double_def ("constructor " ^ cons) loc;
   let arg_typs = List.map
       (fun typ -> sort_check_type inner_env sort_postype (intern_type inner_env typ))
       arg_typs in
   let new_cons = ConsVar.of_string cons in
   let cons_def = Consdef {
       typ_args = List.map (fun (_,x,so) -> (x, so)) new_args;
       val_args = arg_typs;
       private_typs = List.map (fun (_,x,so) -> (x, so)) new_private;
       resulting_type =
         Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args))} in

   let sort = (List.map snd args, Base Positive) in
   let env = {
     env with
     tycons_vars = StringEnv.add name new_name env.tycons_vars;
     tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
     conses = StringEnv.add cons new_cons env.conses } in
   let tyconsdef = {
     full_sort = (List.map snd args), Base Positive;
     ret_sort = sort_postype;
     loc = loc;
     args = List.map (fun (_,x,s) -> (x,s)) new_args;
     content = Pack (new_cons, cons_def)} in
   let prelude = {
     prelude with
     tycons = TyConsVar.Env.add new_name tyconsdef prelude.tycons;
     cons = ConsVar.Env.add new_cons cons_def prelude.cons} in
   (prelude, env)


| Spec_definition { name; args; destr; private_typs; arg_typs; ret_typ; loc } ->

   let new_name = StringEnv.find name env.tycons_vars in
   let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
   let new_private = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) private_typs in
   let inner_env = List.fold_left
       (fun env (str,tyvar,sort) ->
          {env with
           type_vars = StringEnv.add str tyvar env.type_vars;
           prelude_typevar_sort = TyVar.Env.add tyvar sort env.prelude_typevar_sort})
       env
       (new_args @ new_private) in

   if StringEnv.mem destr env.destrs then
     fail_double_def ("destructor " ^ destr) loc;
   let arg_typs = List.map
       (fun typ -> sort_check_type inner_env sort_postype (intern_type inner_env typ))
       arg_typs in
   let ret_typ = sort_check_type inner_env sort_negtype (intern_type inner_env ret_typ) in
   let new_destr = DestrVar.of_string destr in
   let destr_def = Destrdef {
       typ_args = List.map (fun (_,x,so) -> (x, so)) new_args;
       val_args = arg_typs;
       ret_arg = ret_typ;
       private_typs = List.map (fun (_,x,so) -> (x, so)) new_private;
       resulting_type =
         Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args))} in

   let sort = (List.map snd args, Base Negative) in
   let env = {
     env with
     tycons_vars = StringEnv.add name new_name env.tycons_vars;
     tycons_sort = TyConsVar.Env.add new_name sort env.tycons_sort;
     destrs = StringEnv.add destr new_destr env.conses } in
   let tyconsdef = {
     full_sort = (List.map snd args), Base Negative;
     ret_sort = sort_negtype;
     loc = loc;
     args = List.map (fun (_,x,s) -> (x,s)) new_args;
     content = Spec (new_destr, destr_def)} in
   let prelude = {
     prelude with
     tycons = TyConsVar.Env.add new_name tyconsdef prelude.tycons;
     destr = DestrVar.Env.add new_destr destr_def prelude.destr} in
   (prelude, env)


| _ -> (prelude, env)
