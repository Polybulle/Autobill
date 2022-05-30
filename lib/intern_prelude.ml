open Util
open Vars
open Types
open Constructors
open Ast

exception Double_definition of string

exception Bad_sort of {
    loc : position;
    actual : sort;
    expected : sort;
  }

exception Undefined_type of {
    name : string;
    loc : position;
  }

exception Bad_type_cons_arity of {
    cons : string;
    loc : position;
  }

exception Bad_constructor_name of {
    loc : position
  }

exception Higher_order_type_argument of {
    loc : position;
    name : string
  }

let fail_double_def mess loc =
  raise (Double_definition
           (Printf.sprintf "%s: FATAL the %s is already defined"
              (Util.string_of_position loc)
              mess))

let fail_bad_sort loc expected actual =
  raise (Bad_sort {loc; actual; expected})

let fail_undefined_type name loc =
  raise (Undefined_type {name; loc})

let fail_bad_arity cons loc =
  raise (Bad_type_cons_arity {cons; loc})

let fail_bad_constructor loc =
  raise (Bad_constructor_name {loc})

let fail_higher_order_arg name loc =
  raise (Higher_order_type_argument {name; loc})

module StringEnv = Map.Make (struct
    type t = string
    let compare = compare
  end)

module TyVarEnv = Map.Make (struct
    type t = TyVar.t
    let compare = compare
  end)

type sort_check_env = {
  tycons_vars : TyConsVar.t StringEnv.t;
  tycons_sort : sort TyConsEnv.t;
  type_vars : TyVar.t StringEnv.t;
  typevar_sort : sort TyVarEnv.t;
  conses : ConsVar.t StringEnv.t;
  destrs : DestrVar.t StringEnv.t;
}

let empty_sortcheck = {
  tycons_vars = StringEnv.empty;
  tycons_sort = TyConsEnv.empty;
  type_vars = StringEnv.empty;
  typevar_sort = TyVarEnv.empty;
  conses = StringEnv.empty;
  destrs = StringEnv.empty;
}

let internalize_one_typecons = function

  | Cst.Type_definition {name; args; sort; loc; _} ->
    let args = List.rev (List.map snd args) in
    let sort = List.fold_left (fun so arg -> sort_dep arg so)  sort args in
    Some (name, sort, loc)

  | Cst.Data_definition {name; args; loc; _} ->
    let args = List.rev (List.map snd args) in
    let sort = List.fold_left (fun so arg -> sort_dep arg so) sort_postype args in
    Some (name, sort, loc)

  | Cst.Codata_definition {name; args; loc; _} ->
    let args = List.rev (List.map snd args) in
    let sort = List.fold_left (fun so arg -> sort_dep arg so) sort_negtype args in
    Some (name, sort, loc)

  | Cst.Type_declaration {name; sort; loc; _} ->
    (* The syntax allows for defining types with higher-order arguments, *)
    (* but the type inference engine doesn't support them. So we reject *)
    (* types with higher-order arguments *)
    let rec no_higher_order_arg = function
      | Dep (Dep (_,_), _) -> fail_higher_order_arg name loc
      | Dep (Base _, x) -> no_higher_order_arg x
      | Base _ -> () in
    no_higher_order_arg sort;
    Some (name, sort, loc)

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
          let env = TyConsEnv.add real_cons sort env in
          (st, env) in

  List.fold_left go (StringEnv.empty, TyConsEnv.empty) prog


let rec sort_check_type env expected_sort = function

  | TVar {node; loc} ->
    begin
      try
        let name = StringEnv.find node env.tycons_vars in
        let actual_sort = TyConsEnv.find name env.tycons_sort in
        match actual_sort with
        | Base _ when expected_sort = actual_sort ->
          TCons {node = Cons (name, []); loc}
        | _ ->
            fail_bad_sort loc expected_sort actual_sort
      with
        Not_found ->
        try
          let name = StringEnv.find node env.type_vars in
          let actual_sort = TyVarEnv.find name env.typevar_sort in
          if expected_sort <> actual_sort then
            fail_bad_sort loc expected_sort actual_sort
          else
            TVar {node = name; loc}
        with
          Not_found -> fail_undefined_type node loc
    end

  | TCons {node; loc} ->
      let aux sort output =
        if expected_sort <> sort then
          fail_bad_sort loc expected_sort sort
        else
          TCons {loc; node = output} in

      begin match node with
        | Unit ->
          aux sort_postype unit_t
        | Zero -> aux sort_postype zero
        | Top -> aux sort_negtype top
        | Bottom -> aux sort_negtype bottom
        | ShiftPos a ->
          let a = sort_check_type env sort_negtype a in
          aux sort_negtype (shift_pos_t a)
        | ShiftNeg a ->
          let a = sort_check_type env sort_postype a in
          aux sort_negtype (shift_neg_t a)
        | Prod (a,b) ->
          let a = sort_check_type env sort_postype a in
          let b = sort_check_type env sort_postype b in
          aux sort_postype (prod a b)
        | Sum (a,b) ->
          let a = sort_check_type env sort_postype a in
          let b = sort_check_type env sort_postype b in
          aux sort_postype (sum a b)
        | Fun (a,b) ->
          let a = sort_check_type env sort_postype a in
          let b = sort_check_type env sort_negtype b in
          aux sort_negtype (func a b)
        | Choice (a,b) ->
          let a = sort_check_type env sort_negtype a in
          let b = sort_check_type env sort_negtype b in
          aux sort_negtype (choice a b)
        | Cons (cons, args) ->
          let (name, sort) =
            try let name = StringEnv.find cons env.tycons_vars in
              let sort = TyConsEnv.find name env.tycons_sort in
              (name, sort)
            with _ -> fail_undefined_type cons loc in
          let rec go = function
            | [], actual_sort ->
              if expected_sort <> actual_sort then
                fail_bad_sort loc expected_sort actual_sort
              else []
            | h::t, Dep(hsort,tsort) ->
              let h = sort_check_type env hsort h in
              let t = go (t, tsort) in
              h :: t
            | _ -> fail_bad_arity cons loc in
          TCons {node = Cons (name, go (args,sort)); loc}
      end

  | TInternal var -> sort_check_type env expected_sort (TVar {node = var; loc = dummy_pos})

  | TPos t -> sort_check_type env sort_postype t

  | TNeg t -> sort_check_type env sort_negtype t

  | TBox {node; _} -> sort_check_type env sort_postype node



let sort_check_one_item (prelude, env) item =

  match item with

  | Cst.Type_declaration {name; sort; loc} ->

    let typdef = {
      sort = sort;
      loc = loc;
      args = [];
      content = Declared} in
    let name = StringEnv.find name env.tycons_vars in
    let prelude = {prelude with tycons = TyConsEnv.add name typdef prelude.tycons} in
    (prelude, env)

  | Cst.Type_definition {name; args; content; loc; sort} ->

    let name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left
        (fun env (str,tyvar,sort) ->
           {env with type_vars = StringEnv.add str tyvar env.type_vars;
                     typevar_sort = TyVarEnv.add tyvar sort env.typevar_sort})
        env
        new_args in
    let typdef = {
      sort = TyConsEnv.find name env.tycons_sort;
      loc = loc;
      args = List.map (fun (_,a,b) -> (a,b)) new_args;
      content = Defined (sort_check_type inner_env sort content)} in
    let prelude = {prelude with tycons = TyConsEnv.add name typdef prelude.tycons} in
    (prelude, env)


  | Cst.Data_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left
        (fun env (str,tyvar,sort) ->
           {env with type_vars = StringEnv.add str tyvar env.type_vars;
                     typevar_sort = TyVarEnv.add tyvar sort env.typevar_sort})
        env
        new_args in
    let go_one = function
      | PosCons (cons, typs) ->
        if StringEnv.mem cons env.conses then
          fail_double_def ("constructor" ^ cons) loc;
        let typs = List.map (sort_check_type inner_env sort_postype) typs in
        let new_cons = ConsVar.of_string cons in
        let new_content = PosCons (new_cons, typs) in
        let cons_def = Consdef {
            args = List.map (fun (_,x,so) -> (x, so)) new_args;
            content = new_content;
            resulting_type =
              Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args))} in
        (cons, new_cons, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let consdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let conses = List.map (fun (x,y,_,_) -> (x,y)) res in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsEnv.add new_name sort_postype env.tycons_sort;
      conses = StringEnv.add_seq (List.to_seq conses) env.conses } in
    let tyconsdef = {
      sort = sort_postype;
      loc = loc;
      args = List.map (fun (_,x,s) -> (x,s)) new_args;
      content = Data (List.map (fun (_,_,_,x) -> x) res)} in
    let prelude = {
      prelude with
      tycons = TyConsEnv.add new_name tyconsdef prelude.tycons;
      cons = ConsEnv.add_seq (List.to_seq consdefs) prelude.cons} in
    (prelude, env)

| Cst.Codata_definition {name; args; content; loc} ->

    let new_name = StringEnv.find name env.tycons_vars in
    let new_args = List.map (fun (x,s) -> (x,TyVar.of_string x,s)) args in
    let inner_env = List.fold_left (fun env (str,tyvar,sort) ->
        {env with type_vars = StringEnv.add str tyvar env.type_vars;
                  typevar_sort = TyVarEnv.add tyvar sort env.typevar_sort})
        env new_args in

    let go_one = function
      | NegCons (destr, typs, conttyp) ->
        if StringEnv.mem destr env.destrs then
          fail_double_def ("destructor" ^ destr) loc;
        let typs = List.map (sort_check_type inner_env sort_negtype) typs in
        let conttyp = sort_check_type inner_env sort_negtype conttyp in
        let new_destr = DestrVar.of_string destr in
        let new_content = NegCons (new_destr, typs, conttyp) in
        let cons_def = Destrdef {
            args = List.map (fun (_,x,so) -> (x, so)) new_args;
            content = new_content;
            resulting_type =
              Types.cons (typecons new_name (List.map (fun (_,x,_) -> tvar x) new_args));
          } in
        (destr, new_destr, cons_def, new_content)
      | _ -> fail_bad_constructor loc in

    let res = List.map go_one content in
    let destrdefs = List.map (fun (_,x,d,_) -> (x,d)) res in
    let destrs = List.map (fun (x,y,_,_) -> (x,y)) res in
    let env = {
      env with
      tycons_vars = StringEnv.add name new_name env.tycons_vars;
      tycons_sort = TyConsEnv.add new_name sort_postype env.tycons_sort;
      destrs = StringEnv.add_seq (List.to_seq destrs) env.destrs} in
    let tyconsdef = {
      sort = sort_postype;
      loc = loc;
      args = List.map (fun (_,x,s) -> (x,s)) new_args;
      content = Codata (List.map (fun (_,_,_,x) -> x) res)} in
    let prelude = {
      prelude with
      tycons = TyConsEnv.add new_name tyconsdef prelude.tycons;
      destr = DestrEnv.add_seq (List.to_seq destrdefs) prelude.destr} in
    (prelude, env)

| _ -> (prelude, env)

let internalize_prelude prog =
  try
    let tyconses, tycons_sorts = internalize_typcons prog in
    let env = {
      empty_sortcheck with
      tycons_vars = tyconses;
      tycons_sort = tycons_sorts;
    } in
    let (prelude, env) = List.fold_left sort_check_one_item (empty_prelude, env) prog in
    let is_not_prelude = function
      | Cst.Term_definition _ | Cst.Env_definition _ | Cst.Cmd_definition _ -> true
      | _ -> false in
    let prog = List.filter is_not_prelude prog in
    (prog, prelude, env)
  with
  | Bad_sort {loc; actual; expected} ->
    raise (Failure (
        Printf.sprintf "%s: FATAL sort error, wanted %s, got %s"
          (string_of_position loc)
          (string_of_sort expected)
          (string_of_sort actual)))
