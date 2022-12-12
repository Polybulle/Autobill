
open Types
open Vars
open Constructors
open Misc
open FirstOrder.FullFOL


type tycons_definition = {
  sort : sort;
  loc : position;
  args : (TyVar.t * sort) list;
  content : tycons_def_content
}
and tycons_def_content =
  | Declared
  | Defined of typ
  | Data of (ConsVar.t, typ) constructor list
  | Codata of (DestrVar.t, typ, typ) destructor list
  | Pack of ConsVar.t * cons_definition
  | Spec of DestrVar.t * destr_definition

and cons_definition = Consdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  equations : eqn list;
  val_args : typ list;
  resulting_type : typ
}

and destr_definition = Destrdef of {
  typ_args : (TyVar.t * sort) list;
  private_typs : (TyVar.t * sort) list;
  equations : eqn list;
  val_args : typ list;
  ret_arg : typ;
  resulting_type : typ
}


type _prelude = {
  sort_defs : unit SortVar.Env.t;
  relations : sort list RelVar.Env.t;
  tycons : tycons_definition TyConsVar.Env.t;
  cons : cons_definition ConsVar.Env.t;
  destr : destr_definition DestrVar.Env.t;
  vars : typ Var.Env.t;
  covars : typ CoVar.Env.t;
  sorts : sort TyVar.Env.t;
  var_multiplicities : var_multiplicity Var.Env.t;
  covar_multiplicities : var_multiplicity CoVar.Env.t
}

type prelude = _prelude ref

let empty_prelude () = ref {
    sort_defs = SortVar.Env.empty;
    relations = RelVar.Env.empty;
    tycons = TyConsVar.Env.empty;
    cons = ConsVar.Env.empty;
    destr = DestrVar.Env.empty;
    vars = Var.Env.empty;
    covars = CoVar.Env.empty;
    sorts = TyVar.Env.empty;
    var_multiplicities = Var.Env.empty;
    covar_multiplicities = CoVar.Env.empty
  }



let get_env var env =
  match TyVar.Env.find_opt var !env with
  | Some v -> v
  | None ->
    (let w = TyVar.fresh () in
    env := TyVar.Env.add var w !env;
    w)

let rec refresh_typ env typ = match typ with
| TBox b -> TBox {b with node = refresh_typ env b.node}
| TVar {node; loc} -> TVar {node = get_env node env; loc}
| TPos typ -> TPos (refresh_typ env typ)
| TNeg typ -> TNeg (refresh_typ env typ)
| TInternal var -> TInternal (get_env var env)
| TFix t -> TFix (refresh_typ env t)
| TCons _ -> typ
| TApp {loc; tfun; args} ->
  TApp {loc;
       tfun = refresh_typ env tfun;
       args = List.map (refresh_typ env) args}

and refresh_cons env = function
  | PosCons (cons, ts) -> PosCons (cons, List.map (refresh_typ env) ts)
  | _ -> raise (Failure "Internalisation invariant")

and refresh_destr env = function
  | NegCons (cons, ts, t) ->
    NegCons (cons, List.map (refresh_typ env) ts, refresh_typ env t)
  | _ -> raise (Failure "Internalisation invariant")


let add_sorts prelude binds =
  prelude := {!prelude with sorts = List.fold_left
                    (fun sorts (a,so) -> TyVar.Env.add a so sorts)
                    !prelude.sorts
                    binds}


let rec refresh_tycons_def prelude env def =
  let args = List.map (fun (x,so) -> (get_env x env, so)) def.args in
  add_sorts prelude args;
  {def with
   args;
   content = match def.content with
     | Declared -> Declared
     | Defined typ -> Defined (refresh_typ env typ)
     | Data conses -> Data (List.map (refresh_cons env) conses)
     | Codata destrs -> Codata (List.map (refresh_destr env) destrs)
     | Pack (cons, def) -> Pack (cons, refresh_cons_def prelude env def)
     | Spec (destr, def) -> Spec (destr, refresh_destr_def prelude env def)
  }

and refresh_cons_def prelude env
    (Consdef { typ_args; val_args; private_typs; resulting_type; equations }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  add_sorts prelude typ_args;
  add_sorts prelude private_typs;
  let equations = map_eqns (refresh_typ env) equations in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  Consdef {typ_args; val_args; resulting_type; private_typs; equations}

and refresh_destr_def prelude env
    (Destrdef { typ_args; val_args; ret_arg; resulting_type; private_typs; equations }) =
  let typ_args = List.map (fun (x,so) -> (get_env x env, so)) typ_args in
  let private_typs = List.map (fun (x,so) -> (get_env x env, so)) private_typs in
  add_sorts prelude typ_args;
  add_sorts prelude private_typs;
  let equations = map_eqns (refresh_typ env) equations in
  let val_args = List.map (refresh_typ env) val_args in
  let resulting_type = refresh_typ env resulting_type in
  let ret_arg = refresh_typ env ret_arg in
  Destrdef {typ_args; val_args; resulting_type; ret_arg; private_typs; equations}

let def_of_cons prelude cons =
  refresh_cons_def prelude (ref TyVar.Env.empty) (ConsVar.Env.find cons !prelude.cons)


let def_of_destr prelude destr =
 refresh_destr_def prelude (ref TyVar.Env.empty) (DestrVar.Env.find destr !prelude.destr)


let def_of_tycons prelude t =
 refresh_tycons_def prelude (ref TyVar.Env.empty) (TyConsVar.Env.find t !prelude.tycons)
