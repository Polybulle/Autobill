open Intern_prelude
open Vars

let get_pol original_env var =

  let env = ref original_env.polarities in
  let set_pol pol var =
    env := PolVarEnv.add var pol !env in
  let set_pol_all pol vars =
    List.iter (set_pol pol) vars in

  let rec go acc var =
    match PolVarEnv.find_opt var !env with
    | None -> (None, Redirect var, acc)
    | Some (Litteral p) -> (Some p, Litteral p, acc)
    | Some (Redirect newvar) -> go (var::acc) newvar in

  let (ret, updated, tochange) = go [] var in
  set_pol_all updated tochange;
  ret, {original_env with polarities = !env}

let new_pol_var ?init env =
  let var = PolVar.fresh () in
  let env = match init with
    | None ->  env
    | Some p -> {env with polarities = PolVarEnv.add var p env.polarities} in
  var, env
