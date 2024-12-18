open Vars
open Prelude
open Ast.FullAst

type ('a,'b) optim_program = {
    doc : string;
    formula : 'a;
    globals : Vars.TyVar.t list;
    goals : 'b option;
    max_degree : int option;
    constructors : tycons_definition TyConsVar.Env.t
  }

let optim_program_of_prog (prog, post) =
  let goal, deg = match prog.goal with
  | None -> None, None
  | Some (Goal goal) -> Some goal.polynomial, Some goal.degree in
  {
    doc = "";
    formula = post;
    globals = [];
    max_degree = deg;
    goals = goal;
    constructors = let p = !(prog.prelude) in p.tycons
  }

let arity_of_polynomial optim c =
  match TyConsVar.Env.find_opt c optim.constructors with
  | None -> None
  | Some def ->
     let args,_ = Types.unmk_arrow def.sort in
     if not (List.for_all (fun s -> s = Types.Index Primitives.sort_nat) args)
     then None
     else Some (List.length args)
