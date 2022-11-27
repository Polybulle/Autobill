open Ast
open FullAst
open Vars
open Types
open Constructors
open Prelude


module Make () = struct

  let vars = ref (Var.Env.empty)

  let covars = ref (CoVar.Env.empty)

  let bind_var v = vars := Var.Env.add v MulZero !vars

  let bind_covar a = covars := CoVar.Env.add a MulZero !covars

  let annot_var v info =
    vars := Var.Env.update v (update info) !vars

  let annot_covar a info =
    covars := CoVar.Env.update a (update info) !covars

  let use_var v = annot_var v MulOne

  let use_covar a = annot_covar a MulOne

  let gocons c k = match c with
    | Unit -> ()
    | Thunk x | Inj (_, _, x) ->  k x
    | Tupple xs | PosCons (_, xs) -> List.iter k xs

  let godestr d kx ka = match d with
    | Closure a | Proj (_, _, a) -> ka a
    | Call (xs, a) | NegCons (_, xs, a) -> List.iter kx xs; ka a


  let rec goval (MetaVal v) = gopreval v.node

  and gostk (MetaStack s) = goprestk s.node

  and gocmd (Command c) = goval c.valu; gostk c.stk

  and gopreval = function
    | Var v -> use_var v
    | CoTop -> ()
    | Bindcc {bind=(a,_); cmd; _}
    | Box {bind=(a,_); cmd; _} -> bind_covar a; gocmd cmd
    | Fix {self=(x,_); cont=(a,_); cmd} ->
      bind_var x; annot_var x MulMany;
      bind_covar a;
      gocmd cmd
    | Cons c -> gocons c goval
    | Pack (_, _, v) -> goval v
    | Spec {bind=(a,_); cmd; _} -> bind_covar a; gocmd cmd
    | Destr patts -> patts |> List.iter (fun (patt, cmd) ->
        godestr patt (fun (x,_) -> bind_var x) (fun (a,_) -> bind_covar a);
        gocmd cmd
      )

  and goprestk = function
    | Ret a -> use_covar a
    | CoZero -> ()
    | CoBind {bind=(x,_); cmd; _} -> bind_var x; gocmd cmd
    | CoBox {stk;_} -> gostk stk
    | CoFix stk -> gostk stk
    | CoPack {bind=(x,_); cmd; _} -> bind_var x; gocmd cmd
    | CoSpec (_, _, stk) -> gostk stk
    | CoDestr d -> godestr d goval gostk
    | CoCons patts -> patts |> List.iter (fun (patt,cmd) ->
        gocons patt (fun (x,_) -> bind_var x);
        gocmd cmd
      )

  let goitem = function
    | Value_declaration {name;_} -> bind_var name
    | Value_definition {name; content;_} -> goval content; bind_var name
    | Command_execution {content; _} -> gocmd content

  let goprog items =
    List.iter goitem items;
    !vars, !covars

end

let infer_multiplicities (pre,prog) =
  let open Make () in
  let varmul, covarmul = goprog prog in
  pre := {!pre with var_multiplicities = varmul; covar_multiplicities = covarmul};
  (pre, prog)
