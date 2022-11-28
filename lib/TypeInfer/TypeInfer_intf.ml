open Sexpr
open Vars
open Constructors
open Ast
open FullAst


let constraint_as_string (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let x,_ = elab_prog_items items in
  let s1 = Sexpr.to_string (con_to_sexpr (fun n -> V(string_of_int n)) x) in
  let s2 = Sexpr.to_string (subst_to_sexpr !_state) in
  s1 ^ "\n" ^ s2

let post_contraint_as_string (prelude, _, post) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make(P) in
  let post = FirstOrder.normalize_formula post in
  let s1 = Sexpr.to_string (FirstOrder.formula_to_sexpr (fun n -> V (string_of_int n)) post) in
  let s2 = Sexpr.to_string (subst_to_sexpr !_state) in
  s1 ^ "\n" ^ s2

let fill_out_types items =

  let vars = ref (Var.Env.empty) in

  let covars = ref (CoVar.Env.empty) in

  let bind_var (v,t) = vars := Var.Env.add v t !vars in

  let bind_covar (a,t) = covars := CoVar.Env.add a t !covars in

  let gocons c k = match c with
    | Unit -> ()
    | Thunk x | Inj (_, _, x) ->  k x
    | Tupple xs | PosCons (_, xs) -> List.iter k xs in

  let godestr d kx ka = match d with
    | Closure a | Proj (_, _, a) -> ka a
    | Call (xs, a) | NegCons (_, xs, a) -> List.iter kx xs; ka a in


  let rec goval (MetaVal v) = gopreval v.node

  and gostk (MetaStack s) = goprestk s.node

  and gocmd (Command c) = goval c.valu; gostk c.stk

  and gopreval = function
    | Var _ | CoTop -> ()
    | Bindcc {bind; cmd; _} | Box {bind; cmd; _} ->
      bind_covar bind; gocmd cmd
    | Fix {self=(x,_); cont=(a,t); cmd} ->
      bind_var (x, t);
      bind_covar (a, t);
      gocmd cmd
    | Cons c -> gocons c goval
    | Pack (_, _, v) -> goval v
    | Spec {bind; cmd; _} -> bind_covar bind; gocmd cmd
    | Destr patts -> patts |> List.iter (fun (patt, cmd) ->
        godestr patt bind_var bind_covar;
        gocmd cmd
      )

  and goprestk = function
    | Ret _  | CoZero -> ()
    | CoBind {bind; cmd; _} -> bind_var bind; gocmd cmd
    | CoBox {stk;_} -> gostk stk
    | CoFix stk -> gostk stk
    | CoPack {bind; cmd; _} -> bind_var bind; gocmd cmd
    | CoSpec (_, _, stk) -> gostk stk
    | CoDestr d -> godestr d goval gostk
    | CoCons patts -> patts |> List.iter (fun (patt,cmd) ->
        gocons patt bind_var;
        gocmd cmd
      ) in

  let goitem = function
    | Value_declaration {name; typ; _} -> bind_var (name, typ)
    | Value_definition {name; content; typ; _} -> goval content; bind_var (name, typ)
    | Command_execution {content; _} -> gocmd content

  in
  List.iter goitem items;
  !vars, !covars


let type_infer ~trace:trace (prelude, items) =
  let module P = struct let it = prelude end in
  let open Elaborate.Make (P) in
  let items,post = go ~trace items in
  let vars, covars = fill_out_types items in
  prelude := {!prelude with vars; covars};
  (prelude, items, post)
