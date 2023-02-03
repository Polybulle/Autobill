open Lcbpv
open Cst
open Types
open Constructors


let export_box_kind = function
  | Lin -> Types.Linear
  | Exp -> Types.Exponential

let mk_var s =
  let v = Global_counter.fresh_int () in
  s ^ "_" ^ string_of_int v

let mk_vars n s = List.init n (fun _ -> mk_var s)


let rec eval_then e cont =
  let a = mk_var "a" in
  let x = mk_var "x" in
  V.bindcc a None (go e |~| S.bind x None (cont x a))

and eval_many_them es cont =
  let a = mk_var "a" in
  let xs = mk_vars (List.length es) "x" in
  let aux cmd arg var = (go arg) |+| S.bind var None cmd in
  let cmd = List.fold_left2 aux (cont xs a) es xs in
  V.bindcc a None cmd

and go (e : Lcbpv.expression) = match e with

  | Expr_Var x -> V.var x

  | Expr_Int n -> V.cons (Int n)

  | Expr_Constructor (c, args) -> go_cons c args

  | Expr_Method (e, m, args) ->
    let a = mk_var "a" in
    V.bindcc a None (go_method e m args a)

  | Expr_Closure (q, e) ->
    let a = mk_var "a" in
    V.box (export_box_kind q) a None (go e |-| S.ret a)

  | Expr_Thunk e -> V.cons (thunk (go e))

  | Expr_Get cases ->
    V.case (List.map (fun (GetPat (m, xs, cmd)) -> go_method_patt m xs cmd) cases)

  | Expr_Match (e, cases) ->
    let a = mk_var "a" in
    V.bindcc a None ((go e) |+| (S.case (List.map
                          (fun (MatchPat (c, xs , e)) -> go_cons_patt c xs e a)
                          cases)))

  | Expr_Rec (x, e) ->
    let a = mk_var "a" in
    let b = mk_var "b" in
    V.bindcc a None (V.fix (x, None) (a, None) (go e |-| S.ret b) |-| S.cofix (S.ret a))

  | Expr_Block (Blk (instrs, ret)) ->
    let a = mk_var "a" in
    V.bindcc a None (go_block instrs ret a)

  | Expr_Bin_Prim (op, a, b) ->
    let call x y a = (go_bin_primop op |-| S.destr (call [V.var x; V.var y] (S.ret a))) in
    eval_then a (fun x a -> eval_then b (fun y b -> call x y b) |~| S.ret a)

  | Expr_Mon_Prim (op, e) ->
    eval_then e (fun x a -> go_mon_primop op |-| S.destr (call [V.var x] (S.ret a)))

  | Expr_If (b, e1, e2) ->
    go (Expr_Match (b, [MatchPat (True,[],e1); MatchPat (False,[],e2)]))


and go_cons c es = _

and go_method e m es a = _

and go_method_patt m xs e = _

and go_cons_patt c xs e a = _

and go_block instrs ret a = _

and go_bin_primop op = _

and go_mon_primop op = _
