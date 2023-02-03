open Lcbpv
open Cst
open Types
open Constructors

let export_cons = _

let export_method = _

let export_box_kind = function
  | Lin -> Types.Linear
  | Exp -> Types.Exponential

let mk_var s =
  let v = Global_counter.fresh_int () in
  s ^ "_" ^ string_of_int v

let mk_vars n s = List.init n (fun _ -> mk_var s)

let rec go (e : Lcbpv.expression) = match e with

| Expr_Var x -> V.var x

| Expr_Int n -> V.cons (Int n)

| Expr_Constructor (c, args) ->
  let c = export_cons c in
  let a = mk_var "a" in
  let vars = mk_vars (List.length args) "x" in
  let cmd = V.cons (poscons c [] (List.map V.var vars)) |+| S.ret a in
  let aux cmd arg var = (go arg) |+| S.bind var None cmd in
  let cmd = List.fold_left2 aux cmd args vars in
  V.bindcc a None cmd

| Expr_Method (e, m, args) ->
  let m = export_cons m in
  let a = mk_var "a" in
  let vars = mk_vars (List.length args) "x" in
  let y = mk_var "y" in
  let cmd = V.var y |-| S.destr (negcons m [] (List.map V.var vars) (S.ret a)) in
  let aux cmd arg var = (go arg) |+| S.bind var None cmd in
  let cmd = List.fold_left2 aux cmd args vars in
  let cmd = (go e) |-| S.bind y None cmd in
  V.bindcc a None cmd

| Expr_Closure (q, e) ->
  let a = mk_var "a" in
  V.box (export_box_kind q) a None (go e |-| S.ret a)

| Expr_Thunk e -> V.cons (thunk (go e))

| Expr_Get _ -> _

| Expr_Match (_, _) -> _

| Expr_Absurd (_, _) -> _

| Expr_Rec (_, _) -> _

| Expr_Block _ -> _

| Expr_Bin_Prim (_, _, _) -> _

| Expr_Mon_Prim (_, _) -> _

| Expr_If (_, _, _) -> _
