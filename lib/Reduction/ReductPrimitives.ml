open Vars
open Ast
open Constructors
open FullAst

exception Not_a_primitive

let fail_not_a_primitive str =
  Misc.fail_invariant_break
    ("During interpretation, failed to reduce as a primitive: " ^ str)

let fail_wrong_args_number str =
  Misc.fail_invariant_break
    ("During interpretation, a primitive call is not valid for primitive: " ^ str)

let fail_parameters_in_primitives str =
  Misc.fail_invariant_break
    ("During interpretation, forbiden type-level paramaters are given for a call to: " ^ str)

let mk_int n = V.cons (cons (Int n) [] [])

let mk_bool b = V.cons (cons (Bool b) [] [])

let process_int_binop v a b =
  match v with
  | "op_add" -> mk_int (a+b)
  | "op_sub" -> mk_int (a-b)
  | "op_mul" -> mk_int (a*b)
  | "op_div" -> mk_int (a/b)
  | "op_mod" -> mk_int (a mod b)
  | op -> fail_not_a_primitive op

let process_int_test v a b =
  match v with
  | "op_eq" -> mk_bool (a=b)
  | "op_lt" -> mk_bool (a<b)
  | "op_leq" -> mk_bool (a<=b)
  | op -> fail_not_a_primitive op

let process_int_monop v a =
  match v with
  | "op_op" -> mk_int (-a)
  | op -> fail_not_a_primitive op

let process_bool_binop v a b =
  match v with
  | "op_and" -> mk_bool (a&&b)
  | "op_or" -> mk_bool (a||b)
  | op -> fail_not_a_primitive op

let process_bool_monop v a =
  match v with
  | "op_neg" -> mk_bool (not a)
  | op -> fail_not_a_primitive op

let process_args env args =
  let rec aux = function
    | MetaVal {node = Var v; _} -> aux (env v)
    | v -> v in
  List.map aux args
 (* primitives are always a single constructor and don't need normalization *)

let process_prim v args =
  let v = Var.to_string v  in
  match v with
  | "op_add" | "op_sub" | "op_mul" | "op_div" | "op_mod" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Int a; _}); _};
         MetaVal {node = Cons (Raw_Cons {tag = Int b; _}); _}] ->
        (process_int_binop v a b, Types.int)
      | _ -> fail_wrong_args_number v
    end
  | "op_lt" | "op_leq" | "op_eq" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Int a; _}); _};
         MetaVal {node = Cons (Raw_Cons {tag = Int b; _}); _}] ->
        (process_int_test v a b, Types.bool)
      | _ -> fail_wrong_args_number v
    end
  | "op_and" | "op_or" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Bool a; _}); _};
         MetaVal {node = Cons (Raw_Cons {tag = Bool b; _}); _}] ->
        (process_bool_binop v a b, Types.bool)
      | _ -> fail_wrong_args_number v
    end
  | "op_op" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Int a; _}); _}] ->
        (process_int_monop v a, Types.int)
      | _ -> fail_wrong_args_number v
    end
  | "op_not" ->
    begin match args with
      | [MetaVal {node = Cons (Raw_Cons {tag = Bool a; _}); _}] ->
        (process_bool_monop v a, Types.bool)
      | _ -> fail_wrong_args_number v
    end
  | _ -> raise Not_a_primitive

let go arg_nf (Command cmd) =
  try
    match cmd.node with
    | Interact {valu = v; stk = s; _} ->
      let MetaStack {node = s; cont_typ; _} = s in
      let MetaVal {node = v; _} = v in
      begin match v,s with
        | Var v, CoDestr (Raw_Destr {tag = Call _; args; idxs; cont})
          -> begin
              if idxs <> [] then fail_parameters_in_primitives (Var.to_string v);
              let args = try process_args arg_nf args with Not_found -> raise Not_a_primitive in
              let v, ret_t = process_prim v args in
              let v =
                let a = CoVar.fresh () in
                let cmd' = Interact {valu = v; stk = S.ret a; mid_typ = ret_t} in
                let cmd' = Command {loc=cmd.loc; pol= Types.negative; node=cmd'} in
                V.case ~typ:cont_typ ~loc:cmd.loc Types.Thunk [thunk (a,ret_t), cmd'] in
              Some (Command {
                  node = Interact {valu = v; stk = cont; mid_typ = Types.thunk_t ret_t};
                  pol = Types.positive;
                  loc = cmd.loc
                })
            end
        | _ -> raise Not_a_primitive
      end

    | _ -> raise Not_a_primitive
  with Not_a_primitive -> None
