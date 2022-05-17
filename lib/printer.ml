open Types
open Constructors
open Vars
open Ast

let string_of_binding v t =
  let v = Var.to_string v in
  match t with
  | None -> v
  | Some t ->  "(" ^ v ^ " : " ^ string_of_type t ^ ")"

let string_of_binding_no_paren v t =
  let v = Var.to_string v in
  match t with
  | None -> v
  | Some t -> v ^ " : " ^ string_of_type t


let string_of_cobinding t =
  match t with
  | None -> "ret()"
  | Some t ->  "ret() : " ^ string_of_type t

let string_of_pattern p =
  let aux (v,t) = string_of_binding_no_paren v t in
  string_of_constructor aux p

let string_of_copattern p =
  let aux (v,t) = string_of_binding_no_paren v t in
  let aux' t = string_of_cobinding t in
  string_of_destructor aux aux' p


let rec string_of_value = function

  | Var v -> Var.to_string v

  | Bindcc {po; typ; cmd} ->
      let po = string_of_polarity po in
      let bind = string_of_cobinding typ in
      let cmd = string_of_command cmd in
      Printf.sprintf "bind/cc%s %s -> %s" po bind cmd

  | Box {kind; typ; cmd} ->
      let kind = string_of_box_kind kind in
      let bind = string_of_cobinding typ in
      let cmd = string_of_command cmd in
      Printf.sprintf "box(%s) %s -> %s" kind bind cmd

  | Cons c -> string_of_constructor string_of_value c

  | Destr patts ->
    let string_of_case (p,c) =
      let p = string_of_copattern p in
      let c = string_of_command c in
      Printf.sprintf "| %s -> (%s) " p c in
    let cases = List.fold_left (^) "" (List.map string_of_case patts) in
    Printf.sprintf "match %s end" cases

and string_of_stack = function

  | Ret -> "ret()"

  | CoBind {po; name; typ; cmd} ->
    let po = string_of_polarity po in
    let bind = string_of_binding name typ in
    let cmd = string_of_command cmd in
    Printf.sprintf "bind%s %s -> %s" po bind cmd

  | CoBox {kind; stk} ->
    let kind = string_of_box_kind kind in
    let stk = string_of_stack stk in
    Printf.sprintf "unbox(%s).%s" kind stk

  | CoDestr c -> string_of_destructor string_of_value string_of_stack c

  | CoCons patts ->
    let string_of_case (p,c) =
      let p = string_of_pattern p in
      let c = string_of_command c in
      Printf.sprintf "| %s -> (%s) " p c in
    let cases = List.fold_left (^) "" (List.map string_of_case patts) in
    Printf.sprintf "match %s end" cases

and string_of_command (Command {po;valu;typ;stk}) =
  let po = string_of_polarity po in
  let valu = string_of_value valu in
  let typ = match typ with
    | Some typ -> " (" ^ string_of_type typ ^ ")"
    | None -> "" in
  let stk = string_of_stack stk in
  Printf.sprintf "step(%s) %s : %s into %s" po valu typ stk


let list_to_string ?interspace:inter k ls =
  let inter = Option.value ~default:"" inter in
  match ls with
  | [] -> ""
  | [x] -> k x
  | ls -> List.fold_left (fun acc arg -> if acc = "" then k arg else acc ^ inter ^ k arg) "" ls

let tbind_to_string (t, so) =
  match so with
  | Some so -> Printf.sprintf "%s : %s" (TyVar.to_string t) (string_of_sort so)
  | None -> ""

let lhs_to_string name args =
  if args = [] then
    TyVar.to_string name
  else
    let args = list_to_string ~interspace:" " tbind_to_string args  in
    Printf.sprintf "{%s %s}" (TyVar.to_string name) args


let string_of_prog_item = function

  | Type_declaration {name; sort} ->
    Printf.sprintf "decl type %s : %s"
      (TyVar.to_string name)
      (string_of_sort sort)

  | Type_definition {name; sort; args; content} ->
    Printf.sprintf "type %s%s = %s"
      (lhs_to_string name args)
      (match sort with None -> "" | Some so -> " : " ^ string_of_sort so)
      (string_of_type content)

  | Data_definition {name; args; content} ->
    let aux def = "\n  | " ^ Constructors.definition_of_constructor string_of_type def in
    Printf.sprintf "data %s = %s"
      (lhs_to_string name args)
      (list_to_string aux content)

  | Codata_definition {name; args; content} ->
    let aux def = "\n  | " ^ Constructors.definition_of_destructor string_of_type def in
    Printf.sprintf "codata %s = %s"
      (lhs_to_string name args)
      (list_to_string aux content)

  | Term_definition {name; typ; content} ->
    Printf.sprintf "term %s%s = %s"
      (Var.to_string name)
      (match typ with None -> "" | Some typ -> " : " ^ string_of_type typ)
      (string_of_value content)

  | Env_definition {name; typ; content} ->
    Printf.sprintf "env %s%s = %s"
      (Var.to_string name)
      (match typ with None -> "" | Some typ -> " : " ^ string_of_type typ)
      (string_of_stack content)

  | Cmd_definition {name; content; typ} ->
    Printf.sprintf "cmd %s%s = %s"
      (Var.to_string name)
      (match typ with None -> "" | Some typ -> " : " ^ string_of_type typ)
      (string_of_command content)

let string_of_program prog =
  list_to_string ~interspace:"\n" string_of_prog_item prog
