open Types
open Constructors
open Vars
open Ast


let string_of_binding v t =
  let v = Var.to_string v in
  match t with
  | None -> v
  | Some t ->  "(" ^ v ^ " : " ^ string_of_type t ^ ")"

let string_of_cobinding v t =
  let v = CoVar.to_string v in
  match t with
  | None -> v
  | Some t ->  "(" ^ v ^ " : " ^ string_of_type t ^ ")"


let string_of_pattern p =
  let aux (v,t) = string_of_binding v t in
  string_of_constructor aux p

let string_of_copattern p =
  let aux (v,t) = string_of_binding v t in
  let aux' (v,t) = string_of_cobinding v t in
  string_of_destructor aux aux' p

let rec string_of_value = function
  | Var v -> Var.to_string v
  | Bind (v,t,c) -> pp_sexp "let" [
      CoVar.to_string v;
      typ_annot @@ string_of_negtype t;
      string_of_command c]
  | Force (a,t,c) -> pp_sexp "force" [
      CoVar.to_string a;
      typ_annot @@ string_of_postype t;
      string_of_command c
    ]
  | Box (k,v,t,c) -> pp_sexp "box" [
      (string_of_box_kind k);
      (CoVar.to_string v);
      typ_annot @@ (string_of_type t);
      (string_of_command c)
    ]
  | Cons c -> string_of_constructor string_of_value c
  | Destr patts ->
    let string_of_case (p,c) = [
      string_of_copattern p;
      string_of_command c
    ] in
    pp_sexp "match" (List.concat (List.map string_of_case patts))

and string_of_stack = function
  | CoVar v -> CoVar.to_string v
  | CoBind (v,t,c) -> pp_sexp "let" [
      Var.to_string v;
      typ_annot @@ string_of_postype t;
      string_of_command c
    ]
  | CoForce (x,t,c) -> pp_sexp "force" [
      Var.to_string x;
      typ_annot @@ string_of_negtype t;
      string_of_command c
    ]
  | CoBox (k,s) -> pp_sexp "unbox" [
      string_of_box_kind k;
      string_of_stack s
    ]
  | CoDestr c -> string_of_destructor string_of_value string_of_stack c
  | CoCons patts ->
    let string_of_case (p,c) = [
      string_of_pattern p;
      string_of_command c
    ] in
    pp_sexp "match" (List.concat (List.map string_of_case patts))

and string_of_command (Command (p,v,s)) =
  let pol = function
    | Pos -> "jump"
    | Neg -> "enter"
    | Unknown -> "ambiguous"
  in
  pp_sexp (pol p) [string_of_value v; string_of_stack s]

  let list_to_string ?interspace:inter k ls =
    let inter = Option.value ~default:"" inter in
    match ls with
    | [] -> ""
    | [x] -> k x
    | ls -> List.fold_left (fun acc arg -> if acc = "" then k arg else acc ^ inter ^ k arg) "" ls

  let cons_def_to_string ?cont:cont cons args =
    let cont = match cont with
      | None -> ""
      | Some cont -> " cont " ^ string_of_type cont in
    let args =
        list_to_string ~interspace:" * " string_of_type args in
    Printf.sprintf "| %s of %s%s" (ConsVar.to_string cons) args cont

  let tbind_to_string (t, so) =
    Printf.sprintf "%s %s" (TyVar.to_string t) (string_of_sort so)

  let lhs_to_string name args =
    if args = [] then
      TyVar.to_string name
    else
      let args = list_to_string ~interspace:" " tbind_to_string args  in
      Printf.sprintf "{%s %s}" (TyVar.to_string name) args


  let item_to_string = function

    | Type_declaration {name; sort} ->
      Printf.sprintf "decl type %s : %s"
        (TyVar.to_string name)
        (string_of_sort sort)

    | Type_definition {name; sort; args; content} ->
      Printf.sprintf "type %s : %s = %s"
        (lhs_to_string name args)
        (string_of_sort sort)
        (string_of_type content)

    | Data_definition {name; args; content} ->
      let aux def = "\n  | " ^ Constructors.definition_of_constructor string_of_type def in
      Printf.sprintf "data %s =%s"
        (lhs_to_string name args)
        (list_to_string aux content)

    | Codata_definition {name; args; content} ->
      let aux def = "\n  | " ^ Constructors.definition_of_destructor string_of_type def in
      Printf.sprintf "codata %s =%s"
        (lhs_to_string name args)
        (list_to_string aux content)

    | Term_definition {name; typ; content} ->
      Printf.sprintf "term %s : %s = %s"
        (Var.to_string name)
        (string_of_type typ)
        (string_of_value content)

    | Env_definition {name; typ; content} ->
      Printf.sprintf "env %s : %s = %s"
        (Var.to_string name)
        (string_of_type typ)
        (string_of_stack content)

    | Cmd_definition {name; content} ->
      Printf.sprintf "cmd %s = %s"
        (Var.to_string name)
        (string_of_command content)

  let program_to_string prog =
    list_to_string ~interspace:"\n" item_to_string prog
