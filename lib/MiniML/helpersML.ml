open AstML

(* let err msg (pos : Misc.position) = *)
(*   failwith *)
(*   (Printf.sprintf *)
(*       "{\"loc\": %s, \"info\": \"Error on line %d col %d: %s.\"}" *)
(*       (Misc.json_of_loc pos) *)
(*       pos.start_pos.Lexing.pos_lnum *)
(*       (pos.start_pos.Lexing.pos_cnum - pos.start_pos.Lexing.pos_bol) *)
(*       msg *)
(*   ) *)
(* ;; *)

let rec list_getlast_rem = function
  | [] -> raise Not_found
  | [ hd ] -> hd, []
  | hd :: tl ->
    let hd', tl' = list_getlast_rem tl in
    hd', hd :: tl'
;;

let list_getlast ls =
  let last, _ = list_getlast_rem ls in
  last
;;

let array_getlast array = Array.get array (Array.length array - 1)

let func_curryfy args body =
  List.fold_right
    (fun a acc -> { enode = Lambda { arg = a; body = acc }; eloc = a.vloc })
    args
    body
;;

let func_rec var args body =
  match args with
  | [] -> Misc.fatal_error "Parsing ML code" "Recusive Function Without Arguments" ~loc:var.vloc
  | arg :: args ->
    { enode = FunctionRec { var; arg; body = func_curryfy args body }; eloc = var.vloc }
;;

let functype_curryfy args body =
  List.fold_right
    (fun a acc -> { etype = TypeLambda { arg = a; return_type = acc }; tloc = a.tloc })
    args
    body
;;

let call_curryify func args =
  List.fold_left
    (fun acc a -> { enode = Call { func = acc; arg = a }; eloc = a.eloc })
    func
    args
;;
