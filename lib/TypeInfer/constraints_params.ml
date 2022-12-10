open Constraint
open Types
open Vars
open Ast
open Prelude
open FullAst

module type Prelude = sig
  val it : prelude
end

module Params (Prelude : Prelude) = struct

    type sort = SortVar.t Types.sort

    type rel = RelVar.t

    let string_of_rel = RelVar.to_string

    let sort_of_rel rel = RelVar.Env.find rel !(Prelude.it).relations

    let is_valid_sort so = (Types.is_index_sort so) || (Types.is_monotype_sort so)

    let is_syntactic_sort = function
      | Base _ -> true
      | Index _ | Arrow _ -> false

    type node =
      | Var of TyVar.t * sort
      | Unit | Zero | Top | Bottom
      | Cons of TyConsVar.t
      | Fun of int
      | Prod of int
      | Sum of int
      | Choice of int
      | Thunk
      | Closure
      | Fix
      | Box of Types.box_kind

    type deep = typ

    type var = TyVar.t

    let eq a b = a = b

    let string_of_sort = Types.string_of_sort SortVar.to_string

    let string_of_node =
      let aux s n = s ^ "<" ^ string_of_int n ^ ">" in
      function
      | Unit -> "unit"
      | Zero -> "zero"
      | Bottom -> "bottom"
      | Top -> "top"
      | Var (v,sort) -> TyVar.to_string v ^ ":" ^ string_of_sort sort
      | Fix -> "fix"
      | Fun n -> aux "fun" n
      | Prod n -> aux "prod" n
      | Sum n -> aux "sum" n
      | Choice n -> aux "choice" n
      | Box k -> "box<" ^ string_of_box_kind k ^ ">"
      | Thunk -> "thunk"
      | Closure -> "closure"
      | Cons c -> TyConsVar.to_string c

    let sort_of_cons =
      let cst x = ([], x) in
      let (-->) xs x = (xs, x) in
      let pos = Base Positive in
      let neg = Base Negative in
      function
      | Unit | Zero -> cst pos
      | Top | Bottom -> cst neg
      | Prod n | Sum n -> (List.init n (fun _ -> pos) ) --> pos
      | Choice n -> (List.init n (fun _ -> neg)) --> neg
      | Fun n -> (neg :: List.init n (fun _ -> pos)) --> neg
      | Thunk -> [pos]-->neg
      | Closure -> [neg]-->pos
      | Box _ -> [neg]-->pos
      | Cons c -> unmk_arrow (TyConsVar.Env.find c !(Prelude.it).tycons).sort
      | Fix -> [neg]-->neg
      | Var (_,so) -> unmk_arrow so

    let _var_env = ref []

    let tvar_of_string s =
      match List.assoc_opt s !_var_env with
      | Some v -> v
      | None ->
        let v = (TyVar.of_string s) in
        _var_env := (s, v) :: !_var_env;
        v

    let string_of_tvar v =
      match List.assoc_opt v (List.map (fun (u,v) -> (v,u)) !_var_env) with
      | Some u -> u
      | None ->
        let s = TyVar.to_string v in
        _var_env := (s, v) :: !_var_env;
        s

    let string_of_var = string_of_tvar
    let var_of_string = tvar_of_string

    let deep_of_var s = TInternal s

    let mk_var () =
      let s = Global_counter.fresh "a" in
      let v = TyVar.of_string s in
      _var_env := (s, v) :: !_var_env;
      v

    let deep_of_cons args k = match k, args with
      | Var (v,_), _ -> tvar v
      | Unit, _ -> unit_t
      | Zero, _ -> zero
      | Top, _ -> top
      | Bottom, _ -> bottom
      | Fun _, args -> func args
      | Prod _, args -> prod args
      | Sum _, args -> sum args
      | Choice _, args -> choice args
      | Closure, [x] -> app (cons Closure) [x]
      | Thunk, [x] -> app (cons Thunk) [x]
      | Box k, [x] -> boxed k x
      | Cons c, args ->
        if args = [] then
          (cons (Cons c))
        else
          app (cons (Cons c)) args
      | Fix, [x] -> TFix x
      | _ -> raise (Failure "bad arity at type export")

    let folded_of_deep fold_var deep =
      let fold k args = Fold (Shallow (k, args)) in
      let subst = ref TyVar.Env.empty in
      let add v (typ_opt, so) = (subst := TyVar.Env.add v (typ_opt,so) !subst) in
      let get v =
        match TyVar.Env.find_opt v !subst with
        | Some x -> x
        | None -> (None, TyVar.Env.find v !(Prelude.it).sorts)
      in

      let rec go deep : 'a folded = match deep with

        | TVar {node;_} | TInternal node ->
          begin
            let typ_opt, sort =
              try get node with
                Not_found ->
                let sort =
                  try TyVar.Env.find node !(Prelude.it).sorts
                  with Not_found -> raise (Failure (TyVar.to_string node)) in
                (None, sort) in
            match typ_opt with
            | None -> fold_var node sort
            | Some typ -> go typ
          end

        | TBox {kind; node; _} -> fold (Box kind)  [go node]
        | TPos node -> go node
        | TNeg node -> go node
        | TFix x -> fold Fix [go x]

      | TCons {node; _} -> begin match node with
          | Types.Unit -> fold Unit []
          | Zero -> fold Zero []
          | Top -> fold Top []
          | Bottom -> fold Bottom []
          | Thunk -> fold Thunk []
          | Closure -> fold Closure []

          | Cons c ->
            let def = def_of_tycons Prelude.it c in
            begin match def.content with
              | Defined typ ->
                assert (def.args = []);
                go typ 
              | _ -> match def.sort with
                | Base _ | Index _ -> fold (Cons c) []
                | Arrow _ -> assert false (* Constructors must be fully applied *)
            end
          | Prod _ | Choice _ | Sum _ | Fun _ -> assert false (* this is treated further done *)
        end

      | TApp {tfun; args = []; _} -> go tfun
      | TApp {tfun = TApp {tfun; args = args'; _}; args; loc} ->
        go (TApp {loc; tfun; args = args' @ args})
      | TApp {tfun = TCons {node;_}; args; _} -> begin
          match node with
          | Prod n -> fold (Prod n) (List.map go args)
          | Sum n -> fold (Sum n) (List.map go args)
          | Choice n -> fold (Choice n) (List.map go args)
          | Fun n -> fold (Fun n) (List.map go args)
          | Thunk -> fold Thunk (List.map go args)
          | Closure -> fold Closure (List.map go args)
          | Cons c -> begin
              let def = def_of_tycons Prelude.it c in
              match def.content with
              | Defined typ -> begin
                  List.iter2
                    (fun (x,so) y -> add x (Some y,so))
                    def.args args;
                  go typ
                end
              | _ -> fold (Cons c) (List.map go args)
            end
          | _ -> assert false (* this is treated above *)
        end
      | TApp {tfun= TVar {node; _}; args; loc} ->
        begin match get node with
        | None, so -> fold (Var (node, so)) (List.map go args)
        | Some typ, _ -> go (TApp {tfun = typ; args; loc})
        end
      | TApp {tfun=_;_} -> assert false (* by sorting rules, this can't happen *)

      in
      go deep 

  end
