open Constraint
open Types
open Vars
open Prelude

module type Prelude = sig
  val it : prelude
end

module Params (Prelude : Prelude) = struct

    let string_of_rel = RelVar.to_string

    let sort_of_rel rel = RelVar.Env.find rel !(Prelude.it).relations

    let sort_of_node (c : node) =
      let aux c = (TyConsVar.Env.find c !(Prelude.it).tycons).sort in
      unmk_arrow (sort_of_type_cons aux c)

    let is_syntactic_sort = function
      | Idx _ -> false
      | _ -> true

    let pp_rel = RelVar.pp

    let pp_var = TyVar.pp

    let var_of_int = TyVar._debug_of_int

    let int_of_var = TyVar._debug_to_int

    let deep_of_var s = TInternal s

    let mk_var = TyVar.fresh

    let deep_of_cons args c = if args = [] then cons c else app (cons c) args

    let deep_of_index args c = if args = [] then c else app c args

    let folded_of_deep fold_var fold_idx deep sort =
      let fold k args = Fold (Shallow (k, args)) in
      let subst = ref TyVar.Env.empty in
      let add v (typ_opt, so) = (subst := TyVar.Env.add v (typ_opt,so) !subst) in
      let get v =
        match TyVar.Env.find_opt v !subst with
        | Some x -> x
        | None -> (None, TyVar.Env.find v !(Prelude.it).sorts)
      in

      let rec go sort deep : folded = match deep with

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
            | Some typ -> go sort typ
          end

        | TPos node -> go sort node
        | TNeg node -> go sort node

        | TCons {node; _} -> begin match node with
          | Cons c ->
            let def = def_of_tycons Prelude.it c in
            begin match def.content with
              | Defined typ ->
                assert (def.args = []);
                go def.sort typ
              | _ -> fold (Cons c) []
            end
          | c -> fold c []
        end

      | TApp {tfun; args = []; _} -> go sort tfun

      | TApp {tfun = TCons {node;_}; args; _} -> begin
          let sorts, _ = sort_of_node node in
          match node with
          | Unit | Zero | Top | Bottom -> assert false
          | Cons c -> begin
              let def = def_of_tycons Prelude.it c in
              match def.content with
              | Defined typ -> begin
                  List.iter2
                    (fun (x,so) y -> add x (Some y,so))
                    def.args args;
                  go def.sort typ
                end
              | _ -> choose_shallow def.sort node args
            end
          | _ ->  fold node (List.map2 go sorts args)
        end

      | TApp {tfun = (TVar {node; _} | TInternal node); args; loc} ->
        begin match get node with
        | None, sort ->
           let sorts, _ = unmk_arrow sort in
           fold_idx (fold_var node sort) sort (List.map2 go sorts args)
        | Some typ, _ -> go sort (TApp {tfun = typ; args; loc})
        end

      | TApp {tfun = TApp {tfun; args = args1; _}; args = args2; loc} ->
        go sort (TApp {tfun; args = args1 @ args2; loc})

      | TApp {tfun = (TPos _ | TNeg _); _} -> assert false

      and choose_shallow sort node args =
        let sorts, rets = unmk_arrow sort in
        let args1, args2 = Misc.list_take args (List.length sorts) in
        if List.for_all (function Idx _ -> true | _ -> false) sorts then begin
          assert (args2 = []);
          fold_idx (go sort (TCons {node;loc=Misc.dummy_pos})) sort (List.map2 go sorts args1)
        end else if List.for_all (function Idx _ -> false | _ -> true) sorts then begin
          let tfun = fold node (List.map2 go sorts args1) in
          let sorts, _ = unmk_arrow rets in
          fold_idx tfun sort (List.map2 go sorts args2)
        end else
          assert false

      in
      go sort deep

  end
