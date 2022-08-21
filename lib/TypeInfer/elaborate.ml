open UnionFind
open Constraint
open Constructors
open Types
open Vars


module type Unifier_params_params = sig
  val arity_of_cons : ConsVar.t -> int
end

module Params (P : Unifier_params_params) = struct

  open P

  type node =
    | Var of Var.t
    | Unit | Zero | Top | Bottom
    | Cons of ConsVar.t
    | Fun of int
    | Prod of int
    | Sum of int
    | Choice of int
    | ShiftPos
    | ShiftNeg
    | Box of Types.box_kind

  type deep = typ

  let eq a b = a = b

  let arity = function
    | Var _ | Unit | Zero | Top | Bottom -> 0
    | Fun n | Prod n | Sum n | Choice n -> n
    | Box _| ShiftNeg | ShiftPos -> 1
    | Cons c -> arity_of_cons c

  let string_of_node =
    let aux s n = s ^ "<" ^ string_of_int n ^ ">" in
    function
    | Unit -> "unit"
    | Zero -> "zero"
    | Bottom -> "bottom"
    | Top -> "top"
    | Var v -> Var.to_string v
    | Fun n -> aux "fun" n
    | Prod n -> aux "prod" n
    | Sum n -> aux "sum" n
    | Choice n -> aux "choice" n
    | Box k -> "box<" ^ string_of_box_kind k ^ ">"
    | ShiftNeg -> "shift-"
    | ShiftPos -> "shift+"
    | Cons c -> ConsVar.to_string c

  let deep_of_var s = tvar (Var.of_string s)

  let mk_var () = Global_counter.fresh "a"

  let deep_of_cons args k = match k, args with
    | Var v, _ -> tvar v
    | Fun _, ret::args -> cons (Constructors.Fun (args, ret))
    | Prod _, args -> cons (Constructors.Prod args)
    | Sum _, args -> cons (Constructors.Sum args)
    | Choice _, args -> cons (Constructors.Choice args)
    | ShiftNeg, [x] -> cons (Constructors.ShiftNeg x)
    | ShiftPos, [x] -> cons (Constructors.ShiftPos x)
    | Box k, [x] -> boxed k x
    | _ -> raise (Failure "bad arity at type export")

  let rec folded_of_deep fold_var deep =
    let fold k args = Fold (Shallow (k, args)) in
    let args xs = List.map (folded_of_deep fold_var) xs in
    match deep with
    | TVar {node;_} | TInternal node -> fold_var (Var.to_string node)
    | TBox {kind; node; _} -> fold (Box kind) (args [node])
    | TPos node | TNeg node -> folded_of_deep fold_var node
    | TCons {node; _} -> match node with
      | Constructors.Unit -> fold Unit []
      | Zero -> fold Zero []
      | Top -> fold Top []
      | Bottom -> fold Bottom []
      | ShiftPos x -> fold ShiftPos (args [x])
      | ShiftNeg x -> fold ShiftNeg (args [x])
      | Prod xs -> fold (Prod (List.length xs)) (args xs)
      | Sum xs -> fold (Sum (List.length xs)) (args xs)
      | Fun (xs,y) -> fold (Prod (List.length xs)) (args (y::xs))
      | Choice xs -> fold (Choice (List.length xs)) (args xs)
      | Cons (c, xs) -> fold (Cons c) (args xs)

end

module NoCons_Params_Params  : Unifier_params_params = struct
  let arity_of_cons _ = raise (Failure "unimplemented")
end


open Ast.FullAst

open Constraint.Make (Params (NoCons_Params_Params))

type 'a elaboration = 'a -> con * ((uvar -> typ) -> (string -> typ) -> 'a)

let elab_typ : uvar -> typ elaboration = fun u typ ->
  let v, fvs = of_rank1_typ typ in
  exists fvs (eq u v) >>> fun uenv _ -> uenv v


let rec elab_cmd : uvar -> command elaboration = fun u cmd ->
  let Command {pol; valu; stk; mid_typ; final_typ; loc} = cmd in
  let v = fresh_u () in
  let cvalu, gvalu = elab_metaval v valu in
  let cstk, gstk = elab_metastack v u stk in
  let cmid, gmid = elab_typ v mid_typ in
  let cfinal, gfinal = elab_typ u final_typ in
  exists [v] (cvalu @+ cstk @+ cmid @+ cfinal)
  >>> fun uenv varenv -> Command {pol ; loc;
                                  valu = gvalu uenv varenv;
                                  stk = gstk uenv varenv;
                                  mid_typ = gmid uenv varenv;
                                  final_typ = gfinal uenv varenv}

and elab_metaval : uvar -> meta_value elaboration = fun u mval ->
  let MetaVal {node; val_typ; loc} = mval in
  let cnode, gnode = elab_val u node in
  let ctyp, gtyp = elab_typ u val_typ in
  cnode @+ ctyp
  >>> fun uenv varenv ->
    MetaVal {node = gnode uenv varenv; val_typ = gtyp uenv varenv; loc}

and elab_metastack : uvar -> uvar -> meta_stack elaboration =
  fun ucont ufinal mstk ->
  let MetaStack {node; cont_typ; final_typ; loc} = mstk in
  let cnode, gnode = elab_stack ucont ufinal node in
  let ccont, gcont = elab_typ ucont cont_typ in
  let cfinal, gfinal = elab_typ ufinal final_typ in
  cnode @+ ccont @+ cfinal >>> fun uenv varenv ->
  MetaStack {node = gnode uenv varenv;
             cont_typ = gcont uenv varenv;
             final_typ = gfinal uenv varenv;
            loc}

and elab_val : uvar -> pre_value elaboration =
  fun u valu -> match valu with

  | Var x ->
    let spec = new_spec () in
    (* TODO use specializer *)
    cvar (Var.to_string x) u spec >>> fun _ _ -> Var x

  | CoTop ->
    let v,fvs = of_rank1_typ (cons top) in
    exists fvs (eq u v) >>> fun _ _ -> CoTop

  | Bindcc { bind; pol; cmd } ->
    (* TODO generalize here *)
    let cbind, gbind = elab_typ u bind in
    let ccmd, gcmd = elab_cmd u cmd in
    cbind @+ ccmd
    >>> fun uenv varenv -> Bindcc {
      pol;
      bind = gbind uenv varenv;
      cmd = gcmd uenv varenv
    }

  | Box { kind; bind; cmd } ->
    let v = fresh_u () in
    let u' = shallow (Shallow (Box kind, [v])) in
    let cbind, gbind = elab_typ v bind in
    let ccmd, gcmd = elab_cmd v cmd in
    exists [v] (cbind @+ ccmd @+ eq u u')
    >>> fun uenv varenv -> Box {
      kind;
      bind = gbind uenv varenv;
      cmd = gcmd uenv varenv
    }

  | Cons _ -> _

  | Destr _ -> _

and elab_stack : uvar -> uvar -> pre_stack elaboration =
  fun ucont ufinal stk -> match stk with

    (* TODO spectialize here *)
  | Ret -> eq ucont ufinal >>> fun _ _ -> Ret

  | CoZero ->
    let v,fvs = of_rank1_typ (cons zero) in
    exists fvs (eq ucont v) >>> fun _ _ -> CoZero

    (* TODO generalize here *)
  | CoBind { bind=(x,t); pol; cmd } ->
    let ccmd, gcmd = elab_cmd ufinal cmd in
    let cbind, gbind = elab_typ ucont t in
    cbind @+ ccmd
    >>> fun uenv varenv -> CoBind {
      bind = (x, gbind uenv varenv);
      cmd = gcmd uenv varenv;
      pol
    }

  | CoBox { kind; stk } ->
    let v = fresh_u () in
    let u' = shallow (Shallow (Box kind, [v])) in
    let cstk, gstk = elab_metastack v ufinal stk in
    exists [v] (eq ucont u' @+ cstk)
    >>> fun uenv varenv -> CoBox {
      stk = gstk uenv varenv;
      kind
    }

  | CoDestr _ -> _

  | CoCons _ -> _

and elab_cons = _

and elab_destr = _


let rec elab_prelude = _

let elab_prog = _
