open Constraint
open Constructors
open Types
open Vars
open Ast
open FullAst


module type Prelude = sig
  val it : prelude
end

module Make (Prelude : Prelude) = struct

  module Params = struct

    type sort = Types.sort

    type node =
      | Var of Var.t * sort
      | Unit | Zero | Top | Bottom
      | Cons of ConsVar.t
      | Fun of int
      | Prod of int
      | Sum of int
      | Choice of int
      | ShiftPos
      | ShiftNeg
      | Fix
      | Box of Types.box_kind

    type deep = typ

    let eq a b = a = b

    let string_of_sort = function
      | Base Positive -> "+"
      | Base Negative -> "-"

    let string_of_node =
      let aux s n = s ^ "<" ^ string_of_int n ^ ">" in
      function
      | Unit -> "unit"
      | Zero -> "zero"
      | Bottom -> "bottom"
      | Top -> "top"
      | Var (v,sort) ->
        Var.to_string v ^ ":" ^ string_of_sort sort
      | Fix -> "fix"
      | Fun n -> aux "fun" n
      | Prod n -> aux "prod" n
      | Sum n -> aux "sum" n
      | Choice n -> aux "choice" n
      | Box k -> "box<" ^ string_of_box_kind k ^ ">"
      | ShiftNeg -> "shift-"
      | ShiftPos -> "shift+"
      | Cons c -> ConsVar.to_string c

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
      | Fun n -> (List.init n (fun _ -> pos)) --> neg
      | ShiftPos-> [pos]-->neg
      | ShiftNeg-> [neg]-->pos
      | Box _ -> [neg]-->pos
      | Cons c -> (TyConsVar.Env.find c Prelude.it.tycons).full_sort
      | Fix -> [neg]-->neg
      | Var (_,so) -> cst so

    let _var_env = ref []

    let deep_of_var s =
      match List.assoc_opt s !_var_env with
      | Some v -> tvar v
      | None ->
        let v = (TyVar.of_string s) in
        _var_env := (s, v) :: !_var_env;
        tvar v

    let mk_var () = Global_counter.fresh "a"

    let deep_of_cons args k = match k, args with
      | Var (v,_), _ -> tvar v
      | Unit, _ -> cons unit_t
      | Zero, _ -> cons zero
      | Top, _ -> cons top
      | Bottom, _ -> cons bottom
      | Fun _, ret::args -> cons (Constructors.Fun (args, ret))
      | Prod _, args -> cons (Constructors.Prod args)
      | Sum _, args -> cons (Constructors.Sum args)
      | Choice _, args -> cons (Constructors.Choice args)
      | ShiftNeg, [x] -> cons (Constructors.ShiftNeg x)
      | ShiftPos, [x] -> cons (Constructors.ShiftPos x)
      | Box k, [x] -> boxed k x
      | Cons c, args -> cons (Constructors.Cons (c, args))
      | Fix, [x] -> TFix x
      | _ -> raise (Failure "bad arity at type export")

    let rec folded_of_deep fold_var deep =
      let fold k args = Fold (Shallow (k, args)) in
      let args xs = List.map (folded_of_deep fold_var) xs in
      match deep with
      | TVar {node;_} | TInternal node -> fold_var (TyVar.to_string node)
      | TBox {kind; node; _} -> fold (Box kind) (args [node])
      | TPos node | TNeg node -> folded_of_deep fold_var node
      | TFix x -> fold Fix (args [x])
      | TCons {node; _} -> match node with
        | Constructors.Unit -> fold Unit []
        | Zero -> fold Zero []
        | Top -> fold Top []
        | Bottom -> fold Bottom []
        | ShiftPos x -> fold ShiftPos (args [x])
        | ShiftNeg x -> fold ShiftNeg (args [x])
        | Prod xs -> fold (Prod (List.length xs)) (args xs)
        | Sum xs -> fold (Sum (List.length xs)) (args xs)
        | Fun (xs,y) -> fold (Fun (List.length xs)) (args (y::xs))
        | Choice xs -> fold (Choice (List.length xs)) (args xs)
        | Cons (c, xs) -> fold (Cons c) (args xs)

  end

  include Constraint.Make (Params)

  let elab_typ : uvar -> typ elaboration = fun u typ ->
    let v, fvs = of_rank1_typ ~sort:(get_sort u) typ in
    exists fvs (eq u v) >>> fun uenv _ -> uenv v


  let rec elab_cmd : uvar -> command elaboration = fun u cmd ->
    let Command {pol; valu; stk; mid_typ; final_typ; loc} = cmd in
    let v = fresh_u (Base pol) in
    let cvalu, gvalu = elab_metaval v valu in
    let cstk, gstk = elab_metastack v u stk in
    let cmid, gmid = elab_typ v mid_typ in
    let cfinal, gfinal = elab_typ u final_typ in
    CLoc (loc, exists [v] (cvalu @+ cstk @+ cmid @+ cfinal))
    >>> fun uenv varenv -> Command {pol ; loc;
                                    valu = gvalu uenv varenv;
                                    stk = gstk uenv varenv;
                                    mid_typ = gmid uenv varenv;
                                    final_typ = gfinal uenv varenv}


  and elab_metaval : uvar -> meta_value elaboration = fun u mval ->
    let MetaVal {node; val_typ; loc} = mval in
    let cnode, gnode = elab_val u node in
    let ctyp, gtyp = elab_typ u val_typ in
    CLoc (loc, cnode @+ ctyp)
    >>> fun uenv varenv ->
    MetaVal {node = gnode uenv varenv; val_typ = gtyp uenv varenv; loc}


  and elab_metastack : uvar -> uvar -> meta_stack elaboration =
    fun ucont ufinal mstk ->
    let MetaStack {node; cont_typ; final_typ; loc} = mstk in
    let cnode, gnode = elab_stack ucont ufinal node in
    let ccont, gcont = elab_typ ucont cont_typ in
    let cfinal, gfinal = elab_typ ufinal final_typ in
    CLoc (loc, cnode @+ ccont @+ cfinal) >>> fun uenv varenv ->
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
        let v,fvs = of_rank1_typ ~sort:(Base Negative) (cons top) in
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
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Box kind, [v])) in
        let cbind, gbind = elab_typ v bind in
        let ccmd, gcmd = elab_cmd v cmd in
        exists [v;u'] (cbind @+ ccmd @+ eq u u')
        >>> fun uenv varenv -> Box {
          kind;
          bind = gbind uenv varenv;
          cmd = gcmd uenv varenv
        }

      | Fix {self=(x,t); cmd; cont} ->
        let w = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fix, [w])) in
        let v = shallow ~sort:(Base Positive) (Shallow (Box Exponential, [u'])) in
        let ccmd, gcmd = elab_cmd u' cmd in
        let cbind, gbind = elab_typ u' t in
        let ccont, gcont = elab_typ u' cont in
        exists [u';v;w] (CDef (Var.to_string x, v, cbind @+ ccmd @+ ccont ))
        >>> fun uenv varenv ->
        Fix { self = (x, gbind uenv varenv);
              cmd = gcmd uenv varenv;
              cont = gcont uenv varenv}

      | Cons cons ->
        let ccons, gcons = elab_cons u cons in
        ccons >>> fun uenv varenv -> Cons (gcons uenv varenv)

      | Destr copatts ->
        let cpatts, gpatts = List.split @@ List.map (elab_copatt u) copatts in
        CAnd cpatts >>> fun uenv varenv -> Destr (List.map (fun f -> f uenv varenv) gpatts)

      | Pack _ | Spec _ -> raise (Failure "Unsupported")


  and elab_stack : uvar -> uvar -> pre_stack elaboration =
    fun ucont ufinal stk -> match stk with

      (* TODO spectialize here *)
      | Ret -> eq ucont ufinal >>> fun _ _ -> Ret

      | CoZero ->
        let v,fvs = of_rank1_typ ~sort:(Base Positive) (cons zero) in
        exists fvs (eq ucont v) >>> fun _ _ -> CoZero

      (* TODO generalize here *)
      | CoBind { bind=(x,t); pol; cmd } ->
        let ccmd, gcmd = elab_cmd ufinal cmd in
        let cbind, gbind = elab_typ ucont t in
        CDef (Var.to_string x, ucont, cbind @+ ccmd)
        >>> fun uenv varenv -> CoBind {
          bind = (x, gbind uenv varenv);
          cmd = gcmd uenv varenv;
          pol
        }

      | CoBox { kind; stk } ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (Box kind, [v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [v;u'] (eq ucont u' @+ cstk)
        >>> fun uenv varenv -> CoBox {
          stk = gstk uenv varenv;
          kind
        }

      | CoFix stk ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fix, [v])) in
        let cstk, gstk = elab_metastack v ufinal stk in
        exists [v;u'] (eq ucont u' @+ cstk)
        >>> fun uenv varenv -> CoFix (gstk uenv varenv)

      | CoDestr destr ->
        let cdestr, gdestr = elab_destr ucont ufinal destr in
        cdestr >>> fun uenv varenv -> CoDestr (gdestr uenv varenv)

      | CoCons patts ->
        let cpatts, gpatts = List.split @@ List.map (elab_patt ucont ufinal) patts in
        CAnd cpatts
        >>> fun uenv varenv ->
        CoCons (List.map (fun f -> f uenv varenv) gpatts)

      | CoPack _ | CoSpec _ -> raise (Failure "Unsupported")


  and elab_cons : uvar -> (ConsVar.t, meta_value) constructor elaboration =
    fun u cons -> match cons with

      | Unit ->
        let u', fvs = of_rank1_typ ~sort:(Base Negative) (Types.cons unit_t) in
        exists fvs (eq u u') >>> fun _ _ -> Unit

      | ShiftPos mv ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (ShiftPos, [v])) in
        let cmv, gmv = elab_metaval v mv in
        exists [v;u'] (eq u u' @+ cmv)
        >>> fun uenv varenv -> ShiftPos (gmv uenv varenv)

      | Tupple mvs ->
        let n = List.length mvs in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cmvs, gmvs = List.split @@ List.map2 elab_metaval vs mvs in
        let u' = shallow ~sort:(Base Positive) (Shallow (Prod n, vs)) in
        exists (u'::vs) (eq u u' @+ CAnd cmvs) >>> fun uenv varenv ->
        Tupple (List.map (fun f -> f uenv varenv) gmvs)

      | Inj (i, n, mv) ->
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let v = List.nth vs i in
        let u' = shallow ~sort:(Base Positive) (Shallow (Sum n, vs)) in
        let cmv, gmv = elab_metaval v mv in
        exists (u'::vs) (eq u u' @+ cmv)
        >>> fun uenv varenv -> Inj (i, n, gmv uenv varenv)

      | PosCons (cons, args) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let Consdef { typ_args=_; val_args; resulting_type; private_typs } =
          def_of_cons Prelude.it cons in
        if private_typs <> [] then
          raise (Failure "Unsupported");
        let u', fvs = of_rank1_typ ~sort:(Base Positive) resulting_type in
        let val_args, fvss = List.split
            (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
        exists (List.concat (vs :: fvs :: fvss))
          (eq u u' @+ CAnd (List.map2 eq vs val_args) @+ CAnd cargs)
        >>> fun uenv varenv -> PosCons (cons, List.map (fun f -> f uenv varenv) gargs)


  and elab_destr : uvar -> uvar -> (DestrVar.t, 'x, 'a) destructor elaboration =
    fun ucont ufinal destr -> match destr with

      | Call (args, ret) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let w = fresh_u (Base Negative) in
        let cret, gret = elab_metastack w ufinal ret in
        let u' = shallow ~sort:(Base Negative) (Shallow (Fun n, w :: vs)) in
        exists (u'::w::vs) (eq ucont u' @+ cret @+ CAnd cargs) >>> fun uenv varenv ->
        Call (List.map (fun f -> f uenv varenv) gargs, gret uenv varenv)

      | Proj (i, n, ret) ->
        let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
        let v = List.nth vs i in
        let u' = shallow ~sort:(Base Negative) (Shallow (Choice n, vs)) in
        let cret, gret = elab_metastack v ufinal ret in
        exists (u'::vs) (eq ucont u' @+ cret) >>> fun uenv varenv ->
        Proj (i, n, gret uenv varenv)

      | ShiftNeg ret ->
        let v = fresh_u (Base Negative) in
        let u' = shallow ~sort:(Base Positive) (Shallow (ShiftNeg, [v])) in
        let cret, gret = elab_metastack v ufinal ret in
        exists [v;u'] (eq ucont u' @+ cret)
        >>> fun uenv varenv -> ShiftNeg (gret uenv varenv)

      | NegCons (destr, args, ret) ->
        let n = List.length args in
        let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
        let w = fresh_u (Base Negative) in
        let cret, gret = elab_metastack w ufinal ret in
        let cargs, gargs = List.split @@ List.map2 elab_metaval vs args in
        let Destrdef { typ_args=_;
                       val_args; ret_arg; resulting_type; private_typs } =
          def_of_destr Prelude.it destr in
        if private_typs <> [] then
          raise (Failure "Unsupported");
        let u', fvs = of_rank1_typ ~sort:(Base Negative) resulting_type in
        let vs', fvss = List.split
            (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
        let w', fvs' = of_rank1_typ ~sort:(Base Negative) ret_arg in
        exists (w :: List.concat (vs :: fvs :: fvs' :: fvss))
          (eq ucont u' @+ eq w w' @+ CAnd (List.map2 eq vs vs') @+ CAnd cargs @+ cret)
        >>> fun uenv varenv ->
        NegCons (destr, List.map (fun f -> f uenv varenv) gargs, gret uenv varenv)


  and elab_patt : uvar -> uvar -> (pattern * command) elaboration =
    fun ucont ufinal (patt, cmd) ->
    let ccmd, gcmd = elab_cmd ufinal cmd in
    match patt with

    | Unit ->
      let u', fvs = of_rank1_typ ~sort:(Base Positive) (Types.cons unit_t) in
      exists fvs (eq ucont u' @+ ccmd)
      >>> fun uenv varenv -> (Unit, gcmd uenv varenv)

    | ShiftPos (x,t) ->
      let v, fvs = of_rank1_typ ~sort:(Base Negative) t in
      let u' = shallow ~sort:(Base Positive) (Shallow (ShiftPos, [v])) in
      exists (u'::fvs) (eq ucont u' @+ CDecl (Var.to_string x, u', ccmd))
      >>> fun uenv varenv -> (ShiftPos (x, uenv v), gcmd uenv varenv)

    | Tupple binds ->
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let u' = shallow ~sort:(Base Positive) (Shallow (Prod n, vs)) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_string x, v, c)) in
      let c = List.fold_left2 go ccmd vs binds in
      exists vs (eq ucont u' @+ c)
      >>> fun uenv varenv ->
      let binds = List.map2 (fun (x,_) v -> x, uenv v) binds vs in
      (Tupple binds, gcmd uenv varenv)

    | Inj (i, n, (x, t)) ->
      let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
      let v = List.nth vs i in
      let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
      let u' = shallow ~sort:(Base Positive) (Shallow (Sum n, vs)) in
      exists fvs (eq ucont u' @+ eq v v' @+ CDecl (Var.to_string x, v, ccmd))
      >>> fun uenv varenv ->
      (Inj (i, n, (x, uenv v)), gcmd uenv varenv)

    | PosCons (cons, binds) ->
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let Consdef { typ_args=_; val_args; resulting_type; private_typs } =
        def_of_cons (Prelude.it) cons in
      if private_typs <> [] then
          raise (Failure "Unsupported");
      let u', fvs = of_rank1_typ ~sort:(Base Positive) resulting_type in
      let val_args, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
      let fvs = List.concat (fvs :: fvss) in
      let c = exists fvs (CAnd (eq ucont u' :: List.map2 eq vs val_args)) in
      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_string x, v, c)) in
      let c = List.fold_left2 go c vs binds in
      c >>> fun uenv varenv ->
      let binds = List.map2 (fun (x,_) v -> x, uenv v) binds vs in
      (PosCons (cons, binds), gcmd uenv varenv)

  and elab_copatt : uvar -> (copattern * command) elaboration =
    fun ucont (copatt, cmd) ->
    match copatt with

    | Call (binds, t) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let u' = shallow ~sort:(Base Negative) (Shallow (Fun n, ufinal::vs)) in
      let v', fvs = of_rank1_typ ~sort:(Base Negative) t in
      let go c w (x,t) =
        let w', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq w w' @+ CDef (Var.to_string x, w, c)) in
      let c = List.fold_left2 go ccmd vs binds in
      exists fvs (exists vs (eq ucont u' @+ eq ufinal v' @+ c))
      >>> fun uenv varenv ->
      let binds = List.map2 (fun (x,_) v -> x, uenv v) binds vs in
      (Call (binds, uenv ufinal), gcmd uenv varenv)

    | Proj (i, n, t) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let vs = List.init n (fun _ -> fresh_u (Base Negative)) in
      let w = List.nth vs i in
      let w', fvs = of_rank1_typ ~sort:(Base Negative) t in
      let u' = shallow ~sort:(Base Negative) (Shallow (Choice n, vs)) in
      exists fvs (eq ucont u' @+ eq w w' @+ ccmd)
      >>> fun uenv varenv ->
      (Proj (i, n, uenv ufinal), gcmd uenv varenv)

    | ShiftNeg t ->
      let ufinal = fresh_u (Base Positive) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let w, fvs = of_rank1_typ ~sort:(Base Positive) t in
      let u' = shallow ~sort:(Base Negative) (Shallow (ShiftNeg, [w])) in
      exists fvs (eq ucont u' @+ eq ufinal w @+ ccmd)
      >>> fun uenv varenv -> (ShiftNeg (uenv w), gcmd uenv varenv)

    | NegCons (destr, binds, ret) ->
      let ufinal = fresh_u (Base Negative) in
      let ccmd, gcmd = elab_cmd ufinal cmd in
      let n = List.length binds in
      let vs = List.init n (fun _ -> fresh_u (Base Positive)) in
      let Destrdef { typ_args=_; val_args; resulting_type; ret_arg; private_typs }
        = def_of_destr Prelude.it destr in
      if private_typs <> [] then
          raise (Failure "Unsupported");

      let u', fvs = of_rank1_typ ~sort:(Base Negative) resulting_type in
      let val_args, fvss = List.split
          (List.map (of_rank1_typ ~sort:(Base Positive)) val_args) in
      let v', fvs' = of_rank1_typ ~sort:(Base Negative) ret in
      let v'', fvs'' = of_rank1_typ ~sort:(Base Negative) ret_arg in
      let fvs = List.concat (fvs :: fvs' :: fvs'' :: fvss) in
      let c = CAnd (eq ucont u'
                    :: eq ufinal v'
                    :: eq v' v''
                    :: List.map2 eq vs val_args) in

      let go c v (x,t) =
        let v', fvs = of_rank1_typ ~sort:(Base Positive) t in
        exists fvs (eq v v' @+ CDef (Var.to_string x, v, c)) in
      let c' = List.fold_left2 go ccmd vs binds in
      exists fvs (c @+ c') >>> fun uenv varenv ->
      let binds = List.map2 (fun (x,_) v -> x, uenv v) binds vs in
      (NegCons (destr, binds, uenv v'), gcmd uenv varenv)



  let elab_prog_items items =

    let go (con, gen) item = match item with

      | Value_declaration {name; typ; pol; loc} ->
        let u,fvs = of_rank1_typ ~sort:(Base pol) typ in
        exists fvs (CDef (Var.to_string name, u, con))
        >>> fun uenv varenv ->
        Value_declaration {name; pol; loc; typ = uenv u} :: gen uenv varenv

      | Value_definition {name; typ; pol; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) typ in
        let cc, cgen = elab_metaval u content in
        exists fvs (cc @+ CDef (Var.to_string name, u, con))
        >>> fun uenv varenv ->
        Value_definition {
          name; pol; loc;
          typ = uenv u;
          content = cgen uenv varenv} :: gen uenv varenv

      | Command_execution {name; pol; cont; loc; content} ->
        let u, fvs = of_rank1_typ ~sort:(Base pol) cont in
        let cc, cgen = elab_cmd u content in
        exists fvs (cc @+ con)
        >>> fun uenv varenv ->
        Command_execution {
          name; pol; loc;
          cont = uenv u;
          content = cgen uenv varenv} :: gen uenv varenv in

    List.fold_left go (CTrue, fun _ _ -> []) (List.rev items)

  let go ~trace:trace items = solve ~trace elab_prog_items items

end
