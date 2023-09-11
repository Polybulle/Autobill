open AstML
open Lcbpv
open Misc
open Types


module Converter () :
sig
  val trans_prog : AstML.prog -> Lcbpv.program
end

= struct

  module M = Vars.StrM

  type env = polarity M.t

  let _env : env ref = ref M.empty

  let add_var_polarity v pol = _env := M.add v pol !_env

  let get_var_polarity v = M.find v !_env

  let generate_variable loc =
    let v = Vars.Var.fresh () in
    (Vars.Var.to_string v, loc)

  let trans_boolean = function
    | true -> True
    | false -> False


  let trans_litl = function
    | Integer i -> Expr_Int i
    | Boolean b -> Expr_Constructor (trans_boolean b, [])
    | Unit -> Expr_Constructor (Unit, [])

  let rec sub_x eff var (s : statement) : statement =
    match s.snode with
    | Stmt_pure e -> {s with snode = Stmt_pure {eloc = s.sloc; enode = LiftState (e,eff)}}
    | Stmt_return _ -> s
    | Stmt_let (var', s1, s2) ->
      assert (var' <> var);
      { s with
        snode =
          Stmt_let
            ( var'
            , sub_x eff var s1
            , { sloc = s.sloc
              ; snode = Stmt_let (var, { sloc = s.sloc; snode = Stmt_get }, sub_x eff var s2)
              } )
      }
    | Stmt_if (e, s1, s2) -> { s with snode = Stmt_if (e, sub_x eff var s1, sub_x eff var s2) }
    | Stmt_mut (var', e, s) ->
      assert (var' <> var);
      { s with snode = Stmt_mut (var', e, sub_x eff var s) }
    | Stmt_mut_change_set (var', e, s) ->
      let basic_ident = fst (generate_variable s.sloc) in
      let dummy = {basic_ident; vloc = s.sloc } in
      if var = var' then
        {s with snode = Stmt_let
            ( dummy
            , {s with snode = Stmt_set e}
            , sub_x eff var s)}
      else
        s
    | Stmt_for (var', e, s) ->
      { s with
        snode =
          Stmt_for
            ( var'
            , e
            , { sloc = s.sloc
              ; snode = Stmt_let (var, { sloc = s.sloc; snode = Stmt_get }, sub_x eff var s)
              } )
      }
    | _ -> s

  let rec transL eff stmt =
    match stmt.snode with
    | Stmt_pure e -> {stmt with snode = Stmt_pure {eloc = stmt.sloc; enode = LiftEx (e,eff)}}
    | Stmt_return _ -> assert false (*No early return in for loop *)
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transL eff s1, transL eff s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transL eff s) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transL eff s1, transL eff s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transL eff s) }
    | _ -> stmt

  and transC eff stmt =
    match stmt.snode with
    | Stmt_break -> stmt
    | Stmt_continue ->
      {stmt with
       snode = Stmt_pure
           { eloc = stmt.sloc;
             enode = LiftEx ({ eloc = stmt.sloc
                             ; enode = ThrowEx ({ eloc = stmt.sloc; enode = Litteral Unit }, eff)
                             }, eff)}}
    | Stmt_pure e ->
      {stmt with snode = Stmt_pure { eloc = stmt.sloc; enode = LiftEx (e, eff)}}
    | Stmt_return _ -> assert false (* No early return in for loop *)
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transC eff s1, transC eff s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transC eff s) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transC eff s1, transC eff s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transC eff s) }
    | _ -> stmt

  and transB eff stmt =
    match stmt.snode with
    | Stmt_break ->
      { stmt with snode = Stmt_throw { eloc = stmt.sloc; enode = Litteral Unit } }
    | Stmt_return _ -> assert false (* No early return in for loop*)
    | Stmt_pure e ->
      {stmt with snode = Stmt_pure { eloc = stmt.sloc; enode = LiftEx (e, eff)}}
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transB eff s1, transB eff s2) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transB eff s1, transB eff s2) }
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transL eff s) }
    | _ -> stmt

  and transD stmt eff =
    match stmt.snode with
    | Stmt_pure e -> e
    | Stmt_return e -> { enode = Return (e, eff); eloc = stmt.sloc }
    | Stmt_let (var, s1, s2) ->
      { enode =
          BindMonadic
            ( transD s1 eff
            , { enode = Lambda { arg = var; body = transD s2 eff }; eloc = stmt.sloc }
            , eff )
      ; eloc = stmt.sloc
      }
    | Stmt_if (e, s1, s2) ->
      { enode = If (e, transD s1 eff, transD s2 eff, eff); eloc = stmt.sloc }
    | Stmt_mut (var, e, s) ->
      { eloc = stmt.sloc
      ; enode =
          Binding
            { var
            ; init = e
            ; content =
                { eloc = stmt.sloc
                ; enode =
                    RunState
                      ( transD
                          (sub_x eff var s)
                          (State ({ etype = TypeUnit; tloc = stmt.sloc }, eff))
                      , { eloc = stmt.sloc; enode = Variable var },
                      eff)
                }
                (* State + effets de ^ *)
            }
      }
    | Stmt_mut_change_set _ -> assert false
    | Stmt_get -> { eloc = stmt.sloc; enode = Get eff}
    | Stmt_set e -> { eloc = stmt.sloc; enode = Set (e,eff) }
    | Stmt_throw e -> { eloc = stmt.sloc; enode = ThrowEx (e,eff) }
    | Stmt_for (var, e, s) ->
      let eff' = Except ({ etype = TypeUnit; tloc = stmt.sloc }, eff) in
      let eff'' = Except ({ etype = TypeUnit; tloc = stmt.sloc }, eff') in
      { eloc = stmt.sloc
      ; enode =
          RunCatch
            ({ eloc = stmt.sloc
             ; enode =
                 ForM
                   ( e
                   , { eloc = stmt.sloc
                     ; enode =
                         Lambda
                           { arg = var
                           ; body =
                               { eloc = stmt.sloc
                               ; enode = RunCatch (transD (transC eff'' (transB eff s)) eff'', eff')
                               }
                           }
                     }, eff')
             }, eff)
      }
    | Stmt_break -> assert false
    | Stmt_continue -> assert false
    | Stmt_early_return _ -> assert false

  and transR eff stmt =
    match stmt.snode with
    | Stmt_return e -> { stmt with snode = Stmt_throw e }
    | Stmt_pure e ->
      {stmt with snode = Stmt_pure { eloc = stmt.sloc; enode = LiftEx (e, eff)}}
    | Stmt_let (var, s1, s2) -> { stmt with snode = Stmt_let (var, transR eff s1, transR eff s2) }
    | Stmt_if (e, s1, s2) -> { stmt with snode = Stmt_if (e, transR eff s1, transR eff s2) }
    | Stmt_mut (var, e, s) -> { stmt with snode = Stmt_mut (var, e, transR eff s) }
    | Stmt_mut_change_set _ -> stmt
    | Stmt_break -> stmt
    | Stmt_continue -> stmt
    | Stmt_for (var, e, s) -> { stmt with snode = Stmt_for (var, e, transR eff s) }
    | _ -> assert false (* not reachable yet *)

  and trans_block stmt : Lcbpv.expression =
    let eff = (Except ({ etype = TypeUnit; tloc = stmt.sloc }, Ground)) in
    trans_neg_expr { enode = RunCatch (transD (transR eff stmt) eff, Ground)
                   ; eloc = stmt.sloc
                   }

  and trans_eff = function
    | AstML.Ground -> Lcbpv.Ground
    | State (_, eff) -> State (trans_eff eff)
    | Except (_, eff) -> Exn (trans_eff eff)

  and trans_var (pol : polarity) (x : AstML.variable) =
    match pol, get_var_polarity x.basic_ident with
    | Negative, Positive ->
      let y = generate_variable x.vloc in
      Expr_Block
        (Blk
           ( [ Ins_Open (y, Exp, (Expr_Var (x.basic_ident, x.vloc), x.vloc)), x.vloc]
           , (Expr_Var y, x.vloc)
           , x.vloc ))
    | Positive, Negative ->
      let y = generate_variable x.vloc in
      Expr_Block
        (Blk
           ( [ Ins_Force (y, (Expr_Var (x.basic_ident, x.vloc), x.vloc)), x.vloc]
           , (Expr_Var y, x.vloc)
           , x.vloc ))
    | _ -> Expr_Var (x.basic_ident, x.vloc)

  and trans_binder pol x =
    add_var_polarity x.basic_ident pol;
    (x.basic_ident, x.vloc)

  and trans_pos_expr e =
    ( (match e.enode with

          (* positives *)
          | Litteral l -> trans_litl l
          | Variable v -> trans_var Positive v
          | Tuple tpl -> Expr_Constructor (Tuple, List.map trans_pos_expr tpl)
          | Construct construct ->
            Expr_Constructor
              (Cons_Named construct.constructor_ident, List.map trans_pos_expr construct.to_group)
          | CallUnary { op; arg = Some arg } -> Expr_Mon_Prim (op, trans_pos_expr arg)
          | CallBinary { op; args = first :: second :: _ } ->
            Expr_Bin_Prim (op, trans_pos_expr first, trans_pos_expr second)
          | CallUnary { op=_; arg = None } ->
            Misc.fatal_error "Translating ML code" ~loc:e.eloc
              "This primitive call has a wrong number of argument"
          | CallBinary { op=_; args=_ } ->
            Misc.fatal_error "Translating ML code" ~loc:e.eloc
              "This primitive call has a wrong number of argument"

          | Binding _ | Match _ | Sequence _ | Do _ -> fst (trans_neutral_expr Positive e)

          | Call _ ->
            let openvar = generate_variable e.eloc in
            let returnvar = generate_variable e.eloc in
            Expr_Block
              (Blk
                 ( [ Ins_Open (openvar, Exp, trans_pos_expr e), e.eloc
                   ; Ins_Force (returnvar, (Expr_Var openvar, e.eloc)), e.eloc]
                 , (Expr_Var returnvar, e.eloc)
                 , e.eloc ))

          | Lambda _ | FunctionRec _ -> Expr_Closure (Exp, trans_neg_expr e)

          | BindMonadic (_, _, _) | Return (_, _) | If (_, _, _, _) | Get _ | Set _
          | RunState (_, _, _) | LiftState (_, _) | ThrowEx _ | LiftEx (_, _) | RunCatch _
          | ForM (_, _,_) -> assert false

        ), e.eloc)

  and trans_neutral_expr pol e =
    (( match e.enode with
        (* neutrals *)
        | Binding bind ->
          let v = trans_binder pol bind.var in
          Expr_Block
            (Blk
               ( [ Ins_Let (v, trans_neutral_expr pol bind.init), bind.var.vloc ]
               , trans_neutral_expr pol bind.content
               , e.eloc ))
        | Match mat ->
          Expr_Match (trans_pos_expr mat.to_match, List.map (trans_match_case pol) mat.cases)
        | Sequence expr_ls ->
          let last, rem = HelpersML.list_getlast_rem expr_ls in
          Expr_Block
            (Blk
               ( List.map
                   (fun e -> Ins_Let (generate_variable e.eloc, trans_neutral_expr pol e), e.eloc)
                   rem
               , trans_neutral_expr pol last
               , last.eloc ))
        | Do stmt ->
          let v = generate_variable e.eloc in
          let expr = trans_block stmt in
          begin match pol with
            | Negative -> fst expr
            | Positive ->
              Expr_Block
                (Blk
                   ( [ Ins_Force (v, expr), e.eloc ]
                   , (Expr_Var v, e.eloc)
                   , e.eloc ))
          end

        | _ -> fst (match pol with Positive -> trans_pos_expr e | Negative -> trans_neg_expr e)
      ), e.eloc)

  and trans_neg_expr e =
    (( match e.enode with

        | Binding _ | Match _ | Sequence _ | Do _ -> fst (trans_neutral_expr Negative e)

        | Variable v -> trans_var Negative v

        | Call { func; arg } ->
          let openvar = generate_variable e.eloc in
          let returnvar = generate_variable e.eloc in
          Expr_Block
            (Blk ([Ins_Force
                     (returnvar,
                      (Expr_Method (
                          (Expr_Var openvar, e.eloc),
                          (Call, e.eloc),
                          [trans_pos_expr arg])
                      ,e.eloc)), func.eloc]
                 , (Expr_Var returnvar, e.eloc)
                 , e.eloc ))
        | Lambda { arg; body } ->
          let arg = trans_binder Positive arg in
          Expr_Get
            [ GetPatTag
                ( (Call, e.eloc)
                , [ arg ]
                , trans_neg_expr body
                , body.eloc)]
        | FunctionRec { var; arg; body } ->
          let var = trans_binder Positive var in
          let arg = trans_binder Positive arg in
          Expr_Rec
            (var, (Expr_Get
                     [GetPatTag
                        ((Call, e.eloc), [arg],
                         (Expr_Thunk (trans_pos_expr body), body.eloc),
                         body.eloc)
                     ],
                   e.eloc))

        | BindMonadic (x, f, eff)
          -> Expr_Eff ((Eff_Bind, trans_eff eff), [trans_neg_expr x; trans_neg_expr f])
        | Return (e,eff)
          -> Expr_Eff ((Eff_Ret, trans_eff eff), [trans_neg_expr e])
        | If (i,t,e,eff)
          -> Expr_Eff ((Eff_If, trans_eff eff), List.map trans_neg_expr [i;t;e])
        | Get eff
          -> Expr_Eff ((Eff_Get, trans_eff eff), [])
        | Set (e, eff)
          -> Expr_Eff ((Eff_Set, trans_eff eff), [trans_neg_expr e])
        | RunState (e,init,eff)
          -> Expr_Eff ( (Eff_RunST, trans_eff eff), [trans_neg_expr e; trans_neg_expr init] )
        | LiftState (e, eff)
          -> Expr_Eff ((Eff_liftST, trans_eff eff), [trans_neg_expr e])
        | ThrowEx (e,eff)
          -> Expr_Eff ((Eff_throw, trans_eff eff), [trans_neg_expr e])
        | RunCatch (e, eff)
          -> Expr_Eff ((Eff_RunExn, trans_eff eff), [trans_neg_expr e])
        | LiftEx (e, eff)
          -> Expr_Eff ((Eff_liftExn, trans_eff eff), [trans_neg_expr e])
        | ForM (x,e,eff) ->
          Expr_Eff ((Eff_iter, trans_eff eff), [trans_neg_expr x; trans_neg_expr e])

        | Litteral _ | Tuple _ | Construct _ | CallUnary _ | CallBinary _ ->
          Expr_Thunk (trans_pos_expr e)

      ), e.eloc )

  and trans_match_case pol case =
    let conseq = trans_neutral_expr pol case.consequence in
    let conseq_loc = case.consequence.eloc in
    let ptt_loc = case.pattern.ploc in
    match case.pattern.pnode with
    | LitteralPattern litt ->
      (match litt with
       | Integer x -> MatchPatTag (Int_Litt x, [], conseq, conseq_loc)
       | Boolean x -> MatchPatTag (trans_boolean x, [], conseq, conseq_loc)
       | Unit -> MatchPatTag (Unit, [], conseq, conseq_loc))
    | TuplePattern _ -> MatchPatTag (Tuple, getPatternVariable case, conseq, conseq_loc)
    | ConstructorPattern ptt ->
      MatchPatTag
        (Cons_Named ptt.constructor_ident, getPatternVariable case, conseq, conseq_loc)
    | VarPattern x -> MatchPatVar ((x, ptt_loc), conseq, conseq_loc)
    | WildcardPattern -> MatchPatVar (generate_variable ptt_loc, conseq, conseq_loc)

  and getPatternVariable case =
    let step pt =
      match pt.pnode with
      | VarPattern x -> x, pt.ploc
      | WildcardPattern -> generate_variable pt.ploc
      | _ ->
        Misc.fatal_error
          "Converting from ML to CBPV"
          "Pattern Unhandled : Pattern Containig Non Variable"
          ~loc:pt.ploc
    in
    match case.pattern.pnode with
    | TuplePattern ptt -> List.map step ptt
    | ConstructorPattern ptt -> List.map step ptt.content
    | _ -> Misc.fatal_error
             "Converting from ML to CBPV"
             "DeepMatch Pattern Unhandled"
             ~loc:case.pattern.ploc

  let thunk loc t = (Typ_App ((Typ_Thunk, loc), [t]), loc)
  let closure loc t = (Typ_App ((Typ_Closure Lin, loc), [t]), loc)
  let pair loc a b =  (Typ_App ((Typ_Tuple, loc), [a;b]), loc)
  let sum loc a b =  (Typ_App ((Typ_Sum, loc), [a;b]), loc)
  let func loc a b =  (Typ_App ((Typ_Fun, loc), [b;a]), loc)

  let rec trans_monad (eff:AstML.effect) t loc =
    match eff with
    | Ground -> closure loc (thunk loc t)
    | State (s, eff) ->
      let s = trans_type s in
      closure loc (func loc s (trans_monad eff (thunk loc (pair loc s t)) loc))
    | Except (e, eff) ->
      let e = trans_type e in
      trans_monad eff (sum loc e t) loc
  and trans_type t = match t.etype with
    | TypeMonadic (eff,t') ->  trans_monad eff (trans_type t') t.tloc
    | TypeInt -> Typ_App ((Typ_Int, t.tloc), []), t.tloc
    | TypeBool -> Typ_App ((Typ_Bool, t.tloc), []), t.tloc
    | TypeUnit -> Typ_App ((Typ_Unit, t.tloc), []), t.tloc
    | TypeTuple x -> Typ_App ((Typ_Tuple, t.tloc), List.map trans_type x), t.tloc
    | TypeSum x -> Typ_App ((Typ_Sum, t.tloc), List.map trans_type x), t.tloc
    | TypeDefined defined -> Typ_Var (String.capitalize_ascii defined), t.tloc
    | TypeVar vartype -> Typ_Var (String.capitalize_ascii vartype), t.tloc
    | TypeConstructor x -> Typ_App (trans_type x.to_build, List.map trans_type x.parameters), t.tloc
    | TypeLambda { arg; return_type } ->
      closure t.tloc (func t.tloc (trans_type arg) (thunk t.tloc (trans_type return_type)))

  let trans_newconstructor_case case =
    Constructor_Def {
      name = case.constructor_ident;
      parameters = [];
      arguments = List.map trans_type case.c_of;
      equations = []
    }

  type temp =
    | NewTypeDef of program_item
    | NewGlobal of instruction

  let trans_def def =
    let loc = def.dloc in
    match def.dnode with
    | TypeDef newtype ->
      NewTypeDef
        (Typ_Def
           ( String.capitalize_ascii newtype.basic_ident
           , List.map (fun elem -> String.capitalize_ascii elem, Pos) newtype.parameters
           , Def_Datatype (List.map trans_newconstructor_case newtype.constructors)
           , loc ))
    | VariableDef newglb ->
      let var = trans_binder Positive newglb.var in
      NewGlobal (Ins_Let (var, trans_pos_expr newglb.init), loc)


  let rec trans_prog (glbvarls, program_items) nodes =
    match nodes with
    | Def d :: nodes' ->
      (match trans_def d with
       | NewTypeDef newtype -> trans_prog (glbvarls, newtype :: program_items) nodes'
       | NewGlobal newglb ->  trans_prog (newglb :: glbvarls, program_items) nodes')
    | (Expr e) :: [] -> (glbvarls, program_items, trans_pos_expr e)
    | _ -> assert false (* TODO *)


  let trans_prog p =
    let glbVar, progItemLs, last_expr = trans_prog ([], []) p in
    Prog (List.rev progItemLs @ [ Do (Blk (List.rev glbVar, last_expr, dummy_pos)) ])

end
