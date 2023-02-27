Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  val- f : (Fun T__3 -> (Thunk T__3)) =
    bind/cc- a__28 : (Fun T__3 -> (Thunk T__3)) ->
      cmd- : (Fun T__3 -> (Thunk T__3)) val =
        match
          | this.call(y : T__3).ret(a : (Thunk T__3)) -> thunk(y).ret(a)
        end
      stk =
        this.ret(a__28)
      end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  val- f : (Fix T__4) =
    bind/cc- a__29 : (Fix T__4) ->
      cmd- : (Fix T__4) val =
        match this.fix(x : (Exp (Fix T__4))).ret(a : T__4) ->
          x.unbox(Exp).fix().ret(a)
      stk =
        this.ret(a__29)
      end

Test with user sorts
  $ autobill -t sorts.bill
  decl sort nat
  decl sort res
  
  decl type N_to_r : res
  decl type R_to_n : nat
  decl type R0 : res
  type N0 : nat = (R_to_n R0)
  type R1 : res = (N_to_r N0)
  type R2 : res = (N_to_r (R_to_n R0))
  decl type R_to_pos : +
  decl val+ x : (R_to_pos (N_to_r (R_to_n R0)))
  val+ y : (R_to_pos (N_to_r (R_to_n R0))) =
    bind/cc+ a__17 : (R_to_pos (N_to_r (R_to_n R0))) ->
      x.ret(a__17)

Test on the swap function f(x,y) = (y,x):
  $ autobill -t swap.bill
  val- swap : (Fun (T__10 * T__12) -> (Thunk (T__12 * T__10))) =
    bind/cc- a__52 : (Fun (T__10 * T__12) -> (Thunk (T__12 * T__10))) ->
      cmd- : (Fun (T__10 * T__12) -> (Thunk (T__12 * T__10))) val =
        match
          | this.call(t : (T__10 * T__12)).ret(a : (Thunk (T__12 * T__10))) -> t.match
                                                                                 | tuple(x : T__10, y : T__12) -> thunk(tuple(y, x)).ret(a),end
        end
      stk =
        this.ret(a__52)
      end
