open Lcbpv
open Cst
open Vars

let mk_var _ = Var.to_string ~debug:true (Var.fresh ())
let mk_covar _ = CoVar.to_string ~debug:true (CoVar.fresh ())

let rec go_eff
    loc
    ((m,e) : eff_macro)
    (args : V.t list)

  = match m,e,args with

  | Eff_Ret, Ground, [e (* A+ *)] -> (* T(A) *)
    let a = mk_covar "a" in
    V.P.(thunk (var a)) (e |~| S.ret ~loc a)


  | Eff_Ret, State eff, [e (* A+ *)] -> (* S+ -> M(S*A) *)
    let s = mk_var "s" in
    let x = mk_var "x" in
    let y = mk_var "y" in
    let a = mk_covar "a" in
    V.macro_fun ~loc [s,None]
      (V.P.(thunk (var a))
         (e |~| S.bind ~loc x
            (V.C.tuple [V.var ~loc s; V.var ~loc x] |~| S.bind ~loc y
               (go_eff loc (Eff_Ret, eff) [V.var ~loc y] |~| S.ret ~loc a))))


  | Eff_Ret, Exn eff, [e (* A+ *)] -> (* M(E+A) *)
    go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 1 2 e]


  | Eff_Bind, Ground, [x (* TA+ *); f (* A -> TB+ *)] -> (* TB *)
    let a = mk_covar "a" in
    let xx = mk_var "xx" in
    V.P.(thunk (var a))
      (x |~| S.D.thunk ~loc @@ S.bind ~loc xx
         (f |~| S.D.call ~loc [V.var xx] @@ S.D.thunk ~loc @@ (S.ret ~loc a)))


  | Eff_Bind, State eff, [x (* S -> M(S*A) *); f (* A -> S -> M(S*B) *)] -> (* S -> M(S*B) *)
    let s = mk_var "s" in
    let y = mk_var "y" in
    let t = mk_var "t" in
    let tup = mk_var "tup" in
    let a = mk_covar "a" in
    let b = mk_covar "b" in
    let c = mk_covar "c" in
    let d = mk_covar "d" in
    (* passer de xx : S -> M(S*A) et s : S à M(S*A) *)
    let xx = V.bindcc ~loc c (x |~| S.D.call ~loc [V.var ~loc s] @@ S.ret ~loc c) in
    (* passer de f : A -> S -> M(S*B) à S*A -> M(S*B) *)
    let ff =
      (V.P.(call [var tup] (var d))
         (V.var ~loc tup |~| S.P.(tuple [var t; var y])
            (f |~| S.D.call ~loc [V.var ~loc y] @@ S.D.call ~loc [V.var ~loc t] @@ S.ret ~loc d))) in
    (* appeler bindM pour avoir M(S*B) *)
    V.bindcc a (V.P.(call [var s] (var b)) (go_eff loc (Eff_Bind, eff) [xx; ff] |~| S.ret ~loc b)
                |~| S.ret a)



  | Eff_Bind, Exn eff, [x (* M(E+A) *); f (* A -> M(E+B) *)] -> (* M(E+B) *)
    let a = mk_covar "a"
    and e = mk_var "e"
    and ex = mk_var "ex"
    and xx = mk_var "xx" in
    (* passer de f : A -> M(E+B) vers E+A -> M(E+B) *)
    let ff =
      V.P.(call [var ex] (var a))
        (V.var ~loc ex |~| S.P.(branch ~loc [
             inj 0 2 (var e)
               (go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 0 2 (V.var ~loc e)] |~| S.ret ~loc a);
             inj 1 2 (var xx)
               (f |~| S.D.call [V.var ~loc xx] @@ S.ret ~loc a)])) in
    (* appeler bindM sur x et ff pour avoir M(E+B) *)
    go_eff loc (Eff_Bind, eff) [x; ff]


  | Eff_liftST, eff, [e (* MA *)] -> (* S -> M(S*A) *)
    let x = mk_var "x" in
    let b = mk_covar "b" in
    let a = mk_covar "a" in
    let s = mk_var "s" in
    let tup = mk_var "tup" in
    let y = mk_var "y" in
    (* cont s : A -> M(S*A) *)
    let cont s =
      V.P.(call [var x] (var b))
        (V.C.tuple ~loc [s; V.var ~loc x] |~| S.bind ~loc tup
           (go_eff loc (Eff_Ret, eff) [V.var ~loc tup] |~| S.ret ~loc b)) in
    V.P.(call [var s] (var a))
      (cont (V.var ~loc s) |~| S.bind ~loc y
         (go_eff loc (Eff_Bind, eff) [e; V.var ~loc y] |~| S.ret ~loc a))


  | Eff_liftExn, eff, [e (* MA *)] -> (* M(E+A) *)
    let x = mk_var "x" in
    let a = mk_covar "a" in
    let cont =
      V.P.(call [var x] (var a))
        (go_eff loc (Eff_Ret, eff) [V.C.inj 1 2 (V.var x)] |~| S.ret ~loc a) in
    go_eff loc (Eff_Bind, eff) [e; cont]


  | Eff_RunExn, eff, [e (* M(A+A) *)] -> (* MA *)
    let sum = mk_var "sum" in
    let a = mk_covar "a" in
    let b = mk_covar "b" in
    let x = mk_var "x" in
    let y = mk_var "y" in
    let z = mk_var "z" in
    let cont =
      V.P.(call [var sum] (var a))
        (V.bindcc ~loc b
           (V.var ~loc sum |~| S.P.(branch ~loc [
                inj 0 2 (var x) (V.var ~loc x |~| S.ret ~loc b);
                inj 1 2 (var y) (V.var ~loc y |~| S.ret ~loc b)]))
         |~|
         S.bind ~loc z (go_eff loc (Eff_Ret, eff) [V.var ~loc z] |~| S.ret ~loc a)) in
    go_eff loc (Eff_Bind, eff) [e; cont]


  | Eff_RunST, eff, [e (* S -> M(S*A) *); s (* TS *)] -> (* MA *)
    let tup = mk_var "tup" in
    let a = mk_covar "a" in
    let b = mk_covar "b" in
    let c = mk_covar "c" in
    let ss = mk_var "s" in
    let x = mk_var "x" in
    let y = mk_var "y" in
    let z = mk_var "z" in
    (* S*A -> MA *)
    let cont =
      V.P.(call [var tup] (var a))
        ((V.bindcc ~loc b
            (V.var ~loc tup
             |~|
             S.P.(tuple [var ss; var x] (V.var ~loc x |~| S.ret ~loc b))))
         |~|
         S.bind ~loc y (go_eff loc (Eff_Ret, eff) [V.var ~loc y] |~| S.ret ~loc a)) in
    V.bindcc ~loc c
      (s |~| S.D.thunk ~loc @@ S.bind ~loc ss
         (e |~| S.D.call [V.var ~loc ss] @@ S.bind ~loc z
            (go_eff loc (Eff_Bind, eff) [V.var ~loc z; cont] |~| S.ret ~loc c)))


  | Eff_Get, State eff, [] -> (* S -> M(S*S) *)
    let s = mk_var "s" in
    let a = mk_covar "a" in
    V.P.(call [var s] (var a))
      (go_eff loc (Eff_Ret, eff) [V.C.tuple ~loc [V.var ~loc s; V.var ~loc s]]
       |~| S.ret ~loc a)


  | Eff_Set, State eff, [s (* TS *)] -> (* S -> M(S*1) *)
    let dummy = mk_var "dummy" in
    let ss = mk_var "ss" in
    let a = mk_covar "a" in
    V.P.(call [var dummy] (var a))
      (s |~| S.D.thunk ~loc @@ S.bind ~loc ss
         (go_eff loc (Eff_Ret, eff) [V.C.tuple ~loc [V.var ~loc ss; V.C.unit ~loc ()]]
          |~| S.ret ~loc a))


  | Eff_throw, Exn eff, [e (* E *)] -> (* M(E+A) *)
    let a = mk_covar "a" in
    let x = mk_var "x" in
    V.bindcc ~loc a
      (e |~| S.D.thunk ~loc @@ S.bind x
         (go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 0 2 (V.var ~loc x)] |~| S.ret ~loc a))


  | Eff_If, _, [i (* T(bool) *); t (* MA *); e (* MA *)] -> (* M(S*A) *)
    let a = mk_covar "a" in
    V.bindcc ~loc a
      (i |~| S.D.thunk ~loc @@ S.P.branch ~loc [
          S.P.(bool true) (t |~| S.ret ~loc a);
          S.P.(bool false) (e |~| S.ret ~loc a)
        ])


  | _ -> raise (Misc.Invariant_break ("Desugaring monadic effects resulted in \
                                       syntactically invalid intermediate code", Some loc))
