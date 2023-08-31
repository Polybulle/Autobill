open Lcbpv
open Cst

let rec go_eff
    loc
    ((m,e) : eff_macro)
    (args : 'a list)

  = match m,e,args with


  | Eff_Ret, Ground, [e (* A+ *)] -> (* T(A) *)
    V.P.(thunk (var "a")) (e |+| S.ret ~loc "a")


  | Eff_Ret, State eff, [e (* A+ *)] -> (* S+ -> M(S*A) *)
    V.macro_fun ~loc ["s",None]
      (V.P.(thunk (var "c"))
         (e |+| S.bind ~loc "x"
            (V.C.tuple [V.var ~loc "s"; V.var ~loc "x"] |+| S.bind ~loc "y"
               (go_eff loc (Eff_Ret, eff) [V.var ~loc "y"] |~| S.ret ~loc "c"))))


  | Eff_Ret, Exn eff, [e (* A+ *)] -> (* M(E+A) *)
    go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 1 2 e]


  | Eff_Bind, Ground, [x (* TA+ *); f (* A -> TB+ *)] -> (* TB *)
    V.P.(thunk (var "b")) @@
    (x |~| S.D.thunk ~loc @@ S.bind ~loc "xx" @@
     (f |~| S.D.call ~loc [V.var "xx"] @@ S.D.thunk ~loc @@ (S.ret ~loc "b")))


  | Eff_Bind, State eff, [x (* S -> M(S*A) *); f (* A -> S -> M(S*B) *)] -> (* S -> M(S*B) *)
    let outer c = V.bindcc "a" (V.P.(call [var "s"] (var "b")) c |~| S.ret "a") in
    (* passer de xx : S -> M(S*A) et s : S à M(S*A) *)
    let xx = V.bindcc ~loc "b"
        (x |~| S.D.call ~loc [V.var ~loc "s"] @@ S.D.thunk ~loc @@ S.ret ~loc "b") in
    (* passer de f : A -> S -> M(S*B) à S*A -> M(S*B) *)
    let ff =
      (V.P.(call [var "tup"] (var "c"))
         (V.var ~loc "tup" |~| S.P.(tuple [var "t"; var "y"])
            (f |~| S.D.call ~loc [V.var ~loc "t"; V.var ~loc "y"] @@ S.ret ~loc "d"))) in
    (* appeler bindM pour avoir M(S*B) *)
    outer (go_eff loc (Eff_Bind, eff) [xx; ff] |~| S.ret ~loc "b")


  | Eff_Bind, Exn eff, [x (* M(E+A) *); f (* A -> M(E+B) *)] -> (* M(E+B) *)
    (* passer de f : A -> M(E+B) vers E+A -> M(E+B) *)
    let ff =
      V.P.(call [var "ex"] (var "a"))
        (V.var ~loc "ex" |~| S.P.(branch ~loc [
             inj 0 2 (var "e")
               (go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 0 2 (V.var ~loc "e")] |~| S.ret ~loc "a");
             inj 1 2 (var "x")
               (f |~| S.D.call [V.var ~loc "x"] @@ S.ret ~loc "a")])) in
    (* appeler bindM sur x et ff pour avoir M(E+B) *)
    go_eff loc (Eff_Bind, eff) [x; ff]


  | Eff_liftST, State eff, [e (* MA *)] -> (* S -> M(S*A) *)
    (* cont s : A -> M(S*A) *)
    let cont s =
      V.P.(call [var "x"] (var "b"))
        (V.C.tuple ~loc [s; V.var ~loc "x"] |~| S.bind ~loc "tup"
           (go_eff loc (Eff_Ret, eff) [V.var ~loc "tup"] |~| S.ret ~loc "b")) in
    V.P.(call [var "s"] (var "a"))
      (cont (V.var ~loc "s") |~| S.bind ~loc "cont"
         (go_eff loc (Eff_Bind, eff) [e; V.var ~loc "cont"] |~| S.ret ~loc "a"))


  | Eff_liftExn, Exn eff, [e (* MA *)] -> (* M(E+A) *)
    let cont =
      V.P.(call [var "x"] (var "b"))
        (go_eff loc (Eff_Ret, eff) [V.C.inj 1 2 (V.var "x")] |~| S.ret ~loc "b") in
    go_eff loc (Eff_Bind, eff) [e; cont]


  | Eff_RunExn, Exn eff, [e (* M(A+A) *)] -> (* MA *)
    let cont =
      V.P.(call [var "sum"] (var "a"))
        (V.bindcc ~loc "b"
           (V.var ~loc "sum" |~| S.P.(branch ~loc [
                inj 0 2 (var "x") (V.var ~loc "x" |~| S.ret ~loc "a");
                inj 1 2 (var "y") (V.var ~loc "y" |~| S.ret ~loc "a")]))
         |~|
         S.bind ~loc "z" (go_eff loc (Eff_Ret, eff) [V.var ~loc "z"] |~| S.ret ~loc "a")) in
    go_eff loc (Eff_Bind, eff) [e; cont]


  | Eff_RunST, State eff, [e (* S -> M(S*A) *); s (* S *)] -> (* MA *)
    (* S*A -> MA *)
    let cont =
      V.P.(call [var "tup"] (var "a"))
        ((V.bindcc ~loc "b"
            (V.var ~loc "tup"
             |~|
             S.P.(tuple [var "s"; var "x"] (V.var ~loc "x" |~| S.ret ~loc "b"))))
         |~|
         S.bind ~loc "y" (go_eff loc (Eff_Ret, eff) [V.var ~loc "y"] |~| S.ret ~loc "a")) in
    V.bindcc ~loc "c"
      (e |~| S.D.call [s] @@ S.bind ~loc "z"
         (go_eff loc (Eff_Bind, eff) [V.var ~loc "z"; cont] |~| S.ret ~loc "c"))


  | Eff_Get, State eff, [] -> (* S -> M(S*S) *)
    V.P.(call [var "s"] (var "a"))
      (go_eff loc (Eff_Ret, eff) [V.C.tuple ~loc [V.var ~loc "s"; V.var ~loc "s"]]
       |~| S.ret ~loc "a" )


  | Eff_Set, State eff, [s (* S *)] -> (* S -> M(S*1) *)
    V.P.(call [var "s"] (var "a"))
      (go_eff loc (Eff_Ret, eff) [V.C.tuple ~loc [s; V.C.unit ~loc ()]]
       |~| S.ret ~loc "a")


  | Eff_throw, Exn eff, [e (* E *)] -> (* M(E+A) *)
    go_eff loc (Eff_Ret, eff) [V.C.inj ~loc 0 2 e]


  | Eff_If, _, [i (* T(bool) *); t (* MA *); e (* MA *)] -> (* M(S*A) *)
    V.bindcc ~loc "a"
      (i |~| S.D.thunk ~loc @@ S.P.branch ~loc [
          S.P.(bool true) (t |~| S.ret ~loc "a");
          S.P.(bool false) (e |~| S.ret ~loc "a")
        ])


  | _ -> raise (Misc.Invariant_break ("Desugaring monadic effects resulted in \
                                       syntactically invalid intermediate code", Some loc))
