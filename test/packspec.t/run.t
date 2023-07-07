Test a simple pack/spec program.

  $ autobill -M -s sorts.bill
  data T__21 (A__27 : +) =
    | c__28()
  comput U__22 (A__29 : -) =
    | this.d__30().ret(A__29)
  decl type C__23 : -
  decl type D__24 : +
  decl val- x__43 : C__23
  decl val+ z__45 : D__24
  val+ test__47 : T__51 =
    c__28()
  val- test0__52 : T__63 =
    match
      | this.d__30().ret(a__54 : T__55) -> x__43.ret(a__54)
    end
  val- test1__64 : T__76 =
    bind/cc- a__67 : T__66 ->
      x__43.d__30().ret(a__67)


  $ autobill -M -t types.bill
  comput Id__21 =
    | this.inst__25<A__24 : +>().ret((Fun A__24 -> (Thunk A__24)))
  val- id2__38 : Id__21 =
    match
      | this.inst__25<B__40 : +>().ret(a__41 : (Fun T__57 -> (Thunk T__57))) ->
        cmd- : (Fun T__57 -> (Thunk T__57)) val =
         match
           | this.call(x__46 : T__57).ret(b__48 : (Thunk T__57)) ->
             cmd- : (Thunk T__57) val =
              match
                | this.thunk().ret(c__54 : T__57) -> x__46.ret(c__54)
               end
             stk =
              this.ret(b__48)
             end
          end
        stk =
         this.ret(a__41)
        end
    end
