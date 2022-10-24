Test type inference on the identity : (fun t (shift- t))
  $ autobill -t id.bill
  term<-> f__6 : (fun t__9 -> (shift- t__9)) =
    match
      case this.call(y__8 : t__9).ret(a__10 : (shift- t__9)) ->
        step-
          match
            case this.shift-().ret(b__15 : t__9) -> y__8.ret(b__15)
          end
        : (shift- t__9)
        into
          this.ret(a__10)
        end
    end

Test on the trivial fixpoint
  $ autobill -t fixpoint.bill
  (46 : 48)
  (47 : ρ0)
  (48 : 52)
  (49 : (ρ0 (box<exp> 48)))
  (50 : ρ0)
  (51 : ρ0)
  (52 : (ρ0 (fix 47)))
  (53 : (ρ0 (box<exp> 52)))
  (54 : 48)
  (55 : 50)
  (56 : 48)
  (57 : 50)
  (58 : 48)
  (59 : ρ0)
  (60 : 46)
  
  Fatal error: exception Autobill.UnionFind.Unify(52, 49)
  [2]
