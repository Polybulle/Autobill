Test type inference on the identity : (fun t (shift- t))
  $ autobill infer id.bill
  term<-> f<12> : (fun t<64> (shift- t<64>)) =
    match
      case this.call(y<15> : t<64>).ret() : (shift- t<64>) ->
        step-
          match
            case this.shift-().ret() : t<64> -> y<15>.ret()
          end
        : (shift- t<64>)
        into
          this.ret()
        end
    end

Test on the trivial fixpoint
  $ autobill infer fixpoint.bill
  term<-> f<12> : (fix a<42>) =
    match this.fix(x<13> : (fix a<42>)).ret() : (fix a<42>) -> x<13>.unbox(exp)
      .ret()
