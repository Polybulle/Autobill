Test type inference on the identity : (fun t (shift- t))
  $ autobill infer id.bill
  term<-> f<12> : (fun t<16> -> (shift- t<16>)) =
    match
      case this.call(y<15> : t<16>).ret() : (shift- t<16>) ->
        step-
          match
            case this.shift-().ret() : t<16> -> y<15>.ret()
          end
        : (shift- t<16>)
        into
          this.ret()
        end
    end

Test on the trivial fixpoint
  $ autobill infer fixpoint.bill
  term<-> f<12> : (fix a<54>) =
    match this.fix(x<13> : (fix a<54>)).ret() : (fix a<54>) -> x<13>.unbox(exp)
      .ret()
