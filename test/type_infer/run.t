Test type inference on the identity : (fun t (shift- t))
  $ autobill infer <<EOF
  > term f =
  >   match this.call(y).ret() ->
  >      match env this.shift-().ret() in
  >      y.ret()
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