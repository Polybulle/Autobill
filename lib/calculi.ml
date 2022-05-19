module PreLAMECalc = struct
  include Vars
  include Constructors
  include Types
  include Ast
  include Printer
      let x = Var.of_string "x"
      let _ = call (x, None) None |=> (V.cons unit |~| S.ret ())
end
