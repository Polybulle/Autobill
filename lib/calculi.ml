open Ast
open Types

module LAMECacl = struct
  module MyTypes = FullTypes
  include LCalc (MyTypes)
end


module PreLAMECalc = struct
  module MyTypes = PreTypes
  module MyCalc = LCalc (MyTypes)
  module MyProgram = Program.Program
  include Vars
  include Constructors
  include MyTypes
  include MyCalc
  include MyProgram
end
