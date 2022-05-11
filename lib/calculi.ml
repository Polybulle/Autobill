open Ast
open Types

module LAMECacl = struct
  module MyTypes = FullTypes
  include LCalc (MyTypes)
end


module PreLAMECacl = struct
  module Types = PreTypes
  module Calc = LCalc (Types)
  module Program = Program.Program
end
