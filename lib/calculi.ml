open Ast
open Vars
open Types
open Constructors

module EmptyCalc = struct
  module Types = FullTypes (StringVar) (Empty)
  include Types
  include Types.MyVars
  include Types.Constructors
  include LCalc (Types)
end

module ILLCalc = struct
  module Types = FullTypes (StringVar) (ILL)
  include Types
  include Types.MyVars
  include Types.Constructors
  include LCalc (Types)
end
