open Ast
open Vars
open Types
open Constructors

module EmptyCalc = struct
  include Empty
  include StringVar
  module Types = FullTypes (StringVar) (Empty)
  include Types
  include LCalc (StringVar) (Empty) (Types)
end

module ILLCalc = struct
  include ILL
  include StringVar
  module Types = FullTypes (StringVar) (ILL)
  include Types
  include LCalc (StringVar) (ILL) (Types)
end
