open Ast
open Vars
open Types
open Constructors

module EmptyCalc = struct
  module Types = FullTypes (StringVar) (Empty)
  include StringVar
  include Empty
  include Types
  include LCalc (Types)
end

module ILLCalc = struct
  module Types = FullTypes (StringVar) (ILL)
  include StringVar
  include ILL
  include Types
  include LCalc (Types)
end

module LAMECacl = struct
  module LAME = LAME (StringVar)
  module Types = FullTypes (StringVar) (LAME)
  include StringVar
  include LAME
  include Types
  include LCalc (Types)
end

module PreLAMECacl = struct
  module LAME = LAME (StringVar)
  module Types = PreTypes (StringVar) (LAME)
  include StringVar
  include LAME
  include Types
  include LCalc (Types)
end
