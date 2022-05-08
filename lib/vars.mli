val pp_sexp : string -> string list -> string
val pp_texp : string -> string list -> string
module type Sorted =
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end
module type AllVars =
  sig
    module Var : Sorted
    module CoVar : Sorted
    module TyVar : Sorted
    module ConsVar : Sorted
    module SoVar : Sorted
  end
module StringVar :
  sig
    module Var :
      sig
        type t = string
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module CoVar :
      sig
        type t = string
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module TyVar :
      sig
        type t = string
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module ConsVar :
      sig
        type t = string
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
    module SoVar :
      sig
        type t = string
        val of_string : string -> t
        val to_string : t -> string
        val fresh : unit -> t
      end
  end
