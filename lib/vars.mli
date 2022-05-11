val pp_sexp : string -> string list -> string
val pp_texp : string -> string list -> string
module type Sorted =
  sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end
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
