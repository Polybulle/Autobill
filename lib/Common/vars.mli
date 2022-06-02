(** Here we define the different kind of variables in syntax trees. *)
(** Some originate from source code, some don't. *)

(** Exception raised when trying to export a variable that shouldn't exist *)
exception Undefined_variable of string

(** Value variables, introduced in value, stack, & commands. They don't need to
    be registered, and never raise an exception. Linking the right string
    litteral in code to its variable is left up to the internalisation pass. *)
module Var : sig
    type t

    (** Due to name shadowing the argument to [of_string] is assumed to be fresh
        and is given a different variable for each call *)
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

(** Type variables, introduced in values, stacks, commands, type definition and
    annotation. They don't need to be registered, and never raise an exception.
    Linking the right string litteral in code to its variable is left up to the
    internalisation pass. *)
module TyVar : sig
    type t

    (** Due to name shadowing the argument to [of_string] is assumed to be fresh
        and is given a different variable for each call *)
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

(** Type constructors, ie. names of user-defined types *)
module TyConsVar : sig
  type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
end

(** Definition variables, i.e. the names given to top-level expressions *)
module DefVar : sig
    type t

    (** Due to name shadowing the argument to [of_string] is assumed to be fresh
        and is given a different variable for each call *)
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

(** Constructor and destructor variables must be defined only once in the
    prelude, and registered at that point. *)
module ConsVar : sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

module DestrVar : sig
    type t
    val of_string : string -> t
    val to_string : t -> string
    val fresh : unit -> t
  end

(** Internal polarity variables, used for polarity inference. There are no
    polarity variables in the source language, so they are all fresh. *)
module PolVar : sig
  type t
  val to_string : t -> string
  val fresh : unit -> t
end

(** Internal sort variables, used for polarity inference. There are no sort
    variables in the source language, so they are all fresh. *)
module SortVar : sig
  type t
  val to_string : t -> string
  val fresh : unit -> t
end
