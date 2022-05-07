open Vars

module type Constructors =
sig
  type 'x constructor
  type 't pos_type_cons
  type 't neg_type_cons
  type ('x, 'a) destructor
  val string_of_constructor : ('x -> string) -> 'x constructor -> string
  val string_of_pos_type_cons : ('t -> string) -> 't pos_type_cons -> string
  val string_of_neg_type_cons : ('t -> string) -> 't neg_type_cons -> string
  val string_of_destructor :
    ('x -> string) -> ('a -> string) -> ('x, 'a) destructor -> string
end

module TextConstructors : sig
  type 'x constructor = string * 'x list
  type 't pos_type_cons = string * 't list
  type 't neg_type_cons = string * 't list
  type ('x, 'a) destructor = string * 'a * 'x list
  val string_of_constructor : ('x -> string) -> 'x constructor -> string
  val string_of_pos_type_cons : ('t -> string) -> 't pos_type_cons -> string
  val string_of_neg_type_cons : ('t -> string) -> 't neg_type_cons -> string
  val string_of_destructor :
    ('x -> string) -> ('a -> string) -> ('x, 'a) destructor -> string
end

module Empty : sig
  type 'x constructor = |
  type 't pos_type_cons = |
  type 't neg_type_cons = |
  type ('x, 'a) destructor = |
  val fail : 'a -> 'b
  val string_of_constructor : 'a -> 'b
  val string_of_destructor : 'a -> 'b
  val string_of_pos_type_cons : 'a -> 'b
  val string_of_neg_type_cons : 'a -> 'b
end

module ILL : sig
  type 't pos_type_cons = Unit | Zero | Prod of 't * 't | Sum of 't * 't
  val zero : 'a pos_type_cons
  val prod : 'a -> 'a -> 'a pos_type_cons
  val sum : 'a -> 'a -> 'a pos_type_cons
  val string_of_pos_type_cons : ('a -> string) -> 'a pos_type_cons -> string

  type 't neg_type_cons = Top | Bottom | Fun of 't * 't | Choice of 't * 't
  val top : 'a neg_type_cons
  val bottom : 'a neg_type_cons
  val func : 'a -> 'a -> 'a neg_type_cons
  val choice : 'a -> 'a -> 'a neg_type_cons
  val string_of_neg_type_cons : ('a -> string) -> 'a neg_type_cons -> string

  type 'x constructor = Unit | Pair of 'x * 'x | Fst of 'x | Snd of 'x
  val unit : 'a constructor
  val pair : 'a -> 'a -> 'a constructor
  val fst : 'a -> 'a constructor
  val snd : 'a -> 'a constructor
  val string_of_constructor : ('a -> string) -> 'a constructor -> string

  type ('x, 'a) destructor = Call of 'x * 'a | Yes of 'a | No of 'a
  val call : 'a -> 'b -> ('a, 'b) destructor
  val yes : 'a -> ('b, 'a) destructor
  val no : 'a -> ('b, 'a) destructor
  val string_of_destructor :
    ('a -> string) -> ('b -> string) -> ('a, 'b) destructor -> string
end


module LAME (Vars : AllVars) : sig
  type 't pos_type_cons
  val unit_t : 'a pos_type_cons
  val zero : 'a pos_type_cons
  val prod : 'a -> 'a -> 'a pos_type_cons
  val sum : 'a -> 'a -> 'a pos_type_cons
  val posvar : Vars.TyVar.t -> 'a pos_type_cons
  val postype : Vars.TyVar.t -> 'a list -> 'a pos_type_cons
  val string_of_pos_type_cons : ('a -> string) -> 'a pos_type_cons -> string

  type 't neg_type_cons
  val top : 'a neg_type_cons
  val bottom : 'a neg_type_cons
  val func : 'a -> 'a -> 'a neg_type_cons
  val choice : 'a -> 'a -> 'a neg_type_cons
  val negvar : Vars.TyVar.t -> 'a neg_type_cons
  val negtype : Vars.TyVar.t -> 'a list -> 'a neg_type_cons
  val string_of_neg_type_cons : ('a -> string) -> 'a neg_type_cons -> string

  type 'x constructor
  (*val unit : 'a constructor*)
  val unit : 'a constructor
  val pair : 'a -> 'a -> 'a constructor
  val fst : 'a -> 'a constructor
  val snd : 'a -> 'a constructor
  val poscons : Vars.ConsVar.t -> 'a list -> 'a constructor
  val string_of_constructor : ('a -> string) -> 'a constructor -> string

  type ('x, 'a) destructor
  val call : 'a -> 'b -> ('a, 'b) destructor
  val yes : 'a -> ('b, 'a) destructor
  val no : 'a -> ('b, 'a) destructor
  val negcons : Vars.ConsVar.t -> 'a list -> 'b -> ('a, 'b) destructor
  val string_of_destructor :
    ('a -> string) -> ('b -> string) -> ('a, 'b) destructor -> string
end
