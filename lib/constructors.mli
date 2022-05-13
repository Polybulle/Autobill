open Vars

  type 't pos_type_cons
  val unit_t : 'a pos_type_cons
  val zero : 'a pos_type_cons
  val prod : 'a -> 'a -> 'a pos_type_cons
  val sum : 'a -> 'a -> 'a pos_type_cons
  val posvar : TyVar.t -> 'a pos_type_cons
  val postype : TyVar.t -> 'a list -> 'a pos_type_cons
  val string_of_pos_type_cons : ('a -> string) -> 'a pos_type_cons -> string

  type 't neg_type_cons
  val top : 'a neg_type_cons
  val bottom : 'a neg_type_cons
  val func : 'a -> 'a -> 'a neg_type_cons
  val choice : 'a -> 'a -> 'a neg_type_cons
  val negvar : TyVar.t -> 'a neg_type_cons
  val negtype : TyVar.t -> 'a list -> 'a neg_type_cons
  val string_of_neg_type_cons : ('a -> string) -> 'a neg_type_cons -> string

  type 'x constructor
  val unit : 'a constructor
  val pair : 'a -> 'a -> 'a constructor
  val fst : 'a -> 'a constructor
  val snd : 'a -> 'a constructor
  val poscons : ConsVar.t -> 'a list -> 'a constructor
  val string_of_constructor : ('a -> string) -> 'a constructor -> string
  val definition_of_constructor : ('a -> string) -> 'a constructor -> string

  type ('x, 'a) destructor
  val call : 'a -> 'b -> ('a, 'b) destructor
  val yes : 'a -> ('b, 'a) destructor
  val no : 'a -> ('b, 'a) destructor
  val negcons : ConsVar.t -> 'a list -> 'b -> ('a, 'b) destructor
  val string_of_destructor :
    ('a -> string) -> ('b -> string) -> ('a, 'b) destructor -> string
  val definition_of_destructor :
    ('a -> string) -> ('a, 'a) destructor -> string
