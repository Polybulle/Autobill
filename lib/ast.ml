open Vars
open Constructors
open Types

module LCalc (Vars : Var) (Constructors : Constructors) = struct
  include Vars
  include Constructors
  include Types (Vars) (Constructors)

  type pattern = (Var.t * typ) constructor
  type copattern = (Var.t * typ, CoVar.t * typ) destructor
  let string_of_pattern p = string_of_constructor string_of_binding p
  let string_of_copattern p = string_of_destructor string_of_binding string_of_cobinding p

  type value =
    | Var of Var.t
    | Bind of CoVar.t * negtype * command
    | Box of box_kind * CoVar.t * typ * command
    | Cons of value constructor
    | CoDestr of (copattern * command) list
  and stack =
    | CoVar of CoVar.t
    | CoBind of Var.t * postype * command
    | CoBox of box_kind * stack
    | Destr of (value, stack) destructor
    | CoCons of (pattern * command) list
  and command =
    | CUkn of term * environment
    | CPos of term * stack
    | CNeg of value * environment
  and term =
    | Force of CoVar.t * postype * command
    | Val of value
  and environment =
    | CoForce of Var.t * negtype *command
    | Stack of stack

  let rec string_of_value = function
    | Var v -> Var.to_string v
    | Bind (v,t,c) -> pp_sexp "let" [
        CoVar.to_string v;
        string_of_negtype t;
        string_of_command c]
    | Box (k,v,t,c) -> pp_sexp "box" [
        (box_kind_to_string k);
        (CoVar.to_string v);
        (string_of_type t);
        (string_of_command c)
      ]
    | Cons c -> string_of_constructor string_of_value c
    | CoDestr patts ->
      let string_of_case (p,c) = pp_sexp "case" [
          string_of_copattern p;
          string_of_command c
        ] in
      pp_sexp "match" (List.map string_of_case patts)
  and string_of_stack = function
    | CoVar v -> CoVar.to_string v
    | CoBind (v,t,c) -> pp_sexp "let" [
        Var.to_string v;
        string_of_postype t;
        string_of_command c
      ]
    | CoBox (k,s) -> pp_sexp "unbox" [
        box_kind_to_string k;
        string_of_stack s
      ]
    | Destr c -> string_of_destructor string_of_value string_of_stack c
    | CoCons patts ->
      let string_of_case (p,c) = pp_sexp "case" [
          string_of_pattern p;
          string_of_command c
        ] in
      pp_sexp "match" (List.map string_of_case patts)
  and string_of_term = function
    | Force (a,t,c) -> pp_sexp "force" [
        CoVar.to_string a;
        string_of_postype t;
        string_of_command c
      ]
    | Val v -> string_of_value v
  and string_of_env = function
    | CoForce (x,t,c) -> pp_sexp "force" [
        Var.to_string x;
        string_of_negtype t;
        string_of_command c
      ]
    | Stack s -> string_of_stack s
  and string_of_command = function
    | CUkn (t,e) -> pp_sexp "ambiguous" [string_of_term t; string_of_env e]
    | CPos (t,s) -> pp_sexp "jump" [string_of_term t; string_of_stack s]
    | CNeg (v,e) -> pp_sexp "call" [string_of_value v; string_of_env e]

  module V = struct
    type t = value
    let var x = Var(x)
    let bind x t c = Bind (x,t,c)
    let box a t c = Box (Linear,a,t,c)
    let aff a t c = Box (Affine,a,t,c)
    let exp a t c = Box (Exponential,a,t,c)
    let cons c = Cons c
    let cocase l = CoDestr l
  end

  module S = struct
    type t = stack
    let var x = CoVar (x)
    let bind x t c = CoBind (x,t,c)
    let box s = CoBox (Linear, s)
    let aff s = CoBox (Affine, s)
    let exp s = CoBox (Exponential, s)
    let destr c = Destr c
    let case l = CoCons l
  end

  module T = struct
    type t = term
    let str_bind x t c = Force (x,t,c)
    let vall v = Val v
  end

  module E = struct
    type t = environment
    let str_bind x t c = CoForce (x,t,c)
    let stack s = Stack s
  end

  module C = struct
    type t = command
    let (||) t e = CUkn (t, e)
    let (|**|) v s = CUkn (T.vall v, E.stack s)
    let (|+|) (t : T.t) (s : S.t) = CPos (t,s)
    let (|-|) (v : V.t) (e : E.t) = CNeg (v,e)
    let (|>|) (v : V.t) (s : S.t) = (T.vall v) |+| s
    let (|<|) (v : V.t) (s : S.t) = v |-| (E.stack s)
  end

  let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)

  include C
end

