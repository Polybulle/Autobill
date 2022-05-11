open Types
open Vars
open Constructors

let pp_sexp = Vars.pp_sexp

module LCalc (Types : ITypes)
= struct

  open Types

  type pattern = (Var.t * typ) constructor
  type copattern = (Var.t * typ, CoVar.t * typ) destructor
  type polarity = Pos | Neg | Unknown
  let string_of_pattern p = string_of_constructor string_of_binding p
  let string_of_copattern p = string_of_destructor string_of_binding string_of_cobinding p

  type value =
    | Var of Var.t
    | Bind of CoVar.t * negtype * command
    | Force of CoVar.t * postype * command
    | Box of box_kind * CoVar.t * typ * command
    | Cons of value constructor
    | Destr of (copattern * command) list
  and stack =
    | CoVar of CoVar.t
    | CoBind of Var.t * postype * command
    | CoForce of Var.t * negtype *command
    | CoBox of box_kind * stack
    | CoDestr of (value, stack) destructor
    | CoCons of (pattern * command) list
  and command = Command of polarity * value * stack


  let typ_annot s = if String.get s 0 = '{' then s else "{" ^ s ^ "}"

  let rec string_of_value = function
    | Var v -> Var.to_string v
    | Bind (v,t,c) -> pp_sexp "let" [
        CoVar.to_string v;
        typ_annot @@ string_of_negtype t;
        string_of_command c]
    | Force (a,t,c) -> pp_sexp "force" [
        CoVar.to_string a;
        typ_annot @@ string_of_postype t;
        string_of_command c
      ]
    | Box (k,v,t,c) -> pp_sexp "box" [
        (string_of_box_kind k);
        (CoVar.to_string v);
        typ_annot @@ (string_of_type t);
        (string_of_command c)
      ]
    | Cons c -> string_of_constructor string_of_value c
    | Destr patts ->
      let string_of_case (p,c) = [
        string_of_copattern p;
        string_of_command c
      ] in
      pp_sexp "match" (List.concat (List.map string_of_case patts))

  and string_of_stack = function
    | CoVar v -> CoVar.to_string v
    | CoBind (v,t,c) -> pp_sexp "let" [
        Var.to_string v;
        typ_annot @@ string_of_postype t;
        string_of_command c
      ]
    | CoForce (x,t,c) -> pp_sexp "force" [
        Var.to_string x;
        typ_annot @@ string_of_negtype t;
        string_of_command c
      ]
    | CoBox (k,s) -> pp_sexp "unbox" [
        string_of_box_kind k;
        string_of_stack s
      ]
    | CoDestr c -> string_of_destructor string_of_value string_of_stack c
    | CoCons patts ->
      let string_of_case (p,c) = [
        string_of_pattern p;
        string_of_command c
      ] in
      pp_sexp "match" (List.concat (List.map string_of_case patts))

  and string_of_command (Command (p,v,s)) =
    let pol = function
      | Pos -> "jump"
      | Neg -> "call"
      | Unknown -> "ambiguous"
    in
    pp_sexp (pol p) [string_of_value v; string_of_stack s]

  module V = struct
    type t = value
    let var x = Var(x)
    let bind x t c = Bind (x,t,c)
    let str_bind x t c = Force (x,t,c)
    let box k a t c = Box (k,a,t,c)
    let cons c = Cons c
    let case l = Destr l
  end

  module S = struct
    type t = stack
    let var x = CoVar (x)
    let bind x t c = CoBind (x,t,c)
    let str_bind x t c = CoForce (x,t,c)
    let box k s = CoBox (k, s)
    let destr c = CoDestr c
    let case l = CoCons l
  end

  type t = command
  let (|+|) (t : V.t) (s : S.t) = Command (Pos,t,s)
  let (|~|) (v : V.t) (e : S.t) = Command (Neg,v,e)
  let (|?|) (t : V.t) (e : S.t) = Command (Unknown,t,e)

  let (|=>) a b = (a,b) (*  Syntactic suger to allow for `pattern |=> command` in (co)case  *)

end
