type box_kind = BoxKind of string
type cons = Cons of string
type type_cst =
    TCons of cons * type_cst list
  | TBox of box_kind * type_cst
  | TVar of string
  | TPos of type_cst
  | TNeg of type_cst
  | TOmited
type term_cst =
    Var of string
  | Bind of string * type_cst * cmd_cst
  | Force of string * type_cst * cmd_cst
  | Box of box_kind * string * type_cst * cmd_cst
  | Cons of cons * term_cst list
  | Match of (patt_cst * cmd_cst) list
and env_cst =
    CoVar of string
  | CoBind of string * type_cst * cmd_cst
  | CoForce of string * type_cst * cmd_cst
  | UnBox of box_kind * env_cst
  | CoCons of cons * env_cst * type_cst * (term_cst * type_cst) list
  | CoMatch of (copatt_cst * cmd_cst) list
and cmd_cst =
    Jump of string * type_cst * term_cst * env_cst
  | Call of string * type_cst * term_cst * env_cst
and patt_cst = cons * (string * type_cst) list
and copatt_cst = cons * string * type_cst * (string * type_cst) list
