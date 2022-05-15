%{
    open Calculi.PreLAMECalc
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token COLUMN BANG QUESTION PLUS TILDE EQUAL SEMICOL STAR MINUS
%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token JUMP ENTER FORCE LET MATCH
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM
%token DECL TYPE DATA CODATA TERM ENV CMD OF BAR CONT
%token <string> VAR
%token END

%start <program> prog
%%

(* Généralités  *)

sort_typ_pos:
  | LBRACKET PLUS RBRACKET {}

sort_typ_neg:
  | LBRACKET MINUS RBRACKET {}

sort_typ_neut:
  | LBRACKET TILDE RBRACKET {}

sort:
  | sort_typ_neut {Types.NeuType}
  | sort_typ_pos {Types.PosType}
  | sort_typ_neg {Types.NegType}

boxkind:
  | LINEAR {linear}
  | AFFINE {affine}
  | EXP {exp}

tvar:
  | name = VAR {TyVar.of_string name}

tconsvar:
  | name = VAR {TyVar.of_string name}

consvar:
  | COLUMN name = VAR {ConsVar.of_string name}

var:
  | name = VAR {Var.of_string name}

covar:
  | BANG name = VAR {CoVar.of_string name}

(* Méta-langage *)

prog:
  | END {[]}
  | prog = prog_rev END {prog}

prog_rev:
  | first = prog_item SEMICOL rest = prog_rev {(first :: rest)}
  | last = prog_item {[last]}
  | last = prog_item SEMICOL {[last]}

prog_item:
  | DECL TYPE name = tvar COLUMN sort = sort
    {Type_declaration {name; sort}}

  | TYPE LCURLY PLUS name = tvar args = list(typ_arg) RCURLY EQUAL content = typ
    {Type_definition {name;args;sort=Types.PosType;content}}

  | TYPE LCURLY MINUS name = tvar args = list(typ_arg) RCURLY EQUAL content = typ
    {Type_definition {name;args;sort=Types.NegType;content}}

  | TYPE LCURLY TILDE name = tvar args = list(typ_arg) RCURLY EQUAL content = typ
    {Type_definition {name;args;sort=Types.NeuType;content}}

  | TYPE LCURLY name = tvar args = list(typ_arg) RCURLY COLUMN sort = sort EQUAL content = typ
    {Type_definition {name;args;sort=sort;content}}

  | TYPE name = tvar COLUMN sort = sort EQUAL content = typ
    {Type_definition {name;args=[];sort;content}}

  | DATA LCURLY PLUS name = tvar args = list(typ_arg) RCURLY EQUAL
    BAR content = separated_nonempty_list(BAR, data_cons_def)
    { Data_definition{name; args; content} }

  | DATA PLUS name = tvar EQUAL
    BAR content = separated_nonempty_list(BAR, data_cons_def)
    { Data_definition{name; args = []; content} }

  | CODATA LCURLY MINUS name = tvar args = list(typ_arg) RCURLY EQUAL
    BAR content = separated_nonempty_list(BAR, codata_cons_def)
    { Codata_definition{name; args; content} }

  | CODATA MINUS name = tvar EQUAL
    BAR content = separated_nonempty_list(BAR, codata_cons_def)
    { Codata_definition{name; args = []; content} }

  | CMD name = var EQUAL content = cmd
    { Cmd_definition{name;content} }

  | TERM name = var typ = typ_annot EQUAL content = value { Term_definition {name;typ;content} }

  | ENV name = covar typ = typ_annot EQUAL content = stack { Env_definition {name;typ;content} }


data_cons_def:
  | cons = consvar OF args = separated_nonempty_list(STAR, typ)
    {poscons cons args}
  | cons = consvar {poscons cons []}

codata_cons_def:
  | cons = consvar CONT cont = typ {negcons cons [] cont}
  | cons = consvar OF args = separated_nonempty_list(STAR, typ) CONT cont = typ
    {negcons cons args cont}


(* Types *)

typ_annot:
  | typ = typ {typ}
  | {omitted}

typ_arg:
  | PLUS var = tvar { (var, Types.PosType)}
  | MINUS var = tvar { (var, Types.NegType)}
  | TILDE var = tvar { (var, Types.NeuType)}
  | var = tvar sort = sort { (var,sort) }

typ:
  | typ = typ_neut {typ}
  | typ = typ_pos {pos typ}
  | typ = typ_neg {neg typ}

typ_neut:
  | TILDE var = tvar { tvar var }

typ_pos:
  | PLUS var = tvar { posvar var }
  | LCURLY PLUS BOX kind = boxkind content = typ RCURLY {boxed kind content}
  | LCURLY PLUS cons = typ_pos_cons RCURLY {data cons}
  | PLUS UNIT { data unit_t }
  | PLUS ZERO { data zero }

typ_pos_cons:
  | cons = tconsvar args = list(typ) { postype cons args }
  | PROD a = typ b = typ {prod a b}
  | SUM a = typ b = typ {sum a b}

typ_neg:
  | MINUS var = tvar { negvar var }
  | LCURLY MINUS cons = typ_neg_cons RCURLY {codata cons}
  | MINUS TOP { codata top }
  | MINUS BOTTOM { codata bottom }

typ_neg_cons:
  | cons = tconsvar args = list(typ) { negtype cons args }
  | CHOICE a = typ b = typ {choice a b}
  | FUN a = typ b = typ {func a b}

(* Terms *)

cmd:
  | LPAREN JUMP t = value s = stack RPAREN { t |+| s }
  | LPAREN ENTER t = value s = stack RPAREN { t |-| s }

stack:
  | a = covar {S.var a}
  | LPAREN UNBOX k = boxkind s = stack RPAREN {S.box k s}
  | LPAREN LET x = var t = typ cmd = cmd RPAREN {S.bind x t cmd}
  | LPAREN FORCE x = var t = typ cmd = cmd RPAREN {S.str_bind x t cmd}
  | LPAREN s = stack_cons RPAREN {S.destr s}
  | LPAREN MATCH patts = list(value_case) RPAREN {S.case patts}

stack_cons:
  | COLUMN CALL s = stack v = value {call v s}
  | COLUMN YES s = stack {yes s}
  | COLUMN NO s = stack {no s}
  | cons = consvar cont = stack args = list(value) {negcons cons args cont}

covar_cons:
  | COLUMN CALL s = covar v = var {call (v,omitted) (s,omitted)}
  | COLUMN YES s = covar {yes (s, omitted)}
  | COLUMN NO s = covar {no (s, omitted)}
  | cons = consvar cont = covar args = list(var)
    {negcons cons (List.map (fun x -> (x,omitted)) args) (cont, omitted)}

stack_case:
  | LPAREN cons = covar_cons RPAREN cmd = cmd { (cons, cmd) }

value:
  | v = var {V.var v}
  | LPAREN BOX k = boxkind a = covar t = typ c = cmd RPAREN {V.box k a t c}
  | LPAREN LET x = covar t = typ cmd = cmd RPAREN {V.bind x t cmd}
  | LPAREN FORCE x = covar t = typ cmd = cmd RPAREN {V.str_bind x t cmd}
  | v = value_cons {V.cons v}
  | LPAREN MATCH patts = list(stack_case) RPAREN {V.case patts}

value_cons:
  | COLUMN UNIT {unit}
  | cons = consvar {poscons cons []}
  | LPAREN cons = long_value_cons RPAREN {cons}

long_value_cons:
  | COLUMN UNIT {unit}
  | COLUMN PAIR a = value b = value {pair a b}
  | COLUMN LEFT a = value {fst a}
  | COLUMN RIGHT b = value {snd b}
  | cons = consvar args = list(value) {poscons cons args}

var_cons:
  | COLUMN UNIT {unit}
  | COLUMN PAIR a = var b = var {pair (a, omitted) (b,omitted)}
  | COLUMN LEFT a = var {fst (a, omitted)}
  | COLUMN RIGHT b = var {snd (b, omitted)}
  | cons = consvar args = list(var) {poscons cons (List.map (fun x -> (x,omitted)) args)}

value_case:
  | LPAREN cons = var_cons RPAREN cmd = cmd { (cons, cmd) }
