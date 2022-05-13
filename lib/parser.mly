%{
    open Calculi.PreLAMECalc
%}

%token COLUMN BANG QUESTION PLUS TILDE EQUAL SEMICOL STAR MINUS
%token LPAREN RPAREN LCURLY RCURLY
%token JUMP ENTER FORCE LET MATCH
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM
%token TYPNEUT TYPPOS TYPMIN
%token DECL TYPE DATA CODATA TERM ENV CMD
%token <string> VAR
%token END

%start <program> prog
%%

prog:
  | prog = separated_list(SEMICOL, prog_item) END {prog}

prog_item:
  | DECL TYPE name = tvar COLUMN sort = sort
    {Type_declaration {name; sort}}

  | TYPE LCURLY name = tvar args = list(typ_arg) RCURLY COLUMN sort = sort EQUAL content = typ
    {Type_definition {name;args;sort;content}}

  | TYPE name = tvar COLUMN sort = sort EQUAL content = typ
    {Type_definition {name;args=[];sort;content}}

typ_arg:
  | LCURLY var = tvar sort = sort RCURLY { (var,sort) }

tvar:
  | name = VAR {TyVar.of_string name}

tconsvar:
  | COLUMN name = VAR {TyVar.of_string name}

consvar:
  | COLUMN name = VAR {ConsVar.of_string name}

boxkind:
  | LINEAR {linear}
  | AFFINE {affine}
  | EXP {exp}

typ:
  | typ = typ_neut {typ}
  | typ = typ_pos {pos typ}
  | typ = typ_neg {neg typ}

typ_neut:
  | var = tvar { tvar var }

typ_pos:
  | PLUS var = tvar { posvar var }
  | PLUS LCURLY BOX kind = boxkind content = typ RCURLY {boxed kind content}
  | PLUS LCURLY cons = typ_pos_cons RCURLY {data cons}

typ_pos_cons:
  | cons = tconsvar args = list(typ) { postype cons args }
  | UNIT { unit_t }
  | ZERO { zero }
  | PROD a = typ b = typ {prod a b}
  | SUM a = typ b = typ {sum a b}

typ_neg:
  | MINUS var = tvar { negvar var }
  | MINUS LCURLY cons = typ_neg_cons RCURLY {codata cons}

typ_neg_cons:
  | cons = tconsvar args = list(typ) { negtype cons args }
  | TOP { top }
  | BOTTOM { bottom }
  | CHOICE a = typ b = typ {choice a b}
  | FUN a = typ b = typ {func a b}

sort:
  | TYPNEUT {Types.NeuType}
  | TYPPOS {Types.PosType}
  | TYPMIN {Types.NegType}
