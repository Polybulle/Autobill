%{
    open Calculi.PreLAMECalc
%}

%token COLUMN BANG QUESTION PLUS TILDE EQUAL SEMICOL STAR
%token LPAREN RPAREN LCURLY RCURLY
%token JUMP ENTER FORCE LET MATCH
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE
%token TYPNEUT TYPPOS TYPMIN
%token DECL TYPE DATA CODATA TERM ENV CMD
%token <string> VAR
%token END

%start <program> prog
%%

prog:
  | prog = separated_list(SEMICOL, prog_item) END {prog}

prog_item:
  | DECL TYPE name = tvar COLUMN sort = sort {Type_declaration {name; sort}}

tvar:
  | name = VAR {TyVar.of_string name}

sort:
  | TYPNEUT {Types.NeuType}
  | TYPPOS {Types.PosType}
  | TYPMIN {Types.NegType}
