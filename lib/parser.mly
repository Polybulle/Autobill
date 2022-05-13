%{
    open Calculi.PreLAMECalc
%}

%token COLUMN BANG QUESTION PLUS TILDE EQUAL SEMICOL STAR MINUS
%token LPAREN RPAREN LCURLY RCURLY LBRACKET RBRACKET
%token JUMP ENTER FORCE LET MATCH
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM
%token DECL TYPE DATA CODATA TERM ENV CMD OF BAR
%token <string> VAR
%token END

%start <program> prog
%%

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



typ_arg:
  | PLUS var = tvar { (var, Types.PosType)}
  | MINUS var = tvar { (var, Types.NegType)}
  | TILDE var = tvar { (var, Types.NeuType)}
  | LCURLY var = tvar  sort = sort RCURLY { (var,sort) }

tvar:
  | name = VAR {TyVar.of_string name}

tconsvar:
  | name = VAR {TyVar.of_string name}

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

data_cons_def:
  | cons = consvar OF args = separated_nonempty_list(STAR, typ) {poscons cons args}
  | cons = consvar {poscons cons []}
