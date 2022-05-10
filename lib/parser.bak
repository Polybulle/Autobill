%{
    open Cst
%}

%token <string> VAR
%token COLUMN BANG QUESTION PLUS TILDE
%token <string> KEYWORD
%token LPAREN RPAREN LCURLY RCURLY
%token END

%start <Cst.expr> prog
%%

prog:
  | e = expr END {e}

expr:
  |        v = VAR {Var v}
  | BANG   a = VAR {CoVar a}
  | COLUMN c = VAR {Cons c}
  | t = typ_annot {Type t}
  | LPAREN k = KEYWORD es = list(expr) RPAREN {Parens ((Keyword k)::es)}

some_tvar:
  | QUESTION v = VAR {Tvar v}
  | PLUS v = VAR {PosVar v}
  | TILDE v = VAR {NegVar v}

typ_annot:
  | LCURLY v = some_tvar RCURLY {v}
  | t = typ_expr {t}

typ_expr:
  | LCURLY t = some_tvar ts = nonempty_list(typ) RCURLY {Curlies (t :: ts)}

typ:
  | v = some_tvar {v}
  | t = typ_expr {t}
