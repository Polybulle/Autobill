%{
    open Cst
%}

%token <string> VAR
%token <string> CONS
%token <string> COVAR
%token <string> TYVAR
%token <string> KEYWORD
%token LPAREN RPAREN LCURLY RCURLY
%token END

%start <Cst.expr> prog
%%

prog:
  | e = expr END {e}

expr:
  | v = VAR {Var v}
  | a = COVAR {CoVar a}
  | c = CONS {Cons c}
  | t = typ_annot {Type t}
  | LPAREN k = KEYWORD es = list(expr) RPAREN {Parens ((Keyword k)::es)}

typ_annot:
  | LCURLY v = TYVAR RCURLY {Tvar v}
  | t = typ_expr {t}

typ_expr:
  | LCURLY t = TYVAR ts = nonempty_list(typ) RCURLY {Curlies (Tvar t :: ts)}

typ:
  | v = TYVAR {Tvar v}
  | t = typ_expr {t}
