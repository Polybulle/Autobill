%{
    open Lcbpv
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token PLUS EQUAL MINUS ARROW COMMA COLUMN BAR DOT SEMICOL STAR SLASH PERCENT BANG LANGLE AND OR
%token LPAREN RPAREN LCURLY RCURLY
%token UUNIT ZZERO TTOP BBOTTOM TTUPLE SSUM FFUN CCHOICE TTHUNK CCLOSURE EEXP
%token UNIT THUNK CLOSURE TUPLE EXP TRUE FALSE INJ PROJ CALL
%token GET END ABSURD MATCH RETURN LET REC IS OPEN FORCE WITH IF THEN ELSE TYPE DECL DATA COMPUT
%token <string> VAR
%token <string> TCONS
%token <int> NUM
%token EOF

%start <program> prog
%%

(* Généralités  *)

pol:
  | PLUS {Pos}
  | MINUS {Neg}

sort:
  | pol = pol {pol}

(* Binders *)

typ_annot:
  | COLUMN typ = typ {Some typ}
  | {None}

pol_annot:
  | pol = pol {Some pol}
  | {None}

paren_typed_var:
  | LPAREN var = VAR typ = typ_annot RPAREN { (var, typ) }

typed_var:
  | v = VAR typ = typ_annot { (v, typ) }
  | bind = paren_typed_var {bind}

sorted_tyvar:
  | v = VAR  {(v , None)}
  | v = VAR COLUMN sort = sort {(v , Some sort)}
  | LPAREN v = VAR sort = sort RPAREN {(v , Some sort)}

paren_sorted_tyvar_def:
  | LPAREN v = VAR COLUMN sort = sort RPAREN {(v , sort)}

sorted_tyvar_def:
  | v = VAR COLUMN sort = sort {(v , sort)}
  | a = paren_sorted_tyvar_def {a}


(* Types *)

delim_typ:
  | v = VAR {Typ_Var v}
  | c = tcons {Typ_Cons (c, [])}
  | LPAREN t = typ RPAREN {t}

typ:
  | t = delim_typ {t}
  | FFUN LPAREN args = separated_list(COMMA,typ) RPAREN ARROW ret = typ
    {Typ_Cons (Typ_Fun, ret :: args)}
  | c = tcons args = nonempty_list(delim_typ)
    {Typ_Cons (c, args)}

tcons:
  | UUNIT {Typ_Unit}
  | ZZERO {Typ_Zero}
  | TTOP {Typ_Top}
  | BBOTTOM {Typ_Bottom}
  | TTHUNK {Typ_Thunk}
  | CCLOSURE {Typ_Closure Lin}
  | EEXP {Typ_Closure Exp}
  | TTUPLE {Typ_Tuple}
  | SSUM {Typ_Sum}
  | CCHOICE {Typ_LazyPair}
  | var = TCONS {Typ_Named var}

(* Constructors and methods *)

bracket_tupple:
  | LCURLY a = NUM COMMA b = NUM RCURLY { (a,b) }

cons:
  | name = VAR {Cons_Named name}
  | UNIT {Unit}
  | TRUE {True}
  | FALSE {False}
  | n = NUM {Int_Litt n}
  | TUPLE {Tuple}
  | INJ n = bracket_tupple {Inj (fst n, snd n)}

methodd:
  | name = VAR {Method_Named name}
  | CALL {Call}
  | PROJ n = bracket_tupple {Proj (fst n, snd n)}

bin_op:
  | PLUS {Add}
  | MINUS {Subs}
  | STAR {Mult}
  | SLASH {Div}
  | PERCENT {Mod}
  | AND {And}
  | OR {Or}
  | LANGLE EQUAL {Int_Leq}
  | EQUAL EQUAL {Int_Eq}
  | LANGLE {Int_Lt}

mon_op:
  | BANG {Not}
  | MINUS {Opp}

(* Expressions *)

expr:
  | e = delim_expr {e}
  | GET ms = separated_list(BAR, get_patt) END {Expr_Get ms}
  | MATCH v = expr WITH ps = separated_list(COMMA,match_patt) END {Expr_Match (v,ps)}
  | a = delim_expr op = bin_op b = delim_expr {Expr_Bin_Prim (op, a, b)}
  | op = mon_op a = delim_expr {Expr_Mon_Prim (op, a)}
  | IF b = expr THEN x = expr ELSE y = expr {Expr_If (b, x, y)}
  | REC x = VAR IS e = expr {Expr_Rec (x, e)}

delim_expr:
  | LPAREN e = expr RPAREN {e}
  | LCURLY b = block RCURLY {Expr_Block b}
  | name = VAR {Expr_Var name}
  | n = NUM {Expr_Int n}
  | c = cons LPAREN args = separated_list(COMMA, expr) RPAREN {Expr_Constructor (c, args)}
  | THUNK LPAREN v = expr RPAREN {Expr_Thunk v}
  | CLOSURE LPAREN v = expr RPAREN {Expr_Closure (Lin, v)}
  | EXP LPAREN v = expr RPAREN {Expr_Closure (Exp, v)}
  | ABSURD LCURLY p = pol RCURLY LPAREN e = expr RPAREN {Expr_Absurd (p,e)}
  | v = delim_expr DOT m = methodd LPAREN args = separated_list(COMMA,expr) RPAREN
  {Expr_Method (v, m, args)}

get_patt:
  | m = methodd LPAREN args = separated_list(COMMA, VAR) RPAREN ARROW e = expr
  { GetPat (m, args, e) }

match_patt:
  | c = cons LPAREN args = separated_list(COMMA, VAR) RPAREN ARROW e = expr
  { MatchPat (c, args, e) }

(* Block *)

block:
  | i = list(instr) RETURN e = expr {Blk (i,e)}

instr:
  | i = instr_inner SEMICOL {i}

instr_inner:
  | LET x = VAR EQUAL e = expr {Ins_Let (x,e)}
  | FORCE x = VAR EQUAL e = expr {Ins_Force (x,e)}
  | OPEN x = VAR EQUAL e = expr {Ins_Open (x,e)}

(* Définitions et déclarations *)

typ_args:
  | LPAREN args = separated_list(COMMA, sorted_tyvar_def) RPAREN {args}
  | {[]}

cons_method_args:
  | l = separated_list(COMMA, typ) {l}

cons_def:
  | BAR cons = VAR LPAREN args = cons_method_args RPAREN {(cons, args)}

method_def:
  | BAR me = VAR LPAREN args = cons_method_args RPAREN ARROW t = typ
    {(me, args, t)}

prog_item:
  | DECL TYPE k = VAR COLUMN s = sort
    {Typ_Decl (k, [], s)}
  | DECL TYPE k = VAR COLUMN LPAREN args = separated_list(COMMA, sort) RPAREN ARROW s = sort
    {Typ_Decl (k, args, s)}
  | TYPE k = VAR args = typ_args COLUMN so = sort EQUAL t = typ
    {Typ_Def (k, args, Def_Synonym (t, so))}
  | DATA k = VAR args = typ_args EQUAL conses = list(cons_def) END
    {Typ_Def (k, args, Def_Datatype (conses))}
  | COMPUT k = VAR args = typ_args EQUAL meths = list(method_def) END
    {Typ_Def (k, args, Def_Computation(meths))}
  | b = block {Do b}

prog:
  | prog = list(prog_item) EOF {Prog prog}
