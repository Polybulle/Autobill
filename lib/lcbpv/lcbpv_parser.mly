%{
    open Lcbpv
    open Misc
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end

    let locced(start_pos, end_pos) x = (x, {start_pos; end_pos; is_dummy = false})
    let loc (start_pos, end_pos) = {start_pos; end_pos; is_dummy = false}

%}

%token PLUS EQUAL MINUS ARROW COMMA COLUMN BAR DOT SEMICOL STAR SLASH PERCENT BANG LANGLE AND OR
%token LPAREN RPAREN LCURLY RCURLY
%token UUNIT ZZERO TTOP BBOTTOM TTUPLE SSUM FFUN CCHOICE TTHUNK CCLOSURE EEXP AAFF
%token UNIT THUNK CLOSURE TUPLE EXP AFF TRUE FALSE INJ PROJ CALL PACK SPEC
%token GET END MATCH RETURN LET REC IS OPEN FORCE IF THEN ELSE COPY AS TRACE QUOTE
%token TYPE DECL DATA COMPUT SORT REL WHERE WITH
%token <string> VAR
%token <string> TCONS
%token <int> NUM
%token EOF

%start <program> prog
%%

(* Généralités  *)


sort:
  | PLUS {Pos}
  | MINUS {Neg}
  | so = VAR {Index so}
  | LPAREN s = sort ARROW t = sort RPAREN {Arrow ([s], t)}

eqn:
  | a = delim_typ EQUAL b = delim_typ {Eq (a,b)}
  | rel = VAR LPAREN args = separated_list(COMMA,typ) RPAREN {Rel (rel, args)}

eqns:
  | eqns = separated_nonempty_list(COMMA,eqn) {eqns}
  | {[]}

(* Binders *)

(* typ_annot:
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
  | LPAREN v = VAR sort = sort RPAREN {(v , Some sort)} *)

sorted_tyvar_def:
  | v = TCONS COLUMN sort = sort {(v , sort)}


(* Types *)

delim_typ:
  | t = pre_delim_typ {locced $sloc t}

pre_delim_typ:
  | UUNIT {Typ_Unit}
  | ZZERO {Typ_Zero}
  | TTOP {Typ_Top}
  | BBOTTOM {Typ_Bottom}
  | TTHUNK {Typ_Thunk}
  | CCLOSURE {Typ_Closure Lin}
  | EEXP {Typ_Closure Exp}
  | AAFF {Typ_Closure Aff}
  | TTUPLE {Typ_Tuple}
  | SSUM {Typ_Sum}
  | CCHOICE {Typ_LazyPair}
  | v = TCONS {Typ_Var v}
  | LPAREN t = pre_typ RPAREN {t}
  | c = delim_typ LPAREN args = separated_nonempty_list(COMMA, typ) RPAREN
    {Typ_App (c, args)}

typ:
 | t = pre_typ {locced $sloc t}

pre_typ:
  | t = pre_delim_typ {t}
  | FFUN LPAREN args = separated_list(COMMA,typ) RPAREN ARROW ret = typ
    {Typ_App ((locced $loc($1) Typ_Fun), ret :: args)}

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
  | name = VAR {locced $sloc (Method_Named name)}
  | CALL {locced $sloc Call}
  | PROJ n = bracket_tupple {locced $sloc (Proj (fst n, snd n))}

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
  | e = pre_expr {locced $sloc e}

pre_expr:
  | e = pre_delim_expr {e}
  | GET BAR? ms = separated_list(BAR, get_patt) END {Expr_Get ms}
  | MATCH v = expr WITH BAR? ps = separated_list(BAR,match_patt) END {Expr_Match (v,ps)}
  | a = delim_expr op = bin_op b = delim_expr {Expr_Bin_Prim (op, a, b)}
  | op = mon_op a = delim_expr {Expr_Mon_Prim (op, a)}
  | IF b = expr THEN x = expr ELSE y = expr {Expr_If (b, x, y)}
  | REC x = VAR IS e = expr {Expr_Rec (locced $loc(x) x, e)}

delim_expr:
  | e = pre_delim_expr {locced $sloc e}

pre_delim_expr:
  | LPAREN e = pre_expr RPAREN {e}
  | LPAREN RPAREN {Expr_Constructor (Unit, [])}
  | LPAREN e = expr COMMA es = separated_list(COMMA, expr) RPAREN {Expr_Constructor (Tuple, e::es)}
  | LCURLY b = block RCURLY {Expr_Block b}
  | name = VAR {Expr_Var (name, loc $sloc)}
  | n = NUM {Expr_Int n}
  | c = cons LPAREN args = separated_list(COMMA, expr) RPAREN {Expr_Constructor (c, args)}
  | THUNK LPAREN v = expr RPAREN {Expr_Thunk v}
  | CLOSURE LPAREN v = expr RPAREN {Expr_Closure (Lin, v)}
  | AFF LPAREN v = expr RPAREN {Expr_Closure (Aff, v)}
  | EXP LPAREN v = expr RPAREN {Expr_Closure (Exp, v)}
  | v = delim_expr DOT m = methodd LPAREN args = separated_list(COMMA,expr) RPAREN
  {Expr_Method (v, m, args)}

arg_patt:
  | v = VAR {locced $sloc v}

get_patt:
  | m = methodd LPAREN args = separated_list(COMMA, arg_patt) RPAREN ARROW e = expr
  { GetPatTag (m, args, e, loc $sloc) }

match_patt:
  | c = cons LPAREN args = separated_list(COMMA, arg_patt) RPAREN ARROW e = expr
  { MatchPatTag (c, args, e, loc $sloc) }
  | x = arg_patt ARROW e = expr {MatchPatVar (x, e, loc $sloc)}

(* Block *)

block:
  | i = list(instr) RETURN e = expr {Blk (i,e, loc $sloc)}

instr:
  | i = instr_inner SEMICOL {i, loc $sloc}

struct_var:
  | v = VAR { locced $loc(v) v }

instr_inner:
  | LET x = VAR EQUAL e = expr {Ins_Let (locced $loc(x) x,e)}
  | FORCE  x = VAR EQUAL e = expr {Ins_Force (locced $loc(x) x,e)}
  | OPEN q = qual x = VAR EQUAL e = expr {Ins_Open (locced $loc(x) x,q,e)}
  | PACK x = VAR EQUAL e = expr {Ins_Pack (locced $loc(x) x,e)}
  | SPEC x = VAR EQUAL e = expr {Ins_Spec (locced $loc(x) x,e)}
  | TRACE QUOTE comment = VAR? QUOTE dump = expr?  {Ins_Trace (comment, dump)}
  | COPY e = expr AS LPAREN vs = separated_list(COMMA, struct_var) RPAREN
    {Ins_Struct (e,vs)}

qual:
  | CLOSURE {Lin}
  | AFF {Aff}
  | EXP {Exp}

(* Définitions et déclarations *)

typ_args:
  | LPAREN args = separated_list(COMMA, sorted_tyvar_def) RPAREN {args}
  | {[]}

cons_method_args:
  | l = separated_list(COMMA, typ) {l}

(* TODO  parametres dans le parser lcbpv + tests *)

with_params:
  | WITH param = typ_args {param}
  | {[]}

where_eqns:
  | WHERE eqns = eqns {eqns}
  | {[]}

cons_def:
  | cons = VAR LPAREN args = cons_method_args RPAREN
    param = with_params
    eqns = where_eqns
  {Constructor_Def {
         name = cons;
         arguments = args;
         parameters = param;
         equations = eqns
    }}

method_def:
  | me = VAR LPAREN args = cons_method_args RPAREN ARROW t = typ
    param = with_params
    eqns = where_eqns
    {Destructor_Def {
         name = me;
         arguments = args;
         returns = t;
         parameters = param;
         equations = eqns
    }}

prog_item:
  | DECL SORT s = VAR
    {Sort_Decl (s, loc $sloc)}
  | DECL REL r = VAR COLUMN LPAREN args = separated_list(COMMA, sort) RPAREN
    {Rel_Decl (r, args, loc $sloc)}
  | DECL TYPE k = TCONS COLUMN s = sort
    {Typ_Decl ((k, loc $loc(k)), [], s, loc $sloc)}
  | DECL TYPE k = TCONS COLUMN LPAREN args = separated_list(COMMA, sort) RPAREN ARROW s = sort
    {Typ_Decl ((k, loc $loc(k)), args, s, loc $sloc)}
  | TYPE k = TCONS args = typ_args COLUMN so = sort EQUAL t = typ
    {Typ_Def (k, args, Def_Synonym (t, so), loc $sloc)}
  | DATA k = TCONS args = typ_args EQUAL BAR? conses = separated_list(BAR,cons_def)
    {Typ_Def (k, args, Def_Datatype (conses), loc $sloc)}
  | COMPUT k = TCONS args = typ_args EQUAL BAR? meths = separated_list(BAR,method_def)
    {Typ_Def (k, args, Def_Computation(meths), loc $sloc)}

prog_item_bis:
  | i = prog_item SEMICOL {i}

prog:
  | prog = list(prog_item_bis) b = block EOF {Prog (prog @ [Do b])}
