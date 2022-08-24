%{
    open Constructors
    open Types
    open Cst
    open Util
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token COLUMN PLUS EQUAL MINUS DOT ARROW COMMA SLASH META
%token LPAREN RPAREN
%token STEP INTO WITH BIND BINDCC MATCH CASE RET END THIS IN DO FIX
%token GOT_TOP GOT_ZERO
%token BOX UNBOX LINEAR AFFINE EXP
%token TUPPLE INJ CALL PROJ LEFT RIGHT YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM SHIFT
%token DECL TYPE DATA CODATA TERM ENV CMD
%token <string> VAR
%token <int> NUM
%token EOF

%start <program> prog
%%

(* Généralités  *)

pol:
  | PLUS {positive}
  | MINUS {negative}

pol_annot:
  | pol = pol {Some pol}
  | META {None}
  | {None}

sort:
  | pol = pol {sort_base pol}

sort_annot:
  | COLUMN so = sort {so}

boxkind:
  | LINEAR {linear}
  | AFFINE {affine}
  | EXP {exp}

tvar:
  | name = VAR META? {name}

tconsvar:
  | name = VAR META? {name}

consvar:
  | COLUMN name = VAR META? {name}

destrvar:
  | name = VAR META? {name}

var:
  | name = VAR META? {name}

typ_annot:
  | COLUMN typ = typ {Some typ}
  | COLUMN META {None}
  | {None}

paren_typed_var:
  | v = var { (v, None) }
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_var:
  | var = var typ = typ_annot { (var, typ) }

typ_arg:
  | LPAREN v = tvar sort = sort_annot RPAREN {(v , sort)}


(* Types *)

typ:
  | c = one_word_typ_cons
    {cons ~loc:(position $symbolstartpos $endpos) c}
  | var = tvar
    {tvar ~loc:(position $symbolstartpos $endpos) var}
  | LPAREN kind = boxkind content = typ RPAREN
    {boxed ~loc:(position $symbolstartpos $endpos) kind content}
  | LPAREN c = typ_cons RPAREN
    {cons ~loc:(position $symbolstartpos $endpos) c}
  | PLUS typ = typ {pos typ}
  | MINUS typ = typ {neg typ}

one_word_typ_cons:
  | UNIT {unit_t}
  | ZERO {zero}
  | TOP {top}
  | BOTTOM {bottom}

typ_cons:
  | PROD a = nonempty_list(typ) {Prod a}
  | SUM a = nonempty_list(typ) {Sum a}
  | CHOICE a = nonempty_list(typ) {Choice a}
  | FUN a = nonempty_list(typ) ARROW b = typ {Fun(a, b)}
  | SHIFT PLUS a = typ {shift_pos_t a}
  | SHIFT MINUS a = typ {shift_neg_t a}
  | c = tconsvar args = list(typ) {typecons c args}

(* Terms *)

cmd:
  | STEP pol = pol_annot valu = value typ = typ_annot INTO stk = stack END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | STEP pol = pol_annot INTO stk = stack typ = typ_annot WITH valu = value END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | valu = value DOT stk = stk_trail
    {cmd ~loc:(position $symbolstartpos $endpos) None valu stk}

  | TERM pol = pol_annot x = var annot = typ_annot EQUAL v = value IN c = cmd
    {cmd_let_val ~loc:(position $symbolstartpos $endpos) ?pol:pol x annot v c }
  | ENV pol = pol_annot stk = stack annot = typ_annot IN c = cmd
    {cmd_let_env ~loc:(position $symbolstartpos $endpos) ?pol:pol annot stk c }
  | MATCH pol = pol_annot cons = cons EQUAL valu = value IN c = cmd
    {cmd_match_val ~loc:(position $symbolstartpos $endpos) ?pol:pol cons valu c }
  | MATCH pol = pol_annot ENV THIS DOT destr = destr IN c = cmd
    {cmd_match_env ~loc:(position $symbolstartpos $endpos) ?pol:pol destr c}

cont_annot:
  | LPAREN RET LPAREN RPAREN typ = typ_annot RPAREN {typ}
  | {None}

value:
  | v = var
    {V.var ~loc:(position $symbolstartpos $endpos) v}
  | GOT_TOP
    {V.cotop ~loc:(position $symbolstartpos $endpos) ()}
  | c = value_cons
    {V.cons ~loc:(position $symbolstartpos $endpos) c}
  | BOX LPAREN kind = boxkind RPAREN typ = cont_annot ARROW cmd = cmd
    {V.box ~loc:(position $symbolstartpos $endpos) kind typ cmd}
  | BINDCC pol = pol_annot typ = cont_annot ARROW cmd = cmd
    {V.bindcc ~loc:(position $symbolstartpos $endpos) ?pol:pol typ cmd}
  | MATCH THIS DOT FIX LPAREN self = var self_typ = typ_annot RPAREN DOT RET LPAREN RPAREN typ_annot ARROW cmd = cmd
    {Fix {self; self_typ; cmd; loc = (position $symbolstartpos $endpos)}}
  | MATCH patt = copatt
    {V.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH CASE patts = separated_list(CASE,copatt) END
    {V.case ~loc:(position $symbolstartpos $endpos) patts}

  | FUN x = paren_typed_var ARROW v = value
    {let (arg, typ) = x in V.macro_fun ~loc:(position $symbolstartpos $endpos) arg typ v}
  | BOX LPAREN kind = boxkind RPAREN v = value
    {V.macro_box ~loc:(position $symbolstartpos $endpos) kind v}


copatt:
  | THIS DOT destr = destr ARROW cmd = cmd { (destr, cmd) }

destr:
  | cons = destrvar LPAREN args = separated_list(COMMA, typed_var) RPAREN
    DOT RET LPAREN RPAREN typ = typ_annot
    { negcons cons args typ }
  | CALL LPAREN vars = separated_list(COMMA, typed_var) RPAREN DOT RET LPAREN RPAREN typ = typ_annot {Call (vars, typ)}
  | YES  LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {yes typ}
  | NO   LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {no typ}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT RET LPAREN RPAREN typ = typ_annot {Proj (i,n,typ)}
  | SHIFT MINUS LPAREN RPAREN            DOT RET LPAREN RPAREN typ = typ_annot {shift_neg typ}

value_cons:
  | UNIT LPAREN RPAREN { unit }
  | TUPPLE LPAREN xs = separated_list(COMMA, value) RPAREN {Tupple xs}
  | LEFT LPAREN a = value RPAREN {left a}
  | RIGHT LPAREN b = value RPAREN {right b}
  | INJ LPAREN i = NUM SLASH n = NUM COMMA a = value RPAREN {Inj (i,n,a)}
  | SHIFT PLUS LPAREN a = value RPAREN {shift_pos a}
  | cons = consvar LPAREN args = separated_list(COMMA,value) RPAREN {poscons cons args}

stack:
  | THIS DOT stk = stk_trail {stk}

stk_trail:
  | GOT_ZERO LPAREN RPAREN
    {S.cozero ~loc:(position $symbolstartpos $endpos) ()}
  | RET LPAREN RPAREN
    {S.ret ~loc:(position $symbolstartpos $endpos) ()}
  | CALL LPAREN xs = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Call (xs, stk))}
  | YES LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (yes stk)}
  | NO LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (no stk)}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Proj (i,n, stk))}
  | SHIFT MINUS LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (shift_neg stk)}
  | cons = destrvar LPAREN args = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (negcons cons args stk)}
  | UNBOX LPAREN kind = boxkind RPAREN DOT stk = stk_trail
    {S.box ~loc:(position $symbolstartpos $endpos) kind stk}
  | FIX LPAREN RPAREN DOT stk = stk_trail
    {CoFix {loc = (position $symbolstartpos $endpos); stk}}
  | BIND pol = pol_annot x = paren_typed_var ARROW cmd = cmd
    {let (x,t) = x in S.bind ~loc:(position $symbolstartpos $endpos) ?pol:pol t x cmd}
  | MATCH patt = patt
    {S.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH CASE patts = separated_list(CASE,patt) END
    {S.case ~loc:(position $symbolstartpos $endpos) patts}

patt:
  | cons = cons ARROW cmd = cmd { (cons, cmd) }

cons:
  | cons = consvar LPAREN args = separated_list(COMMA, typed_var) RPAREN
    { poscons cons args }
  | UNIT {unit}
  | TUPPLE LPAREN vs = separated_list(COMMA, typed_var) RPAREN { Tupple vs }
  | LEFT LPAREN a = typed_var RPAREN {left a}
  | RIGHT LPAREN b = typed_var RPAREN {right b}
  | INJ LPAREN i = NUM SLASH n = NUM COMMA a = typed_var RPAREN { Inj (i,n,a) }
  | SHIFT PLUS LPAREN a = typed_var RPAREN {shift_pos a}


(* Méta-langage *)

data_cons_def:
  | cons = consvar LPAREN args = separated_list(COMMA, typ) RPAREN
    {poscons cons args}
  | cons = consvar {poscons cons []}

codata_cons_def:
  | THIS DOT cons = destrvar LPAREN RPAREN
    DOT RET LPAREN RPAREN COLUMN typ = typ
    {negcons cons [] typ}
  | THIS DOT cons = destrvar LPAREN args = separated_nonempty_list(COMMA, typ) RPAREN
    DOT RET LPAREN RPAREN COLUMN typ = typ
    {negcons cons args typ}

prog:
  | EOF {[]}
  | prog = prog_rev EOF {prog}

prog_rev:
  | first = prog_item rest = prog_rev {(first :: rest)}
  | last = prog_item {[last]}

prog_item:
  | DECL TYPE name = tvar COLUMN sort = sort
    {Type_declaration {name; sort;loc = position $symbolstartpos $endpos}}

  | TYPE name = tvar args = list(typ_arg) sort = sort_annot EQUAL content = typ
    {Type_definition {name;args;sort;content;loc = position $symbolstartpos $endpos}}

  | DATA name = tvar args = list(typ_arg) EQUAL
    CASE content = separated_nonempty_list(CASE, data_cons_def)
    { Data_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  | CODATA name = tvar args = list(typ_arg) EQUAL
    CASE content = separated_nonempty_list(CASE, codata_cons_def)
    { Codata_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  | CMD META? name = var typ = typ_annot EQUAL content = cmd
    { Cmd_execution {name = Some name;content; typ ;loc = position $symbolstartpos $endpos} }

  | CMD DO content = cmd
    { Cmd_execution {name = None; content; typ = None; loc = position $symbolstartpos $endpos} }

  | TERM META? name = var typ = typ_annot EQUAL content = value
    { Term_definition {name;typ;content;loc = position $symbolstartpos $endpos} }

  | DECL TERM META? name = var COLUMN typ = typ
    { Term_declaration {name;typ;loc = position $symbolstartpos $endpos} }
