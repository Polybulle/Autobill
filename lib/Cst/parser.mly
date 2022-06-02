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

%token COLUMN PLUS EQUAL MINUS DOT ARROW COMMA META
%token LPAREN RPAREN
%token STEP INTO WITH BIND BINDCC MATCH CASE RET END THIS IN
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM SHIFT
%token DECL TYPE DATA CODATA TERM ENV CMD
%token <string> VAR
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
  | LPAREN arg = sort RPAREN ARROW ret = sort {sort_dep arg ret}

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
  | c = one_word_typ_cons {c}
  | PROD a = typ b = typ {prod a b}
  | SUM a = typ b = typ {sum a b}
  | CHOICE a = typ b = typ {choice a b}
  | FUN a = typ b = typ {func a b}
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
  | c = value_cons
    {V.cons ~loc:(position $symbolstartpos $endpos) c}
  | BOX LPAREN kind = boxkind RPAREN typ = cont_annot ARROW cmd = cmd
    {V.box ~loc:(position $symbolstartpos $endpos) kind typ cmd}
  | BINDCC pol = pol_annot typ = cont_annot ARROW cmd = cmd
    {V.bindcc ~loc:(position $symbolstartpos $endpos) ?pol:pol typ cmd}
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
  | CALL LPAREN var = typed_var RPAREN DOT RET LPAREN RPAREN typ = typ_annot {call var typ}
  | YES  LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {yes typ}
  | NO   LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {no typ}
  | SHIFT MINUS LPAREN RPAREN            DOT RET LPAREN RPAREN typ = typ_annot {shift_neg typ}

value_cons:
  | UNIT LPAREN RPAREN { unit }
  | PAIR LPAREN a = value COMMA b = value RPAREN {pair a b}
  | LEFT LPAREN a = value RPAREN {left a}
  | RIGHT LPAREN b = value RPAREN {right b}
  | SHIFT PLUS LPAREN a = value RPAREN {shift_pos a}
  | cons = consvar LPAREN args = separated_list(COMMA,value) RPAREN {poscons cons args}

stack:
  | THIS DOT stk = stk_trail {stk}

stk_trail:
  | RET LPAREN RPAREN
    {S.ret ~loc:(position $symbolstartpos $endpos) ()}
  | CALL LPAREN x = value RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (call x stk)}
  | YES LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (yes stk)}
  | NO LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (no stk)}
  | SHIFT MINUS LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (shift_neg stk)}
  | cons = destrvar LPAREN args = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (negcons cons args stk)}
  | UNBOX LPAREN kind = boxkind RPAREN DOT stk = stk_trail
    {S.box ~loc:(position $symbolstartpos $endpos) kind stk}
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
  | PAIR LPAREN a = typed_var COMMA b = typed_var RPAREN { pair a b }
  | LEFT LPAREN a = typed_var RPAREN {left a}
  | RIGHT LPAREN b = typed_var RPAREN {right b}
  | SHIFT PLUS a = typed_var RPAREN {shift_pos a}


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
    { Cmd_definition{name;content; typ ;loc = position $symbolstartpos $endpos} }

  | TERM META? name = var typ = typ_annot EQUAL content = value
    { Term_definition {name;typ;content;loc = position $symbolstartpos $endpos} }

  | ENV META? name = var typ = typ_annot EQUAL content = stack
    { Env_definition {name;typ;content; loc = position $symbolstartpos $endpos} }
