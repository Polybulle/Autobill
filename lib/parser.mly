%{
    open Calculi.PreLAMECalc
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token COLUMN PLUS TILDE EQUAL MINUS DOT ARROW COMMA BAR
%token LPAREN RPAREN
%token STEP INTO WITH BIND BINDCC MATCH RET END THIS
%token BOX UNBOX LINEAR AFFINE EXP
%token PAIR LEFT RIGHT CALL YES NO
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM
%token DECL TYPE DATA CODATA TERM ENV CMD
%token <string> VAR
%token EOF

%start <program> prog
%%


(* Généralités  *)

pol:
  | PLUS {positive}
  | MINUS {negative}
  | TILDE {ambiguous}

pol_annot:
  | pol = pol {pol}
  | {ambiguous}

sort:
  | pol = pol {sort_base pol}

sort_annot:
  | COLUMN so = sort {Some so}
  | {None}

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

destrvar:
  | name = VAR {ConsVar.of_string name}

var:
  | name = VAR {Var.of_string name}

typ_annot:
  | COLUMN typ = typ {Some typ}
  | {None}

paren_typed_var:
  | v = var { (v, None) }
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_var:
  | var = var typ = typ_annot { (var, typ) }


typ_arg:
  | v = tvar { (v, None) }
  | LPAREN v = tvar sort = sort_annot RPAREN {(v , sort)}


(* Types *)

typ:
  | c = one_word_typ_cons {cons c}
  | var = tvar {tvar var}
  | LPAREN kind = boxkind content = typ RPAREN {boxed kind content}
  | LPAREN c = typ_cons RPAREN {cons c}
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
  | c = tconsvar args = list(typ) {typecons c args}

(* Terms *)

cmd:
  | STEP po = pol_annot valu = value typ = typ_annot INTO stk = stack {cmd po typ valu stk}
  | STEP po = pol_annot INTO stk = stack typ = typ_annot WITH valu = value {cmd po typ valu stk}
  | valu = value DOT stk = stk_trail {cmd ambiguous None valu stk}

cont_annot:
  | LPAREN RET LPAREN RPAREN typ = typ_annot RPAREN {typ}
  | {None}

value:
  | v = var {V.var v}
  | c = value_cons {V.cons c}
  | BOX LPAREN kind = boxkind RPAREN typ = cont_annot ARROW cmd = cmd {V.box kind typ cmd}
  | BINDCC po = pol_annot typ = cont_annot ARROW cmd = cmd {V.bindcc po typ cmd}
  | MATCH patt = copatt {V.case [patt]}
  | MATCH BAR patts = separated_list(BAR,copatt) END {V.case patts}

copatt:
  | THIS DOT destr = destr ARROW cmd = cmd { (destr, cmd) }

destr:
  | cons = destrvar LPAREN args = separated_list(COMMA, typed_var) RPAREN
    DOT RET LPAREN RPAREN typ = typ_annot
    { negcons cons args typ }
  | CALL LPAREN var = typed_var RPAREN DOT RET LPAREN RPAREN typ = typ_annot { call var typ }
  | YES  LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {yes typ}
  | NO   LPAREN RPAREN                 DOT RET LPAREN RPAREN typ = typ_annot {no typ}

value_cons:
  | UNIT LPAREN RPAREN { unit }
  | PAIR LPAREN a = value COMMA b = value RPAREN {pair a b}
  | LEFT LPAREN a = value RPAREN {fst a}
  | RIGHT LPAREN b = value RPAREN {snd b}
  | cons = consvar LPAREN args = separated_list(COMMA,value) RPAREN {poscons cons args}

stack:
  | THIS DOT stk = stk_trail {stk}

stk_trail:
  | RET LPAREN RPAREN {S.ret}
  | CALL LPAREN x = value RPAREN DOT stk = stk_trail {S.destr (call x stk)}
  | YES LPAREN RPAREN DOT stk = stk_trail {S.destr (yes stk)}
  | NO LPAREN RPAREN DOT stk = stk_trail {S.destr (no stk)}
  | cons = destrvar LPAREN args = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr (negcons cons args stk)}
  | UNBOX LPAREN kind = boxkind RPAREN DOT stk = stk_trail {S.box kind stk}
  | BIND po = pol_annot x = paren_typed_var ARROW cmd = cmd {let (x,t) = x in S.bind po t x cmd}
  | MATCH patt = patt {S.case [patt]}
  | MATCH BAR patts = separated_list(BAR,patt) END {S.case patts}

patt:
  | cons = cons ARROW cmd = cmd { (cons, cmd) }

cons:
  | cons = consvar LPAREN args = separated_list(COMMA, typed_var) RPAREN
    { poscons cons args }
  | UNIT {unit}
  | PAIR LPAREN a = typed_var COMMA b = typed_var RPAREN { pair a b }
  | LEFT LPAREN a = typed_var RPAREN {fst a}
  | RIGHT LPAREN b = typed_var RPAREN {snd b}


(* Méta-langage *)

data_cons_def:
  | cons = consvar LPAREN args = separated_nonempty_list(COMMA, typ) RPAREN
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
    {Type_declaration {name; sort}}

  | TYPE name = tvar args = list(typ_arg) sort = sort_annot EQUAL content = typ
    {Type_definition {name;args;sort;content}}

  | DATA name = tvar args = list(typ_arg) EQUAL
    BAR content = separated_nonempty_list(BAR, data_cons_def)
    { Data_definition{name; args; content} }

  | CODATA name = tvar args = list(typ_arg) EQUAL
    BAR content = separated_nonempty_list(BAR, codata_cons_def)
    { Codata_definition{name; args; content} }

  | CMD name = var typ = typ_annot EQUAL content = cmd
    { Cmd_definition{name;content; typ} }

  | TERM name = var typ = typ_annot EQUAL content = value { Term_definition {name;typ;content} }

  | ENV name = var typ = typ_annot EQUAL content = stack { Env_definition {name;typ;content} }
