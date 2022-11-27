%{
    open Constructors
    open Types
    open Cst
    open Misc
    (* Due to a bug in the dune/menhir interaction, we need to define a dummy "Autobill"*)
    (* module to avoid incorrect resolving of modules leading to cyclical build dependency.*)
    (* see https://github.com/ocaml/dune/issues/2450 *)
    module Autobill = struct end
%}

%token COLUMN PLUS EQUAL MINUS DOT ARROW COMMA SLASH META
%token LPAREN RPAREN LBRACKET RBRACKET
%token VAL STK CMD BIND BINDCC MATCH CASE RET END IN
%token TUPPLE INJ CALL PROJ LEFT RIGHT YES NO PACK SPEC THIS FIX
%token GOT_TOP GOT_ZERO
%token BOX UNBOX LINEAR AFFINE EXP
%token UNIT ZERO PROD SUM FUN CHOICE TOP BOTTOM THUNK CLOSURE
%token DECL TYPE DATA COMPUT SORT
%token <string> VAR
%token <int> NUM
%token EOF

%start <program> prog
%%

(* Généralités  *)

tvar:
  | name = VAR META? {name}

tconsvar:
  | name = VAR META? {name}

consvar:
  | name = VAR META? {name}

destrvar:
  | name = VAR META? {name}

var:
  | name = VAR META? {name}

covar:
  | name = VAR META? {name}

sortvar:
  | name = VAR META? {name}

pol:
  | PLUS {positive}
  | MINUS {negative}

sort:
  | pol = pol {sort_base pol}
  | so = sortvar {sort_idx so}
  | LPAREN s = sort ARROW t = sort RPAREN {sort_arrow [s] t}

boxkind:
  | LINEAR {linear}
  | AFFINE {affine}
  | EXP {exp}

(* Binders *)

typ_annot:
  | COLUMN typ = typ {Some typ}
  | COLUMN META {None}
  | {None}

pol_annot:
  | pol = pol {Some pol}
  | META {None}
  | {None}

sort_annot:
  | COLUMN so = sort {so}

paren_typed_var:
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_var:
  | v = var typ = typ_annot { (v, typ) }
  | bind = paren_typed_var {bind}

paren_typed_covar:
  | LPAREN var = var typ = typ_annot RPAREN { (var, typ) }

typed_covar:
  | var = covar typ = typ_annot { (var, typ) }
  | bind = paren_typed_covar {bind}

paren_sorted_tyvar:
  | LPAREN v = tvar sort = sort_annot RPAREN {(v , sort)}

sorted_tyvar:
  | v = tvar sort = sort_annot {(v , sort)}
  | a = paren_sorted_tyvar {a}


(* Types *)

typ:
  | var = tvar
    {tvar ~loc:(position $symbolstartpos $endpos) var}
  | PLUS typ = typ {pos typ}
  | MINUS typ = typ {neg typ}
  | UNIT {unit_t}
  | ZERO {zero}
  | TOP {top}
  | BOTTOM {bottom}
  | LPAREN kind = boxkind content = typ RPAREN
    {boxed ~loc:(position $symbolstartpos $endpos) kind content}
  | LPAREN FIX a = typ RPAREN {fix a}
  | LPAREN PROD a = nonempty_list(typ) RPAREN {prod a}
  | LPAREN SUM a = nonempty_list(typ) RPAREN {sum a}
  | LPAREN CHOICE a = nonempty_list(typ) RPAREN {choice a}
  | LPAREN FUN a = nonempty_list(typ) RPAREN {func a}
  | LPAREN THUNK a = typ RPAREN {thunk_t a}
  | LPAREN CLOSURE a = typ RPAREN {closure_t a}
  | LPAREN c = tconsvar args = list(typ) RPAREN {typecons c args}


(* Terms *)

cmd:
  | CMD pol = pol_annot typ = typ_annot VAL EQUAL valu = value STK EQUAL stk = stack END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | CMD pol = pol_annot typ = typ_annot STK EQUAL stk = stack VAL EQUAL valu = value END
    {cmd ~loc:(position $symbolstartpos $endpos) ?pol typ valu stk}
  | valu = value DOT stk = stk_trail
    {cmd ~loc:(position $symbolstartpos $endpos) None valu stk}

  | VAL pol = pol_annot bind = typed_var EQUAL v = value IN c = cmd
    {let x,annot = bind in
     cmd_let_val ~loc:(position $symbolstartpos $endpos) ?pol:pol x annot v c }
  | STK pol = pol_annot bind = typed_covar EQUAL stk = stack IN c = cmd
    {let a, annot = bind in
     cmd_let_env ~loc:(position $symbolstartpos $endpos) ?pol:pol a annot stk c }
  | MATCH pol = pol_annot cons = cons EQUAL valu = value IN c = cmd
    {cmd_match_val ~loc:(position $symbolstartpos $endpos) ?pol:pol valu cons c }
  | MATCH pol = pol_annot STK THIS DOT destr = destr EQUAL stk = stack IN c = cmd
    {cmd_match_env ~loc:(position $symbolstartpos $endpos) ?pol:pol stk destr c}

value:
  | v = var
    {V.var ~loc:(position $symbolstartpos $endpos) v}
  | GOT_TOP
    {V.cotop ~loc:(position $symbolstartpos $endpos) ()}
  | c = value_cons
    {V.cons ~loc:(position $symbolstartpos $endpos) c}
  | BOX LPAREN kind = boxkind RPAREN a = typed_covar ARROW cmd = cmd
    {let a,t = a in V.box ~loc:(position $symbolstartpos $endpos) kind a t cmd}
  | BINDCC pol = pol_annot a = typed_covar ARROW cmd = cmd
    {let (a,t) = a in V.bindcc ~loc:(position $symbolstartpos $endpos) ?pol:pol a t cmd}

  | MATCH THIS DOT FIX LPAREN self = typed_var RPAREN DOT RET cont = paren_typed_covar
    typ_annot ARROW cmd = cmd
    {Fix {self; cmd; cont; loc = (position $symbolstartpos $endpos)}}
  | MATCH patt = copatt
    {V.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH CASE patts = separated_list(CASE,copatt) END
    {V.case ~loc:(position $symbolstartpos $endpos) patts}
  | cons = consvar LBRACKET typs = separated_list(COMMA,typ) RBRACKET LPAREN content = value RPAREN
    {Pack {cons; typs; content; loc = (position $symbolstartpos $endpos)}}
  | MATCH THIS DOT destr = destrvar LBRACKET spec_vars = separated_list(COMMA, sorted_tyvar)
    RBRACKET LPAREN RPAREN DOT RET bind = paren_typed_covar  ARROW cmd = cmd
    {Spec {destr; spec_vars; cmd; bind; loc = (position $symbolstartpos $endpos)}}

  | FUN x = paren_typed_var ARROW v = value
    {let (arg, typ) = x in V.macro_fun ~loc:(position $symbolstartpos $endpos) arg typ v}
  | BOX LPAREN kind = boxkind COMMA v = value RPAREN
    {V.macro_box ~loc:(position $symbolstartpos $endpos) kind v}


copatt:
  | THIS DOT destr = destr ARROW cmd = cmd { (destr, cmd) }

destr:
  | cons = destrvar LPAREN args = separated_list(COMMA, typed_var) RPAREN
    DOT RET cont = paren_typed_covar
    { negcons cons args cont }
  | CALL LPAREN vars = separated_list(COMMA, typed_var) RPAREN DOT RET cont = paren_typed_covar
    {Call (vars, cont)}
  | YES  LPAREN RPAREN                 DOT RET cont = paren_typed_covar {yes cont}
  | NO   LPAREN RPAREN                 DOT  RET cont = paren_typed_covar {no cont}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT RET cont = paren_typed_covar {Proj (i,n,cont)}
  | CLOSURE LPAREN RPAREN            DOT RET cont = paren_typed_covar {closure cont}

value_cons:
  | UNIT LPAREN RPAREN { unit }
  | TUPPLE LPAREN xs = separated_list(COMMA, value) RPAREN {Tupple xs}
  | LEFT LPAREN a = value RPAREN {left a}
  | RIGHT LPAREN b = value RPAREN {right b}
  | INJ LPAREN i = NUM SLASH n = NUM COMMA a = value RPAREN {Inj (i,n,a)}
  | THUNK LPAREN a = value RPAREN {thunk a}
  | cons = consvar LPAREN args = separated_list(COMMA,value) RPAREN {poscons cons args}

stack:
  | THIS DOT stk = stk_trail {stk}

stk_trail:
  | GOT_ZERO LPAREN RPAREN
    {S.cozero ~loc:(position $symbolstartpos $endpos) ()}
  | RET LPAREN a = covar RPAREN
    {S.ret ~loc:(position $symbolstartpos $endpos) a}
  | CALL LPAREN xs = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Call (xs, stk))}
  | YES LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (yes stk)}
  | NO LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (no stk)}
  | PROJ LPAREN i = NUM SLASH n = NUM RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (Proj (i,n, stk))}
  | CLOSURE LPAREN RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (closure stk)}
  | cons = destrvar LPAREN args = separated_list(COMMA, value) RPAREN DOT stk = stk_trail
    {S.destr ~loc:(position $symbolstartpos $endpos) (negcons cons args stk)}
  | UNBOX LPAREN kind = boxkind RPAREN DOT stk = stk_trail
    {S.box ~loc:(position $symbolstartpos $endpos) kind stk}
  | FIX LPAREN RPAREN DOT stk = stk_trail
    {CoFix {loc = (position $symbolstartpos $endpos); stk}}
  | BIND pol = pol_annot x = typed_var ARROW cmd = cmd
    {let (x,t) = x in S.bind ~loc:(position $symbolstartpos $endpos) ?pol:pol x t cmd}
  | MATCH patt = patt
    {S.case ~loc:(position $symbolstartpos $endpos) [patt]}
  | MATCH CASE patts = separated_list(CASE,patt) END
    {S.case ~loc:(position $symbolstartpos $endpos) patts}
  | destr = destrvar LBRACKET typs = separated_list(COMMA,typ) RBRACKET LPAREN RPAREN DOT content = stk_trail
    {CoSpec {destr; typs; content; loc = (position $symbolstartpos $endpos)}}
  | MATCH cons = consvar LBRACKET pack_vars = separated_list(COMMA, sorted_tyvar) RBRACKET
    LPAREN x = var t = typ_annot RPAREN ARROW cmd = cmd
    {CoPack {cons; pack_vars; bind = (x,t); cmd; loc = (position $symbolstartpos $endpos)}}

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
  | THUNK LPAREN a = typed_var RPAREN {thunk a}


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
  | first = prog_item rest = prog {(first :: rest)}

prog_item:
  | DECL SORT name = sortvar
    {Sort_declaration {name; loc = position $symbolstartpos $endpos}}

  | DECL TYPE name = tvar COLUMN sort = sort
    {Type_declaration {name; sort;loc = position $symbolstartpos $endpos}}

  | TYPE name = tvar args = list(paren_sorted_tyvar) sort = sort_annot EQUAL content = typ
    {Type_definition {name;args;sort;content;loc = position $symbolstartpos $endpos}}

  | DATA name = tvar args = list(paren_sorted_tyvar) EQUAL
    CASE content = separated_nonempty_list(CASE, data_cons_def)
    { Data_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  | COMPUT name = tvar args = list(paren_sorted_tyvar) EQUAL
    CASE content = separated_nonempty_list(CASE, codata_cons_def)
    { Codata_definition{name; args; content;loc = position $symbolstartpos $endpos} }

  (* pack et spec *)
  | PACK name = tvar sort_annot? args = list(paren_sorted_tyvar) EQUAL
     cons = consvar LBRACKET private_typs = separated_list(COMMA, sorted_tyvar) RBRACKET
    LPAREN arg_typ = typ RPAREN
    {Pack_definition {name; args; cons; private_typs; arg_typs = [arg_typ];
    loc = position $symbolstartpos $endpos}}

  | SPEC name = tvar sort_annot? args = list(paren_sorted_tyvar) EQUAL THIS DOT
    destr = destrvar LBRACKET private_typs = separated_list(COMMA, sorted_tyvar) RBRACKET
    LPAREN RPAREN DOT RET LPAREN RPAREN COLUMN ret_typ = typ
    {Spec_definition {name; args; destr; private_typs; arg_typs = []; ret_typ;
    loc = position $symbolstartpos $endpos}}

  | CMD META? name = option(var) RET cont = typed_covar EQUAL content = cmd
    { let cont, typ = cont in
      Cmd_execution { name; content; cont; typ; loc = position $symbolstartpos $endpos} }

  | VAL META? name = var typ = typ_annot EQUAL content = value
    { Term_definition {name;typ;content;loc = position $symbolstartpos $endpos} }

  | DECL VAL META? name = var COLUMN typ = typ
    { Term_declaration {name;typ;loc = position $symbolstartpos $endpos} }
