{
  open Parser
  exception Error of string
}

let name = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse

  | '(' {LPAREN}
  | ')' {RPAREN}
  | ':' {COLUMN}
  | '+' {PLUS}
  | '~' {TILDE}
  | '-' {MINUS}
  | '=' {EQUAL}
  | '|' {BAR}
  | '.' {DOT}
  | ',' {COMMA}
  | "->" {ARROW}

  | "ret" {RET}
  | "this" {THIS}
  | "step" {STEP}
  | "into" {INTO}
  | "with" {WITH}
  | "bind" {BIND}
  | "bind/cc" {BINDCC}
  | "match" {MATCH}
  | "end" {END}

  | "box" {BOX}
  | "unbox" {UNBOX}
  | "lin" {LINEAR}
  | "aff" {AFFINE}
  | "exp" {EXP}

  | "pair" {PAIR}
  | "left" {LEFT}
  | "right" {RIGHT}
  | "call" {CALL}
  | "yes" {YES}
  | "no" {NO}

  | "unit" {UNIT}
  | "zero" {ZERO}
  | "prod" {PROD}
  | "sum" {SUM}
  | "fun" {FUN}
  | "choice" {CHOICE}
  | "top" {TOP}
  | "bottom" {BOTTOM}

  | "decl" {DECL}
  | "type" {TYPE}
  | "data" {DATA}
  | "codata" {CODATA}
  | "term" {TERM}
  | "env" {ENV}
  | "cmd" {CMD}

  | "//" {line_comment lexbuf}
  | "/*" {delim_comment lexbuf}

  | name {VAR (Lexing.lexeme lexbuf)}
  | eof {EOF}
  | whitespace {token lexbuf}
  | _ {raise (Error ("Lexing failed because of unexpected: " ^ Lexing.lexeme lexbuf))}

and line_comment = parse
  | [^ '\n' ]+ { line_comment lexbuf }
  | '\n' {token lexbuf}
   | eof {EOF}

and delim_comment = parse
  | "*/" {token lexbuf}
  | _ { delim_comment lexbuf }
