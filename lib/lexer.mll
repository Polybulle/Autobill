{
  open Parser
  exception Error of string
}

let name = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse

  | '(' {LPAREN}
  | ')' {RPAREN}
  | '{' {LCURLY}
  | '}' {RCURLY}
  | '!' {BANG}
  | ':' {COLUMN}
  | '?' {QUESTION}
  | '+' {PLUS}
  | '~' {TILDE}
  | '-' {MINUS}
  | '=' {EQUAL}
  | ';' {SEMICOL}
  | '*' {STAR}

  | "jump" {JUMP}
  | "enter" {ENTER}
  | "force" {FORCE}
  | "let" {LET}
  | "match" {MATCH}

  | "typ~" {TYPNEUT}
  | "typ+" {TYPPOS}
  | "typ-" {TYPMIN}

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

  | name {VAR (Lexing.lexeme lexbuf)}
  | eof {END}
  | whitespace {token lexbuf}
  | _ {raise (Error ("Lexing failed because of unexpected: " ^ Lexing.lexeme lexbuf))}
