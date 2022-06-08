{
  open Parser
  open Lexing
  exception Error of string
}

let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let meta = [^ '>']*
let name = ['a'-'z'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse

  | '(' {LPAREN}
  | ')' {RPAREN}
  | ':' {COLUMN}
  | '+' {PLUS}
  | '-' {MINUS}
  | '=' {EQUAL}
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
  | "case" {CASE}
  | "end" {END}
  | "fun" {FUN}
  | "in" {IN}

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
  | "shift" {SHIFT}

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
  | '<' meta '>' {META}
  | eof {EOF}
  | white {token lexbuf}
  | newline {new_line lexbuf; token lexbuf}
  | _ {raise (Error (
    Printf.sprintf "Lexing failed because of unexpected %s" (Lexing.lexeme lexbuf)))}

and line_comment = parse
  | [^ '\n' ]+ { line_comment lexbuf }
  | newline {new_line lexbuf; token lexbuf}
  | eof {EOF}

and delim_comment = parse
  | "*/" {token lexbuf}
  | newline {new_line lexbuf; delim_comment lexbuf}
  | eof {raise (Error ("Lexing failed because unclosed /* */ comment"))}
  | _ { delim_comment lexbuf }
