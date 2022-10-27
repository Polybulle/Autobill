{
  open Parser
  open Lexing
  exception Error of string
}

let num = ['0'-'9']*
let alphanum = ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let meta = [^ '>']*
let name = ['a'-'z'] alphanum
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule token = parse

  | "//" {line_comment lexbuf}
  | "/*" {delim_comment lexbuf}

  | '(' {LPAREN}
  | ')' {RPAREN}
  | '[' {LBRACKET}
  | ']' {RBRACKET}
  | ':' {COLUMN}
  | '+' {PLUS}
  | '-' {MINUS}
  | '=' {EQUAL}
  | '.' {DOT}
  | ',' {COMMA}
  | "->" {ARROW}
  | '/' {SLASH}

  | "ret" {RET}
  | "this" {THIS}
  | "bind" {BIND}
  | "bind/cc" {BINDCC}
  | "match" {MATCH}
  | "case" {CASE}
  | "end" {END}
  | "fun" {FUN}
  | "in" {IN}
  | "fix" {FIX}
  | "pack" {PACK}
  | "spec" {SPEC}

  | "GOT_ZERO" {GOT_ZERO}
  | "GOT_TOP" {GOT_TOP}

  | "box" {BOX}
  | "unbox" {UNBOX}
  | "lin" {LINEAR}
  | "aff" {AFFINE}
  | "exp" {EXP}

  | "tupple" {TUPPLE}
  | "left" {LEFT}
  | "right" {RIGHT}
  | "inj" {INJ}
  | "call" {CALL}
  | "yes" {YES}
  | "no" {NO}
  | "proj" {PROJ}
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
  | "comput" {COMPUT}
  | "val" {VAL}
  | "stk" {STK}
  | "cmd" {CMD}

  | num {NUM (int_of_string (Lexing.lexeme lexbuf))}
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
