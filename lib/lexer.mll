{
  open Parser
  exception Error of string
}

let small_name = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let big_name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

let whitespace = [' ' '\t' '\n' '\r']+

rule token = parse
  | '(' {LPAREN}
  | ')' {RPAREN}
  | '{' {LCURLY}
  | '}' {RCURLY}
  | ("jump" | "call" | "force" | "let" | "match" |
     "box" | "unbox" | "lin" | "aff" | "exp") {KEYWORD (Lexing.lexeme lexbuf)}
  | '!' small_name {COVAR (Lexing.lexeme lexbuf)}
  | ':' small_name {COVAR (Lexing.lexeme lexbuf)}
  | small_name {VAR (Lexing.lexeme lexbuf)}
  | big_name {TYVAR (Lexing.lexeme lexbuf)}
  | eof {END}
  | whitespace {token lexbuf}
  | _ {raise (Error ("Lexing failed because of unexpected: " ^ Lexing.lexeme lexbuf))}
