
let string_of_miniml prog =
  FormatML.fmp_prog Format.str_formatter prog;
  Format.flush_str_formatter ()

let parse_miniml name inch =
  let lexbuf = Lexing.from_channel ~with_positions:true inch in
  Lexing.set_filename lexbuf name;
  try ParserML.prog LexerML.token lexbuf with
  | LexerML.Error c ->
   Misc.fatal_error
     "Lexing ML lexbuf"
      (Printf.sprintf "Unrecognized char '%s'" c)
      ~loc:(Misc.position (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))
  | ParserML.Error ->
    Misc.fatal_error
      "Parsing ML lexbuf"
      "Syntax error"
      ~loc:(Misc.position (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))

let parse_miniml_from_lexbuf lexbuf =
  try ParserML.prog LexerML.token lexbuf with
  | LexerML.Error c ->
   Misc.fatal_error
     "Lexing ML lexbuf"
      (Printf.sprintf "Unrecognized char '%s'" c)
      ~loc:(Misc.position (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))
  | ParserML.Error ->
    Misc.fatal_error
      "Parsing ML lexbuf"
      "Syntax error"
      ~loc:(Misc.position (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf))

let lcbpv_of_miniml code =
  let module M = Lcbpv_of_ML.Converter () in
  M.trans_prog code
