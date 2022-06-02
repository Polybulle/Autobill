open Autobill
open Lexing

let usage_spiel =
  {|usage: autobill <subcommand> <input_file>
      allowed subcommands: parse, intern
      if input_file is omitted, input is read from stdin|}

type subcommand =
  | Parse
  | Intern

let parse_command = function
  | "parse" -> Parse
  | "intern" -> Intern
  | _ -> print_endline usage_spiel; exit 1

let parse_cli_invocation () =
  match Sys.argv with
  | [|_; comm|] -> parse_command comm, stdin, "<stdin>"
  | [|_;comm; in_file|] -> parse_command comm, open_in in_file, in_file
  | _ -> print_endline usage_spiel; exit 1

let pos_of_error lexbuf =
  Printf.sprintf "%d:%d"
      lexbuf.lex_curr_p.pos_lnum
      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let parse lexbuf =
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^  msg))
  | Parser.Error ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^ " syntax error"))

let parse_cst name inch =
  let lexbuf = Lexing.from_channel ~with_positions:true inch in
  Lexing.set_filename lexbuf name;
  parse lexbuf

let intern_cst cst =
  let prelude, prog, env = Intern_prog.internalize cst in
  prelude, prog

let string_of_cst prog =
  CstPrettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let string_of_intern_ast prog =
  Intern_prettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()


let () =
  let comm, inch, name = parse_cli_invocation () in
  let stop_if_cmd comm' final =
    if comm = comm' then begin
      print_newline ();
      final ();
      exit 0
    end in
  let cst = parse_cst name inch in
  stop_if_cmd Parse (fun () -> print_endline (string_of_cst cst));
  let intern = intern_cst cst in
  stop_if_cmd Intern (fun () -> print_endline (string_of_intern_ast intern))
