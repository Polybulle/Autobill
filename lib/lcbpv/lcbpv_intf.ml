open Lcbpv
open Lcbpv_Printer
open Cst
open Lexing
open Lcbpv_lexer
open Lcbpv_parser

let converted_prog = [
  Cmd_execution {
    name = None;
    typ = None;
    cont = "alpha";
    loc = Misc.dummy_pos;
    content = Command {
        pol = None;
        typ = None;
        valu = Var {node = "x"; loc = Misc.dummy_pos};
        stk = Ret {var = "alpha"; loc = Misc.dummy_pos};
        loc = Misc.dummy_pos
      }
  }
]


let pos_of_error lexbuf =
  Printf.sprintf "%d:%d"
      lexbuf.lex_curr_p.pos_lnum
      (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol)

let parse lexbuf =
  try
    prog token lexbuf
  with
  | Lexer.Error msg ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^  msg))
  | Parser.Error ->
    raise (Failure (pos_of_error lexbuf ^ ":" ^ " syntax error"))

let parse_cst name inch =
  let lexbuf = from_channel ~with_positions:true inch in
  set_filename lexbuf name;
  parse lexbuf


let string_of_cst prog =
  pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let convert_to_machine_code prog = converted_prog
