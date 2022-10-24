open Autobill
open Cst_intf
open Intern_intf
open Sort_intf
open Reduction_intf
open TypeInfer_intf

let version = "v0.0.4-alpha"

let usage_spiel =
  {|usage: autobill [options] [input_file]|}

type subcommand =
  | Version
  | Parse
  | Intern
  | SortInfer
  | Constraint
  | TypeInfer

let do_trace = ref false

let do_simplify = ref false

let in_ch = ref stdin

let in_name = ref "<stdin>"

let out_ch = ref stdout

let subcommand = ref TypeInfer

let set_input_file name =
  in_name := name;
  in_ch := open_in name

let set_output_file name =
  out_ch := open_out name

let parse_cli_invocation () =
  let open Arg in
  let process x () = subcommand := x in
  let speclist = [
    ("-v", Unit (process Version), "Print version and exit");
    ("-p", Unit (process Parse), "Just parse the program");
    ("-i", Unit (process Intern), "Parse and internalize");
    ("-s", Unit (process SortInfer), "Parse, internalize, and infer sorts");
    ("-c", Unit (process Constraint), "All of the above, and generate a type contraint");
    ("-t", Unit (process TypeInfer), "All of the above, and typecheck");
    ("-o", String set_output_file, "Set output file");
    ("-V", Set do_trace, "Trace the sort and type inference");
    ("-r", Set do_simplify, "Simplify source file before type inference");
  ] in
  Arg.parse speclist set_input_file usage_spiel

let stop_if_cmd comm' final =
  if !subcommand = comm' then begin
    output_string !out_ch (final ());
    exit 0
  end

let string_of_full_ast prog =
  PrettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()


let () =

 let _ = parse_cli_invocation () in

  stop_if_cmd Version (fun () -> version);

  let cst = parse_cst !in_name !in_ch in
  stop_if_cmd Parse (fun () -> string_of_cst cst);

  let prog, env = intern_error_wrapper (fun () -> internalize cst) in
  stop_if_cmd Intern (fun () -> string_of_intern_ast (env.prelude, prog));

  let prog = intern_error_wrapper (fun () -> polarity_inference ~trace:!do_trace env prog) in
  let prog = if !do_simplify then interpret_prog prog else prog in
  stop_if_cmd SortInfer (fun () -> string_of_full_ast prog);

  let s = constraint_as_string prog in
  stop_if_cmd Constraint (fun () -> s);

  let prog = type_infer ~trace:!do_trace prog in
  stop_if_cmd TypeInfer (fun () -> string_of_full_ast prog);

  print_endline "Not yet implemented.";
  exit 1
