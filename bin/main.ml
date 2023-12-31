open Autobill
open Cst_intf
open Intern_intf


let version =
  match Build_info.V1.version () with
  | None -> "dev"
  | Some v -> Build_info.V1.Version.to_string v

let usage_spiel =
  {|usage: autobill <subcommand> <input_file>
      allowed subcommands: parse, intern, polinfer, version
      if input_file is omitted, input is read from stdin|}

type subcommand =
  | Version
  | Parse
  | Intern
  | PolInfer

let parse_command = function
  | "version" -> Version
  | "parse" -> Parse
  | "intern" -> Intern
  | "polinfer" -> PolInfer
  | _ -> print_endline usage_spiel; exit 1

let parse_cli_invocation () =
  match Sys.argv with
  | [|_; comm|] -> parse_command comm, stdin, "<stdin>"
  | [|_;comm; in_file|] -> parse_command comm, open_in in_file, in_file
  | _ -> print_endline usage_spiel; exit 1

let string_of_full_ast prog =
  PrettyPrinter.pp_program Format.str_formatter prog;
  Format.flush_str_formatter ()

let () =

  let comm, inch, name = parse_cli_invocation () in

  let stop_if_cmd comm' final =
    if comm = comm' then begin
      final ();
      exit 0
    end in

  stop_if_cmd Version (fun () -> print_endline version);

  let cst = parse_cst name inch in
  stop_if_cmd Parse (fun () -> print_endline (string_of_cst cst));

  let prog, env = intern_error_wrapper (fun () -> internalize cst) in
  stop_if_cmd Intern (fun () -> print_endline (string_of_intern_ast (env.prelude, prog)));

  let prelude, prog = intern_error_wrapper (fun () -> polarity_inference env prog) in
  stop_if_cmd PolInfer (fun () -> print_endline (string_of_full_ast (prelude, prog)));

  print_endline "Not yet implemented.";
  exit 1
