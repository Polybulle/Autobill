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
  | Machine
  | Intern
  | SortInfer
  | Constraint
  | Multiplicities
  | TypeInfer
  | Interpret
  | PostConstraint

let do_trace = ref false

let do_simplify = ref false

let in_ch = ref stdin

let in_name = ref "<stdin>"

let out_ch = ref stdout

let subcommand = ref Interpret

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
    ("-m", Unit (process Machine), "Parse, and convert to machine code");
    ("-i", Unit (process Intern), "Parse, and internalize machine code");
    ("-s", Unit (process SortInfer), "All of the above, and infer sorts");
    (* ("-l", Unit (process Multiplicities), "All of the above, and infer multiplicities"); *)
    ("-c", Unit (process Constraint), "All of the above, and generate a type contraint");
    ("-t", Unit (process TypeInfer), "All of the above, and typecheck");
    ("-C", Unit (process PostConstraint), "All of the above, and print the index constraint");
    ("-o", String set_output_file, "Set output file");
    ("-V", Set do_trace, "Trace the sort and type inference");
    ("-r", Clear do_simplify, "Do not simplify source file before type inference");
  ] in
  Arg.parse speclist set_input_file usage_spiel

let stop_if_cmd comm' final =
  if !subcommand = comm' then begin
    output_string !out_ch (final ());
    exit 0
  end

let string_of_full_ast ?debug:(debug=false) prog =
  PrettyPrinter.PP.pp_program ~debug Format.str_formatter prog;
  Format.flush_str_formatter ()


let () =

 let _ = parse_cli_invocation () in

  stop_if_cmd Version (fun () -> version);

  let cst = Lcbpv_intf.parse_cst !in_name !in_ch in
  stop_if_cmd Parse (fun () -> Lcbpv_intf.string_of_cst cst);

  let cst = Lcbpv_intf.convert_to_machine_code cst in
  stop_if_cmd Machine (fun () -> string_of_cst cst);

  let prog, env = (* intern_error_wrapper ( fun () ->*) internalize cst in
  stop_if_cmd Intern (fun () -> string_of_intern_ast (env.prelude, prog));

  let prog = (* intern_error_wrapper (fun () ->  *)polarity_inference ~trace:!do_trace env prog in
  (* let prog = if !do_simplify then simplify_untyped_prog prog else prog in *)
  stop_if_cmd SortInfer (fun () -> string_of_full_ast prog);

  (* let prog = infer_multiplicities prog in *)
  (* stop_if_cmd Multiplicities (fun () -> string_of_full_ast ~debug:true prog); *)

  stop_if_cmd Constraint (fun () ->  constraint_as_string prog);

  let prelude, prog, post_con = type_infer ~trace:!do_trace prog in
  stop_if_cmd TypeInfer (fun () -> string_of_full_ast (prelude, prog));

  stop_if_cmd PostConstraint (fun () -> post_contraint_as_string (prelude, prog, post_con));

  let prog = interpret_prog (prelude, prog) in
  stop_if_cmd Interpret (fun () -> string_of_full_ast prog);

  print_endline "Not yet implemented.";
  exit 1
