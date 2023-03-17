open Autobill
open Cst_intf
open Intern_intf
open Sort_intf
open Reduction_intf
open TypeInfer_intf

let version = "v0.0.6-alpha"

let usage_spiel =
  {|usage: autobill [options] [input_file]|}

type subcommand =
  | Version
  | Parse
  | Print_Machine
  | Parse_Machine
  | Intern
  | SortInfer
  | Constraint
  | TypeInfer
  | PostConstraint
  | AaraGen
  | Simplify

type input_lang =
  | Lcbpv
  | Autobill

let do_trace = ref false

let do_simplify = ref true

let in_ch = ref stdin

let in_name = ref "<stdin>"

let out_ch = ref stdout

let subcommand = ref Simplify

let in_lang = ref Lcbpv

let set_input_file name =
  in_name := name;
  in_ch := open_in name

let set_output_file name =
  out_ch := open_out name

let set ?step ?lang () =
  Option.iter (fun x -> subcommand := x) step;
  Option.iter (fun x -> in_lang := x) lang

let set_input_lang input = in_lang := input

let parse_cli_invocation () =
  let open Arg in
  let speclist = [
    ("-v", Unit (set ~step: Version), "Print version and exit");
    ("-p", Unit (set ~step: Parse), "Parse a LCBPV program");
    ("-m", Unit (set ~step: Print_Machine), "Parse and desugar a LCBPV program into machine");
    ("-l", Unit (set ~lang:Autobill), "Parse a LCBPV program (default)");
    ("-M", Unit (set ~lang: Autobill), "Parse a machine program");
    ("-i", Unit (set ~step: Intern), "Parse and internalize");
    ("-s", Unit (set ~step: SortInfer), "Infer sorts");
    (* ("-l", Unit (set ~step: Multiplicities), "All of the above, and infer multiplicities"); *)
    ("-c", Unit (set ~step: Constraint), "Generate a type contraint");
    ("-t", Unit (set ~step: TypeInfer), "Typecheck");
    ("-C", Unit (set ~step: PostConstraint), "Print the index constraint of a typechecked program");
    ("-a", Unit (set ~step: AaraGen), "Print the AARA constraint");
    ("-r", Unit (set ~step: Simplify), "Simplify a typechecked program");
    ("-o", String set_output_file, "Set output file");
    ("-V", Set do_trace, "Trace the sort and type inference");
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

  let cst = match !in_lang with
    | Lcbpv ->
      let cst = Lcbpv_intf.parse_cst !in_name !in_ch in
      stop_if_cmd Parse (fun () -> Lcbpv_intf.string_of_cst cst);
      let cst = Lcbpv_intf.convert_to_machine_code cst in
      stop_if_cmd Print_Machine (fun () -> string_of_cst cst);
      cst
    | Autobill -> parse_cst !in_name !in_ch in

  stop_if_cmd Parse (fun () -> string_of_cst cst);

  let prog, env = internalize cst in
  stop_if_cmd Intern (fun () -> string_of_intern_ast (env.prelude, prog));

  let prog = polarity_inference ~trace:!do_trace env prog in
  stop_if_cmd SortInfer (fun () -> string_of_full_ast prog);

  stop_if_cmd Constraint (fun () ->  constraint_as_string prog);

  let prelude, prog, post_con = type_infer ~trace:!do_trace prog in
  stop_if_cmd TypeInfer (fun () -> string_of_full_ast (prelude, prog));

  match !subcommand with
  | PostConstraint ->
    output_string !out_ch (post_contraint_as_string (prelude, prog, post_con))
  | AaraGen ->
    output_string !out_ch (aara_constraint_as_string (prelude, prog, post_con))
  | Simplify ->
    let prog = simplify_untyped_prog (prelude, prog) in
    output_string !out_ch (string_of_full_ast prog)
  | _ ->
    print_endline "Not yet implemented.";
    exit 1
