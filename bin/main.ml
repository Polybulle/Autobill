open Autobill
open Cst_intf
open Intern_intf
open Sort_intf
open Reduction_intf
open TypeInfer_intf
open Lexing
open Misc

let version = "v0.0.8-alpha"

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
  | CoqGen
  | Simplify

type input_lang =
  | Lcbpv
  | Autobill

type error_report_format =
  | Human
  | JSON


let do_trace = ref false

let do_simplify = ref true

let in_ch = ref stdin

let in_name = ref "<stdin>"

let out_ch = ref stdout

let subcommand = ref Simplify

let in_lang = ref Lcbpv

let error_format = ref Human

let set_input_file name =
  in_name := name;
  in_ch := open_in name

let set_output_file name =
  out_ch := open_out name

let set ?step ?lang ?errors () =
  Option.iter (fun x -> subcommand := x) step;
  Option.iter (fun x -> in_lang := x) lang;
  Option.iter (fun x -> error_format := x) errors

let set_input_lang input = in_lang := input

let parse_cli_invocation () =
  let open Arg in
  let speclist = [
    ("-v", Unit (set ~step: Version), "Print version and exit");
    ("-j", Unit (set ~errors: JSON), "Reports errors in JSON format");
    ("-p", Unit (set ~step: Parse), "Parse a LCBPV program");
    ("-m", Unit (set ~step: Print_Machine), "Parse and desugar a LCBPV program into machine");
    ("-l", Unit (set ~lang: Lcbpv), "Parse a LCBPV program (default)");
    ("-M", Unit (set ~lang: Autobill), "Parse a machine program");
    ("-i", Unit (set ~step: Intern), "Parse and internalize");
    ("-s", Unit (set ~step: SortInfer), "Infer sorts");
    ("-c", Unit (set ~step: Constraint), "Generate a type contraint");
    ("-t", Unit (set ~step: TypeInfer), "Typecheck");
    ("-C", Unit (set ~step: PostConstraint), "Print the index constraint of a typechecked program");
    ("-a", Unit (set ~step: AaraGen), "Print the AARA constraint");
    ("-q", Unit (set ~step: CoqGen), "Print the parameter constraint as a coq propositon");
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


let string_of_ast ?debug:(debug=false) prog =
  PrettyPrinter.PP_NoTypes.pp_program ~debug Format.str_formatter prog;
  Format.flush_str_formatter ()

let human_error_reporter e = match e with

  | Fatal_error {phase; info; loc; pos} ->
    let loc = match loc, pos with
      | Some loc, _ -> string_of_position loc
      | None, Some pos -> Printf.sprintf "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol)
      | None, None -> "" in
    let loc = if loc = "" then "" else Printf.sprintf "At position %s,\n" loc in
    Printf.eprintf "\nFATAL ERROR:\nDuring %s,\n%s%s\n" phase loc info;
    exit 1

  | Invariant_break (info, loc) ->
    let loc = match loc with
      | Some loc -> Printf.sprintf "At position %s,\n" (string_of_position loc)
      | None -> "" in
    Printf.eprintf "\nFATAL ERROR: Invariant Break! It's on me, you did nothing \
                    wrong.\n%s%s\n" loc info

  | e -> raise e

let json_of_pos p =
  Printf.sprintf "{\"line\": %d, \"column\": %d}" p.pos_lnum (p.pos_cnum - p.pos_bol)

let json_of_loc loc =
  Printf.sprintf "{\"beginning\": %s, \"end\": %s}"
    (json_of_pos loc.start_pos)
    (json_of_pos loc.end_pos)

let rec json_error_reporter e = match e with

  | Fatal_error {phase; info; loc; pos} ->
    let loc = match loc, pos with
      | Some loc, _ -> json_of_loc loc
      | None, Some pos -> json_of_pos pos
      | None, None -> "false" in
    Printf.eprintf "{\"phase\": \"%s\", \"loc\": %s, \"info\": \"%s\"}" phase loc info

  | Invariant_break (info, loc) ->
    json_error_reporter (Fatal_error {info; loc; phase = "false"; pos = None})

  | e -> raise e


let () =

  try

    parse_cli_invocation ();

    stop_if_cmd Version (fun () -> version);

    let cst = match !in_lang with
      | Lcbpv ->
        let cst = parse_lcbpv_cst !in_name !in_ch in
        stop_if_cmd Parse (fun () -> string_of_lcbpv_cst cst);
        let cst = convert_to_machine_code cst in
        stop_if_cmd Print_Machine (fun () -> string_of_machine_cst cst);
        cst
      | Autobill -> parse_machine_cst !in_name !in_ch in

    stop_if_cmd Parse (fun () -> string_of_machine_cst cst);

    let prog = internalize cst in
    stop_if_cmd Intern (fun () -> string_of_intern_ast prog);

    let prog = polarity_inference prog in
    stop_if_cmd SortInfer (fun () -> string_of_full_ast prog);

    stop_if_cmd Constraint (fun () ->  constraint_as_string prog);

    let prog, post_con = type_infer ~trace:!do_trace prog in

    stop_if_cmd TypeInfer (fun () -> string_of_full_ast prog);

    stop_if_cmd PostConstraint (fun () -> (post_contraint_as_string (prog, post_con)));

    stop_if_cmd Simplify (fun () -> (string_of_ast (simplify_untyped_prog prog)));

    let post_con = AaraCompress.compress_unification post_con in
    stop_if_cmd AaraGen
      (fun () -> match prog.goal with
         | Some goal -> AaraExport.convert_to_minizinc_file goal post_con
         | None ->
           Misc.fatal_error "Generating complexity model" "The program defines no goal to infer");
    stop_if_cmd CoqGen (fun () -> CoqExport.export_as_coq_term post_con);

    fail_invariant_break "Mishandled command"

  with

  | e -> match !error_format with
    | Human -> human_error_reporter e; exit 1
    | JSON -> json_error_reporter e; exit 1
