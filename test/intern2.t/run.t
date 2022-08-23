Test the prelude internalizer
  $ autobill polinfer test_prelude.bill
  usage: autobill <subcommand> <input_file>
        allowed subcommands: parse, intern, sort, simplify, infer, version
        if input_file is omitted, input is read from stdin
  [1]

Test the program internalizer on name shadowing:
  $ autobill polinfer test_prog.bill
  usage: autobill <subcommand> <input_file>
        allowed subcommands: parse, intern, sort, simplify, infer, version
        if input_file is omitted, input is read from stdin
  [1]

Test both programs:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer
  usage: autobill <subcommand> <input_file>
        allowed subcommands: parse, intern, sort, simplify, infer, version
        if input_file is omitted, input is read from stdin
  [1]


Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer | autobill parse
  Fatal error: exception Failure("1:5: syntax error")
  [2]
