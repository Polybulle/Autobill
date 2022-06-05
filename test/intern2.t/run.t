Test the prelude internalizer
  $ autobill polinfer test_prelude.bill
  polarity inference not yet implemented.

Test the program internalizer on name shadowing:
  $ autobill polinfer test_prog.bill
  polarity inference not yet implemented.

Finally, test a roundtrip of the whole thing:
  $ cat test_prelude.bill test_prog.bill | autobill polinfer | autobill parse
  Fatal error: exception Failure("1:8: syntax error")
  [2]
