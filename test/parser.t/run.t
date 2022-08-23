Test the parser on a BILL program testingthe whole grammar
  $ autobill parse test.bill
  Fatal error: exception Failure("6:71: syntax error")
  [2]

Now test the parser with a roundtrip
  $ autobill parse test.bill | autobill parse
  Fatal error: exception Failure("6:71: syntax error")
  
