import Examples

def dotFile (name: String) := s!"diagrams/{name}.dot"

def runExample (name : String) (program : Z Unit E A) : IO Unit := do
  match <- Z.unsafeRunSync program name (some (dotFile name)) with
  | .success _ => pure ()
  | .failure _ =>
      throw (IO.userError s!"Example '{name}' failed.")

def main : IO Unit := do
  runExample "succeedNowExample" succeedNowExample
  runExample "zipExample" zipExample
  runExample "zipExample2" zipExample2
  runExample "mapExample" mapExample
  runExample "monadExample" monadExample
  runExample "succeedExample" succeedExample
  runExample "attemptExample" attemptExample
  runExample "coercionExample" coercionExample
  runExample "asyncExample" asyncExample
  runExample "forkExample" forkExample
  runExample "stackSafetyExample" stackSafetyExample
  runExample "ensuringExample" ensuringExample
  runExample "uninterruptibleExample" uninterruptibleExample
  runExample "interruptionExample1" interruptionExample1
  runExample "interruptionExample2" interruptionExample2
  runExample "interruptionExample3" interruptionExample3
  runExample "uninterruptibleExample1" uninterruptibleExample1
  runExample "uninterruptibleExample2" uninterruptibleExample2
  runExample "envExample1ready" envExample1ready
  runExample "envExample2ready" envExample2ready
  runExample "errorHandling1a" errorHandling1a
  runExample "errorHandling1b" errorHandling1b
  runExample "errorHandling1c" errorHandling1c
  runExample "errorHandling2a" errorHandling2a
  runExample "defectRecovery" defectRecovery
  runExample "onionArchitecture" OnionArchitecture.runnableDemo
  println! "---- exiting main ----"

-- Run these examples and write their execution diagrams with `lake exe z`.
