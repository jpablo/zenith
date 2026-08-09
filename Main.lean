import Examples

def dotFile (name: String) := s!"diagrams/{name}.dot"

def runExample (name : String) (program : Z Unit E A) : IO Unit := do
  let _ <- Z.unsafeRunSync program (dotFile name)
  pure ()

def main: IO Unit := do
  runExample "succeedNowExample" succeedNowExample
  runExample "zipExample" zipExample
  runExample "zipExample2" zipExample2
  runExample "mapExample" mapExample
  runExample "monadExample" monadExample
  runExample "succeedExample" succeedExample
  runExample "attemptExample" attemptExample
  runExample "coercionExample" coercionExample
  runExample (E := Empty) "asyncExample" asyncExample
  runExample "forkExample" forkExample
  runExample "stackOverflow" stackOverflow
  runExample "flatMapEx" flatMapEx
  runExample "ensuringExample" ensuringExample
  runExample "uninterruptibleExample" uninterruptibleExample
  runExample "interruptionExample1" interruptionExample1
  runExample "interruptionExample2" interruptionExample2
  runExample "interruptionExample2b" interruptionExample2b
  runExample "interruptionExample3" interruptionExample3
  runExample "uninterruptibleExample1" uninterruptibleExample1
  runExample "uninterruptibleExample2" uninterruptibleExample2
  runExample "envExample1ready" envExample1ready
  runExample "envExample2ready" envExample2ready
  runExample "envExample3" envExample3
  runExample "errorHandling1a" errorHandling1a
  runExample "errorHandling1b" errorHandling1b
  runExample "errorHandling1c" errorHandling1c
  runExample "errorHandling2a" errorHandling2a
  println! "---- exiting main ----"
    


-- find . -name "*.lean" | entr -s 'lake build'
-- ./build/bin/z
-- for f in $(find diagrams -name "*.dot"); do echo $f; dot -Tsvg $f -o diagrams/$(basename $f .dot).svg; done
