import Z

namespace InterpreterBenchmark

structure Case where
  name : String
  operations : Nat
  action : IO Unit

structure Config where
  warmups : Nat := 2
  samples : Nat := 7
  runnerIterations : Nat := 1000
  flatMapSteps : Nat := 20000
  syncSteps : Nat := 10000
  errorSteps : Nat := 5000
  asyncSteps : Nat := 2000
  forkSteps : Nat := 1000

def Config.default : Config := {}

def Config.quick : Config where
  warmups := 1
  samples := 3
  runnerIterations := 25
  flatMapSteps := 1000
  syncSteps := 500
  errorSteps := 250
  asyncSteps := 100
  forkSteps := 50

private partial def flatMapProgram : Nat -> Nat -> Z Unit Empty Nat
  | 0, total => Z.succeed total
  | steps + 1, total =>
      (Z.succeed total).flatMap fun value =>
        flatMapProgram steps (value + 1)

private partial def ioBindProgram : Nat -> Nat -> IO Nat
  | 0, total => pure total
  | steps + 1, total => do
      let value <- pure total
      ioBindProgram steps (value + 1)

@[noinline] private def incrementRef (value : IO.Ref Nat) : IO Unit :=
  value.modify (· + 1)

private partial def syncProgram : Nat -> Nat -> Z Unit Empty Nat
  | 0, total => Z.succeed total
  | steps + 1, total =>
      (Z.fromIO (pure total)).flatMap fun value =>
        syncProgram steps (value + 1)

private partial def errorProgram : Nat -> Nat -> Z Unit Empty Nat
  | 0, total => Z.succeed total
  | steps + 1, total =>
      (Z.fail "expected" : Z Unit String Nat).catchAll fun _ =>
        errorProgram steps (total + 1)

private def immediateAsync (value : Nat) : Z Unit Empty Nat :=
  Z.async fun callback => callback (.success value)

private partial def asyncProgram : Nat -> Nat -> Z Unit Empty Nat
  | 0, total => Z.succeed total
  | steps + 1, total =>
      (immediateAsync total).flatMap fun value =>
        asyncProgram steps (value + 1)

private partial def forkProgram : Nat -> Nat -> Z Unit Empty Nat
  | 0, total => Z.succeed total
  | steps + 1, total => do
      let fiber <- (Z.succeed total).fork "benchmark-child"
      let value <- fiber.join
      forkProgram steps (value + 1)

private def runExpected
    (fiberId : FiberId)
    (expected : Nat)
    (program : Z Unit Empty Nat) : IO Unit := do
  match <- Z.unsafeRunSync program fiberId with
  | .success actual =>
      unless actual == expected do
        throw <| IO.userError
          s!"{fiberId}: expected {expected}, got {actual}"
  | .failure cause =>
      throw <| IO.userError s!"{fiberId}: benchmark failed: {cause}"

private def runIOExpected
    (name : String)
    (expected : Nat)
    (action : IO Nat) : IO Unit := do
  let actual <- action
  unless actual == expected do
    throw <| IO.userError s!"{name}: expected {expected}, got {actual}"

private def measure (action : IO Unit) : IO Nat := do
  let before <- IO.monoNanosNow.toIO
  action
  let after <- IO.monoNanosNow.toIO
  pure (after - before)

private def formatNanos (nanos : Nat) : String :=
  if nanos >= 1000000000 then
    let whole := nanos / 1000000000
    let fraction := (nanos % 1000000000) / 1000000
    let padded :=
      if fraction < 10 then s!"00{fraction}"
      else if fraction < 100 then s!"0{fraction}"
      else toString fraction
    s!"{whole}.{padded} s"
  else if nanos >= 1000000 then
    let whole := nanos / 1000000
    let fraction := (nanos % 1000000) / 1000
    let padded :=
      if fraction < 10 then s!"00{fraction}"
      else if fraction < 100 then s!"0{fraction}"
      else toString fraction
    s!"{whole}.{padded} ms"
  else if nanos >= 1000 then
    s!"{nanos / 1000} us"
  else
    s!"{nanos} ns"

private def runCase (config : Config) (benchmark : Case) : IO Unit := do
  for _ in [0:config.warmups] do
    benchmark.action
  let mut durations := []
  for _ in [0:config.samples] do
    durations := (<- measure benchmark.action) :: durations
  let sorted := durations.mergeSort (· <= ·)
  let minimum := sorted.head!
  let median := sorted[sorted.length / 2]!
  let maximum := sorted.getLast!
  let nanosPerOperation :=
    if median < benchmark.operations then
      "<1"
    else
      toString (median / benchmark.operations)
  IO.println s!"{benchmark.name}"
  IO.println s!"  min {formatNanos minimum} | median {formatNanos median} | max {formatNanos maximum}"
  IO.println s!"  {nanosPerOperation} ns/op over {benchmark.operations} operations"

private def cases (config : Config) : IO (List Case) := do
  let ioRef <- IO.mkRef 0
  let flatMap := flatMapProgram config.flatMapSteps 0
  let sync := syncProgram config.syncSteps 0
  let errors := errorProgram config.errorSteps 0
  let async := asyncProgram config.asyncSteps 0
  let forks := forkProgram config.forkSteps 0
  pure [
    {
      name := "baseline/io-bind"
      operations := config.flatMapSteps
      action := runIOExpected "baseline-io-bind" config.flatMapSteps
        (ioBindProgram config.flatMapSteps 0)
    },
    {
      name := "baseline/io-task"
      operations := config.runnerIterations
      action := do
        for index in [0:config.runnerIterations] do
          let task <- IO.asTask (pure index)
          match <- IO.wait task with
          | .ok actual =>
              unless actual == index do
                throw <| IO.userError
                  s!"baseline-io-task: expected {index}, got {actual}"
          | .error error => throw error
    },
    {
      name := "baseline/io-ref"
      operations := config.syncSteps
      action := do
        let before <- ioRef.get
        for _ in [0:config.syncSteps] do
          incrementRef ioRef
        let actual <- ioRef.get
        let expected := before + config.syncSteps
        unless actual == expected do
          throw <| IO.userError
            s!"baseline-io-ref: expected {expected}, got {actual}"
    },
    {
      name := "run/succeed"
      operations := config.runnerIterations
      action := do
        for index in [0:config.runnerIterations] do
          runExpected s!"benchmark-run-{index}" 0 (Z.succeed 0)
    },
    {
      name := "run/flatMap"
      operations := config.flatMapSteps
      action := runExpected "benchmark-flatMap" config.flatMapSteps flatMap
    },
    {
      name := "run/sync"
      operations := config.syncSteps
      action := runExpected "benchmark-sync" config.syncSteps sync
    },
    {
      name := "run/error-recovery"
      operations := config.errorSteps
      action := runExpected "benchmark-error" config.errorSteps errors
    },
    {
      name := "run/immediate-async"
      operations := config.asyncSteps
      action := runExpected "benchmark-async" config.asyncSteps async
    },
    {
      name := "run/fork-join"
      operations := config.forkSteps
      action := runExpected "benchmark-fork" config.forkSteps forks
    }
  ]

def run (config : Config) : IO Unit := do
  RuntimeLog.setEnabled false
  IO.println "Zenith interpreter benchmark"
  IO.println s!"warmups: {config.warmups}, samples: {config.samples}"
  for benchmark in (<- cases config) do
    runCase config benchmark

end InterpreterBenchmark

def main (arguments : List String) : IO UInt32 := do
  let config :=
    if arguments.contains "--quick" then
      InterpreterBenchmark.Config.quick
    else
      InterpreterBenchmark.Config.default
  InterpreterBenchmark.run config
  pure 0
