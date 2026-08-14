import Tests.Support

/-! Direct tests for small public values, runtime state, and default services. -/

def testCauseUtilities : IO Unit := do
  let mappedFailure : Cause Nat := Cause.map String.length (.fail "error")
  assertTrue "Cause.map did not map a typed failure"
    (mappedFailure == .fail 5)

  let defect := IO.userError "defect"
  let mappedDefect : Cause Nat :=
    Cause.map String.length (.die defect : Cause String)
  assertTrue "Cause.map changed a defect" (mappedDefect == .die defect)

  let mappedInterrupt : Cause Nat :=
    Cause.map String.length (.interrupt : Cause String)
  assertTrue "Cause.map changed interruption"
    (mappedInterrupt == .interrupt)

  assertTrue "Cause.failureOption lost a typed failure"
    ((.fail "error" : Cause String).failureOption == some "error")
  assertTrue "Cause.failureOption returned a defect"
    ((.die defect : Cause String).failureOption == none)
  assertTrue "Cause.failureOption returned an interruption"
    ((.interrupt : Cause String).failureOption == none)

  let failureOrCause : String ⊕ Cause Nat :=
    (.fail "error" : Cause String).failureOrCause
  assertTrue "Cause.failureOrCause did not return the typed failure"
    (failureOrCause == .inl "error")
  let interruptOrCause : String ⊕ Cause Nat :=
    (.interrupt : Cause String).failureOrCause
  assertTrue "Cause.failureOrCause changed interruption"
    (interruptOrCause == .inr .interrupt)
  assertTrue "Cause.show changed its stable interruption text"
    (toString (.interrupt : Cause String) == "Cause.interrupt")

  let sequential : Cause String :=
    .sequential (.fail "first") (.die defect)
  let mappedSequential : Cause Nat := sequential.map String.length
  assertTrue "Cause.map did not preserve sequential composition"
    (mappedSequential == .sequential (.fail 5) (.die defect))
  assertTrue "Cause.failureOption did not find the first sequential failure"
    (sequential.failureOption == some "first")
  let sequentialFailureOrCause : String ⊕ Cause Nat :=
    sequential.failureOrCause
  assertTrue "Cause.failureOrCause did not find a composed typed failure"
    (sequentialFailureOrCause == .inl "first")
  assertTrue "Cause.show did not show sequential structure"
    (toString sequential ==
      "Cause.sequential (Cause.fail (first), Cause.die (defect))")

  let parallel : Cause String :=
    .parallel (.die defect) .interrupt
  let mappedParallel : Cause Nat := parallel.map String.length
  assertTrue "Cause.map did not preserve parallel composition"
    (mappedParallel == .parallel (.die defect) .interrupt)
  assertTrue "Cause.failureOption returned a failure from a defect-only cause"
    (parallel.failureOption == none)
  let parallelFailureOrCause : String ⊕ Cause Nat :=
    parallel.failureOrCause
  assertTrue "Cause.failureOrCause did not preserve defect-only structure"
    (parallelFailureOrCause ==
      .inr (.parallel (.die defect) .interrupt))
  assertTrue "Cause.show did not show parallel structure"
    (toString parallel ==
      "Cause.parallel (Cause.die (defect), Cause.interrupt)")

def testExitUtilities : IO Unit := do
  let success : Exit String Nat := .success 42
  let failure : Exit String Nat := .failure (.fail "bad")
  assertTrue "Exit.show changed its success text"
    (toString success == "Exit.success (...)")
  assertTrue "Exit.show changed its failure text"
    (toString failure == "Exit.failure (Cause.fail (bad))")
  assertTrue "different successful exits compared as equal"
    (success != .success 7)
  assertTrue "different exit variants compared as equal"
    (success != failure)

def testInterruptStatusUtilities : IO Unit := do
  assertTrue "interruptible status did not convert to true"
    InterruptStatus.interruptible.toBool
  assertTrue "uninterruptible status did not convert to false"
    !InterruptStatus.uninterruptible.toBool
  assertTrue "interruptible status text changed"
    (toString InterruptStatus.interruptible == "interruptible")
  assertTrue "uninterruptible status text changed"
    (toString InterruptStatus.uninterruptible == "uninterruptible")

private def makeInterruption : IO Interruption := do
  pure {
    interrupted := ← IO.mkRef false
    isInterruptible := ← IO.mkRef true
    isInterrupting := false
    interruptDelivered := ← IO.mkRef false
    interruptHandler := ← IO.mkRef IO.unit
  }

def testInterruptionState : IO Unit := do
  let interruption ← makeInterruption
  assertTrue "a new interruption requested an interrupt"
    !(← interruption.shouldInterrupt)

  interruption.interrupted.set true
  assertTrue "an interruptible request was ignored"
    (← interruption.shouldInterrupt)

  interruption.isInterruptible.set false
  assertTrue "an uninterruptible request was delivered"
    !(← interruption.shouldInterrupt)

  interruption.isInterruptible.set true
  let unwinding ← interruption.beginUnwind
  assertTrue "beginUnwind did not mark interruption as delivered"
    (← interruption.interruptDelivered.get)
  assertTrue "beginUnwind did not suppress a second interruption"
    !(← unwinding.shouldInterrupt)

  let recovered := unwinding.endUnwind
  assertTrue "endUnwind kept the runtime in unwind mode"
    !recovered.isInterrupting
  assertTrue "a delivered request was delivered twice"
    !(← recovered.shouldInterrupt)

  recovered.interruptDelivered.set false
  assertTrue "a later request was not deliverable after recovery"
    (← recovered.shouldInterrupt)

def testFiberStateUtilities : IO Unit := do
  let fiber : Fiber Empty Nat ← Fiber.empty "state-utilities"
  assertTrue "a created fiber was not considered active"
    (← (FiberState.created : FiberState Empty Nat).isRunning)
  assertTrue "a created fiber did not describe its state"
    ((← fiber.showState).contains "state: .created")

  let gate ← IO.Promise.new (α := Unit)
  let task ← IO.asTask do
    let _ ← IO.wait gate.result?
    pure ()
  fiber.setTask task
  let runningState ← fiber.state.get
  assertTrue "a pending fiber task was not considered active"
    (← runningState.isRunning)
  assertTrue "a running fiber did not describe its state"
    ((← fiber.showState).contains "state: .running")

  fiber.complete (.success 42)
  let doneState ← fiber.state.get
  assertTrue "a completed fiber was still considered active"
    !(← doneState.isRunning)
  assertTrue "a completed fiber did not describe its state"
    ((← fiber.showState).contains "state: .done")
  match ← fiber.await with
  | .success 42 => pure ()
  | _ => failTest "Fiber.await did not return the completed value"
  gate.resolve ()
  fiber.awaitTask

def testFiberInterruptionBridge : IO Unit := do
  let fiber : Fiber Empty Nat ← Fiber.empty "interruption-bridge"
  let handlerCalled ← IO.mkRef false
  fiber.interruptHandler.set (handlerCalled.set true)
  let info := fiber.toFiberInfo
  assertTrue "FiberInfo changed the fiber ID"
    (toString info.fiberId == "interruption-bridge")
  info.interrupt
  assertTrue "FiberInfo.interrupt did not request interruption"
    (← fiber.interrupted.get)
  assertTrue "FiberInfo.interrupt did not call the active handler"
    (← handlerCalled.get)

  let interruption ← fiber.toInterruption
  assertTrue "Fiber.toInterruption did not share the request state"
    (← interruption.shouldInterrupt)
  fiber.complete (.success 7)
  info.await

def testConsoleAccessors : IO Unit := do
  let output ← IO.mkRef ([] : List String)
  let console : Console := {
    printLine := fun line =>
      Z.succeed <| output.modify (fun lines => lines ++ [toString line])
    readLine := Z.succeedNow "typed-input"
  }
  let program : Z Console IO.Error String := do
    Console.printLineZ (42 : Nat)
    Console.readLineZ
  match ← runProgram "console-accessors"
      (program.provideEnvironment console) with
  | .success "typed-input" => pure ()
  | _ => failTest "Console accessors did not use the provided service"
  assertTrue "Console.printLineZ changed or dropped its value"
    ((← output.get) == ["42"])

  let failedConsole : Console := {
    printLine := fun _ => pure ()
    readLine :=
      (Z.fail (IO.userError "read failure") : Z Unit IO.Error Empty)
        |>.map impossible
  }
  match ← runProgram "console-read-failure"
      (Console.readLineZ.provideEnvironment failedConsole) with
  | .failure (.fail _) => pure ()
  | _ => failTest "Console.readLineZ lost the typed read failure"

def testRandomBoundariesAndAccessor : IO Unit := do
  let call ← IO.mkRef (none : Option (Nat × Nat))
  let random : Random := {
    nextNat := fun lo hi => do
      let _ ← Z.succeed (call.set (some (lo, hi)))
      pure 17
  }
  match ← runProgram "random-accessor"
      ((Random.nextNatZ 3 9).provideEnvironment random) with
  | .success 17 => pure ()
  | _ => failTest "Random.nextNatZ did not return the service result"
  assertTrue "Random.nextNatZ changed the requested range"
    ((← call.get) == some (3, 9))

  match ← runProgram "random-single-value" (Random.randomLive.nextNat 4 4) with
  | .success 4 => pure ()
  | _ => failTest "Random.randomLive did not support a one-value range"

  for _ in [0:128] do
    match ← runProgram "random-bounds" (Random.randomLive.nextNat 10 5) with
    | .success value =>
        assertTrue s!"Random.randomLive returned {value} outside [5, 10]"
          (5 ≤ value && value ≤ 10)
    | _ => failTest "Random.randomLive failed while checking its bounds"

def primitiveTests : List (String × IO Unit) := [
  ("testCauseUtilities", testCauseUtilities),
  ("testExitUtilities", testExitUtilities),
  ("testInterruptStatusUtilities", testInterruptStatusUtilities),
  ("testInterruptionState", testInterruptionState),
  ("testFiberStateUtilities", testFiberStateUtilities),
  ("testFiberInterruptionBridge", testFiberInterruptionBridge),
  ("testConsoleAccessors", testConsoleAccessors),
  ("testRandomBoundariesAndAccessor", testRandomBoundariesAndAccessor)
]
