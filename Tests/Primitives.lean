import Tests.Support
import Zenith.Formalization.CoreLaws

/-!
Direct runtime tests for public values, runtime state, and default services.

Pure laws for `Cause`, `Exit`, `InterruptStatus`, and ordinary `Environment`
projection are proved in `Zenith.Formalization.CoreLaws`.
-/

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
      Z.fromIO <| output.modify (fun lines => lines ++ [toString line])
    readLine := Z.succeed "typed-input"
  }
  let program : Z Console IO.Error String := do
    Console.printLineM (42 : Nat)
    Console.readLineM
  match ← runProgram "console-accessors"
      (program.provideEnvironment console) with
  | .success "typed-input" => pure ()
  | _ => failTest "Console accessors did not use the provided service"
  assertTrue "Console.printLineM changed or dropped its value"
    ((← output.get) == ["42"])

  let failedConsole : Console := {
    printLine := fun _ => pure ()
    readLine :=
      (Z.fail (IO.userError "read failure") : Z Unit IO.Error Empty)
        |>.map impossible
  }
  match ← runProgram "console-read-failure"
      (Console.readLineM.provideEnvironment failedConsole) with
  | .failure (.fail _) => pure ()
  | _ => failTest "Console.readLineM lost the typed read failure"

def testRandomBoundariesAndAccessor : IO Unit := do
  let call ← IO.mkRef (none : Option (Nat × Nat))
  let random : Random := {
    nextNat := fun lo hi => do
      let _ ← Z.fromIO (call.set (some (lo, hi)))
      pure 17
  }
  match ← runProgram "random-accessor"
      ((Random.nextNatM 3 9).provideEnvironment random) with
  | .success 17 => pure ()
  | _ => failTest "Random.nextNatM did not return the service result"
  assertTrue "Random.nextNatM changed the requested range"
    ((← call.get) == some (3, 9))

  match ← runProgram "random-single-value" (Random.live.nextNat 4 4) with
  | .success 4 => pure ()
  | _ => failTest "Random.live did not support a one-value range"

  for _ in [0:128] do
    match ← runProgram "random-bounds" (Random.live.nextNat 10 5) with
    | .success value =>
        assertTrue s!"Random.live returned {value} outside [5, 10]"
          (5 ≤ value && value ≤ 10)
    | _ => failTest "Random.live failed while checking its bounds"

/-- Adapted from ZIO `ZIOLazinessSpec`: lifted `IO` runs only at execution. -/
def testFromIOIsLazyAndRepeatable : IO Unit := do
  let runs ← IO.mkRef 0
  let effect : Z Unit Empty Nat :=
    Z.fromIO <| runs.modifyGet fun current =>
      let next := current + 1
      (next, next)
  assertTrue "Z.fromIO ran while constructing an effect" ((← runs.get) == 0)
  match ← runProgram "from-io-lazy-first" effect with
  | .success 1 => pure ()
  | exit => failTest s!"the first Z.fromIO run returned {exit}"
  match ← runProgram "from-io-lazy-second" effect with
  | .success 2 => pure ()
  | exit => failTest s!"the second Z.fromIO run returned {exit}"

/-- Adapted from ZIO `ZIOLazinessSpec`: `attempt` also defers its raw `IO`. -/
def testAttemptIsLazyAndRepeatable : IO Unit := do
  let runs ← IO.mkRef 0
  let effect : Z Unit IO.Error Nat :=
    Z.attempt <| runs.modifyGet fun current =>
      let next := current + 1
      (next, next)
  assertTrue "Z.attempt ran while constructing an effect" ((← runs.get) == 0)
  match ← runProgram "attempt-lazy-first" effect with
  | .success 1 => pure ()
  | exit => failTest s!"the first Z.attempt run returned {exit}"
  match ← runProgram "attempt-lazy-second" effect with
  | .success 2 => pure ()
  | exit => failTest s!"the second Z.attempt run returned {exit}"

def primitiveTests : List (String × IO Unit) := [
  ("testInterruptionState", testInterruptionState),
  ("testFiberStateUtilities", testFiberStateUtilities),
  ("testFiberInterruptionBridge", testFiberInterruptionBridge),
  ("testConsoleAccessors", testConsoleAccessors),
  ("testRandomBoundariesAndAccessor", testRandomBoundariesAndAccessor),
  ("testFromIOIsLazyAndRepeatable", testFromIOIsLazyAndRepeatable),
  ("testAttemptIsLazyAndRepeatable", testAttemptIsLazyAndRepeatable)
]
