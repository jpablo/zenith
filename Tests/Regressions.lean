import Tests.Support
import Z.Random
import Std.Data.HashSet

/-!
Regression tests for the defects found by the module `Z` review.

Every test in this file reproduces a confirmed defect. Each one is expected to
fail until the corresponding fix lands.
-/

/-! ## Interruption -/

/--
Interrupting a fiber that is suspended in an async effect must still run the
finalizers registered by `ensuring`.
-/
def testAsyncInterruptionRunsFinalizer : IO Unit := do
  let registered ← IO.mkRef false
  let finalized ← IO.mkRef false
  let program : Z Unit Empty Unit :=
    (Z.async fun _ => registered.set true).ensuring
      (Z.succeed (finalized.set true))
  let fiber ← Z.unsafeFork program "async-interruption-finalizer"
  waitForFlag "async registration" registered
  fiber.requestInterrupt
  match ← fiberExitWithin fiber with
  | some (.failure .interrupt) => pure ()
  | some exit =>
      failTest s!"interrupting a suspended async returned {exit}"
  | none => failTest "interrupting a suspended async never completed the fiber"
  assertTrue "interrupting a suspended async skipped its ensuring finalizer"
    (← flagBecameTrue finalized)

/--
A fiber whose handler recovers from `Cause.interrupt` must stay interruptible;
a later interrupt request has to stop it.
-/
def testInterruptRecoveryRemainsInterruptible : IO Unit := do
  let recovered ← IO.mkRef false
  -- Both loops are bounded so that a fiber which ignores the second interrupt
  -- still terminates and lets the process exit.
  let firstLoop : Z Unit Empty Unit := Z.repeatN 4000 (Z.sleep 5)
  let secondLoop : Z Unit Empty Unit := Z.repeatN 1200 (Z.sleep 5)
  let program : Z Unit Empty Unit := do
    let _ ← firstLoop.exit
    let _ ← Z.succeed (recovered.set true)
    secondLoop
  let fiber ← Z.unsafeFork program "interrupt-recovery"
  IO.sleep 25
  fiber.requestInterrupt
  waitForFlag "recovery after the first interruption" recovered
  fiber.requestInterrupt
  match ← fiberExitWithin fiber 1500 with
  | some _ => pure ()
  | none =>
      failTest
        "a fiber that recovered from an interruption ignored a later interrupt"

/--
A fiber blocked inside its async registration must still observe an interrupt.
-/
def testBlockingRegistrationIsInterruptible : IO Unit := do
  let registered ← IO.mkRef false
  let release ← IO.Promise.new (α := Unit)
  let program : Z Unit Empty Unit := Z.async fun _ => do
    registered.set true
    let _ ← IO.wait release.result?
    pure ()
  let fiber ← Z.unsafeFork program "blocking-registration"
  waitForFlag "blocking async registration" registered
  fiber.requestInterrupt
  let exit ← fiberExitWithin fiber
  -- Let the registration thread finish regardless of the outcome.
  release.resolve ()
  match exit with
  | some (.failure .interrupt) => pure ()
  | some exit =>
      failTest s!"interrupting a blocked registration returned {exit}"
  | none =>
      failTest
        "a fiber blocked inside its async registration ignored an interrupt"

/-- A cancellable async effect must run its cancellation action exactly once. -/
def testAsyncInterruptCancelerRunsOnce : IO Unit := do
  let registering ← IO.mkRef false
  let cancels ← IO.mkRef 0
  let pending : Z Unit Empty Unit := Z.asyncInterrupt fun _ => do
    registering.set true
    IO.sleep 50
    pure (cancels.modify (· + 1))
  let fiber ← Z.unsafeFork pending "async-interrupt-once"
  waitForFlag "cancellable async registration" registering
  fiber.requestInterrupt
  match ← fiberExitWithin fiber with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"a cancelled async returned {exit}"
  | none => failTest "a cancelled async never completed the fiber"
  IO.sleep 100
  let count ← cancels.get
  assertTrue s!"the async cancellation action ran {count} times instead of once"
    (count == 1)

/-! ## Defect handling -/

private def failingCurrentNodeDiagram : ExecutionDiagram (IO Unit) :=
  { ExecutionDiagram.empty with
    enabled := true
    currentNode := fun label _ _ _ _ _ _ _ =>
      if label == "defect-here" then
        throw (IO.userError "diagram write failed")
      else
        pure () }

/--
An `IO.Error` raised by the interpreter itself must still unwind through the
finalizers that are pending on the execution stack.
-/
def testInterpreterDefectRunsFinalizer : IO Unit := do
  let finalized ← IO.mkRef false
  let body : ZCore Unit Empty Unit :=
    (ZCore.succeed' (pure ())).withLabel "defect-here"
  let finalizer : ZCore Unit Empty Unit :=
    ZCore.succeed' (finalized.set true)
  let program : ZCore Unit Empty Unit := body.ensuring finalizer
  let fiber ← ZCore.unsafeRunFiber failingCurrentNodeDiagram
    program Environment.empty "" "interpreter-defect-finalizer" 0
  match ← fiberExitWithin fiber with
  | some (.failure (.die _)) => pure ()
  | some exit =>
      failTest s!"an interpreter defect returned {exit}"
  | none => failTest "an interpreter defect never completed the fiber"
  assertTrue "an interpreter defect skipped a pending ensuring finalizer"
    (← flagBecameTrue finalized)

/--
`HEIO.asyncInterrupt` must not deadlock when the registration invokes its
callback synchronously while interruption is already requested.
-/
def testHEIOSynchronousCallbackDoesNotDeadlock : IO Unit := do
  let interruption ← HEIO.Interruption.new
  let pending : HEIO (Cause IO.Error) (ULift.{1} Nat) :=
    HEIO.asyncInterrupt Cause.die fun callback => do
      -- Request interruption without running the handlers, which is the state
      -- `Interruption.requestBase` produces before its handler task starts.
      interruption.interrupted.set true
      callback (.ok 7)
      pure IO.unit
  let worker ← IO.asTask (prio := Task.Priority.dedicated) <|
    HEIO.toIOResultInterruptible
      interruption (Cause.interrupt : Cause IO.Error) pending
  match ← taskResultWithin worker 3000 with
  | some _ => pure ()
  | none => do
      -- The deadlocked worker also blocks process exit, so report the failure
      -- before throwing: otherwise nothing is ever printed.
      IO.eprintln
        "testHEIOSynchronousCallbackDoesNotDeadlock: a synchronous \
         asyncInterrupt callback deadlocked under interruption"
      failTest
        "a synchronous asyncInterrupt callback deadlocked under interruption"

/-! ## Error channels -/

/--
An `IO` action lifted into a typed `IO.Error` channel must be catchable there;
otherwise the coercion has to be rejected.
-/
def testCoercedIOFailureIsCatchable : IO Unit := do
  let failing : IO Nat := throw (IO.userError "boom")
  let coerced : Z Unit IO.Error Nat := failing
  let program : Z Unit Empty Nat := coerced.catchAll fun _ => Z.succeedNow 0
  match ← runProgram "coerced-io-failure" program with
  | .success 0 => pure ()
  | exit =>
      failTest s!"a coerced IO failure bypassed catchAll and returned {exit}"

private def dieBody : Z Unit Empty Nat :=
  (Z.die (IO.userError "boom")).map impossible

/-- Annotating a `zdo` block must not change which errors its `catch` sees. -/
def testZDoAnnotatedCatchMatchesInferred : IO Unit := do
  let inferred := zdo
    try
      let _ : Nat ← dieBody
      pure 0
    catch _ =>
      Z.succeedNow 7
  let inferred : Z Unit Empty Nat := inferred
  match ← runProgram "zdo-inferred-defect-catch" inferred with
  | .success 7 => pure ()
  | exit => failTest s!"the inferred zdo catch returned {exit}"

  let annotated : Z Unit IO.Error Nat := zdo
    try
      let _ : Nat ← dieBody
      pure 0
    catch _ =>
      Z.succeedNow 7
  match ← runProgram "zdo-annotated-defect-catch" annotated with
  | .success 7 => pure ()
  | exit =>
      failTest s!"an annotated zdo catch stopped catching defects: {exit}"

/-- `zdo[E]` must not change which errors a `catch` sees either. -/
def testZDoExplicitErrorCatchMatchesInferred : IO Unit := do
  let explicit := zdo[IO.Error]
    try
      let _ : Nat ← dieBody
      pure 0
    catch _ =>
      Z.succeedNow 7
  let explicit : Z Unit IO.Error Nat := explicit
  match ← runProgram "zdo-explicit-error-defect-catch" explicit with
  | .success 7 => pure ()
  | exit =>
      failTest s!"a zdo[E] catch stopped catching defects: {exit}"

/-! ## Layers and environments -/

/-- A shared layer must be built from the input its composition feeds it. -/
def testMemoizedLayerUsesBuildInput : IO Unit := do
  let source : Layer Nat Empty String :=
    Layer.fromFunction fun value => s!"source-{value}"
  let upstream : Layer Nat Empty Nat :=
    Layer.fromFunction fun value => value + 100
  let program : Z String Empty String := Z.serviceWith id

  match ← (upstream.to source).run 1 program "memoize-direct" with
  | .success "source-101" => pure ()
  | .success other => failTest s!"the direct composition produced {other}"
  | .failure cause => failTest s!"the direct composition failed: {cause}"

  let shared := source.share fun sharedSource => upstream.to sharedSource
  match ← shared.run 1 program "memoize-shared" with
  | .success "source-101" => pure ()
  | .success other =>
      failTest s!"a shared layer ignored its build input and produced {other}"
  | .failure cause => failTest s!"the shared composition failed: {cause}"

/--
Two distinct services of the same type must not collapse into one when the
environment is projected as a pair.
-/
def testEnvironmentSelectsDistinctServices : IO Unit := do
  let environment : Environment (Char × String × String × Nat) :=
    ('c', "first", "second", 1)
  let pair : String × String := Environment.get environment (String × String)
  assertTrue
    s!"the environment produced {pair}, duplicating one service"
    (pair.1 != pair.2)

/-! ## Services -/

/-- Concurrent draws from the `Random` service must not collide. -/
def testRandomServiceIsConcurrencySafe : IO Unit := do
  let workerCount := 8
  let drawsPerWorker := 64
  let range := 1000000000000
  let tasks ← (List.range workerCount).mapM fun index =>
    IO.asTask (prio := Task.Priority.dedicated) do
      let values ← IO.mkRef ([] : List Nat)
      for _ in [0:drawsPerWorker] do
        match ← Z.unsafeRunSync
            (Random.randomLive.nextNat 0 range) s!"random-{index}" with
        | .success value => values.modify (value :: ·)
        | _ => pure ()
      values.get
  let mut draws : List Nat := []
  for task in tasks do
    match ← taskResultWithin task 20000 with
    | some (.ok values) => draws := draws ++ values
    | _ => failTest "a concurrent random worker did not finish"
  let unique := draws.foldl (init := ({} : Std.HashSet Nat)) fun set value =>
    set.insert value
  assertTrue
    s!"concurrent random draws repeated {draws.length - unique.size} values"
    (unique.size == draws.length)

def regressionTests : List (String × IO Unit) := [
  ("testAsyncInterruptionRunsFinalizer", testAsyncInterruptionRunsFinalizer),
  ("testInterruptRecoveryRemainsInterruptible",
    testInterruptRecoveryRemainsInterruptible),
  ("testBlockingRegistrationIsInterruptible",
    testBlockingRegistrationIsInterruptible),
  ("testAsyncInterruptCancelerRunsOnce", testAsyncInterruptCancelerRunsOnce),
  ("testInterpreterDefectRunsFinalizer", testInterpreterDefectRunsFinalizer),
  ("testHEIOSynchronousCallbackDoesNotDeadlock",
    testHEIOSynchronousCallbackDoesNotDeadlock),
  ("testCoercedIOFailureIsCatchable", testCoercedIOFailureIsCatchable),
  ("testZDoAnnotatedCatchMatchesInferred",
    testZDoAnnotatedCatchMatchesInferred),
  ("testZDoExplicitErrorCatchMatchesInferred",
    testZDoExplicitErrorCatchMatchesInferred),
  ("testMemoizedLayerUsesBuildInput", testMemoizedLayerUsesBuildInput),
  ("testEnvironmentSelectsDistinctServices",
    testEnvironmentSelectsDistinctServices),
  ("testRandomServiceIsConcurrencySafe", testRandomServiceIsConcurrencySafe)
]
