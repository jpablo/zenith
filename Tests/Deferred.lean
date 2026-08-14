import Z
import Tests.Support

/-!
Regression tests for the interruption-aware, one-shot `Deferred` cell.
-/

private def makeDeferred : IO (Deferred String Nat) := do
  match ← Z.unsafeRunSync Deferred.make "deferred-make" with
  | .success deferred => pure deferred
  | .failure cause =>
      failTest s!"Deferred.make failed: {cause}"

def testDeferredCompletesWaitingFiber : IO Unit := do
  let deferred ← makeDeferred
  let started ← IO.mkRef false
  let startedEffect : Z Unit String Unit :=
    (Z.fromIO (started.set true)).widenError
  let fiber ← Z.unsafeFork (startedEffect *> deferred.await) "deferred-await"
  waitForFlag "deferred awaiter" started
  IO.sleep 5
  match ← Z.unsafeRunSync (deferred.succeed 42) "deferred-succeed" with
  | .success true => pure ()
  | _ => failTest "the first Deferred completion did not win"
  match ← fiber.await with
  | .success 42 => pure ()
  | _ => failTest "Deferred.await did not receive the completion value"

/-- Adapted from ZIO `PromiseSpec`: every pending awaiter receives one result. -/
def testDeferredCompletesAllWaitingFibers : IO Unit := do
  let deferred ← makeDeferred
  let waiterCount := 128
  let entered ← IO.mkRef 0
  let ready ← IO.mkRef false
  let markEntered : Z Unit String Unit :=
    (Z.fromIO do
      let count ← entered.modifyGet fun current =>
        let next := current + 1
        (next, next)
      if count == waiterCount then
        ready.set true).widenError
  let fibers ← (List.range waiterCount).mapM fun index =>
    Z.unsafeFork (markEntered *> deferred.await) s!"deferred-many-awaiters-{index}"
  waitForFlag "all Deferred awaiters" ready
  IO.sleep 10
  match ← Z.unsafeRunSync (deferred.succeed 42) "deferred-many-succeed" with
  | .success true => pure ()
  | _ => failTest "Deferred did not accept the shared completion"
  for fiber in fibers do
    match ← fiberExitWithin fiber with
    | some (.success 42) => pure ()
    | some exit => failTest s!"a Deferred awaiter returned {exit}"
    | none => failTest "a Deferred awaiter did not complete"

def testDeferredKeepsFirstCompletion : IO Unit := do
  let deferred ← makeDeferred
  match ← Z.unsafeRunSync (deferred.succeed 1) "deferred-first" with
  | .success true => pure ()
  | _ => failTest "the first Deferred completion did not report success"
  match ← Z.unsafeRunSync (deferred.fail "later") "deferred-second" with
  | .success false => pure ()
  | _ => failTest "a later Deferred completion replaced the first result"
  match ← Z.unsafeRunSync deferred.await "deferred-await-first" with
  | .success 1 => pure ()
  | _ => failTest "Deferred.await did not keep the first completion"
  match ← Z.unsafeRunSync deferred.poll "deferred-poll" with
  | .success (some (.success 1)) => pure ()
  | _ => failTest "Deferred.poll did not return the winning completion"

def testDeferredPreservesFailure : IO Unit := do
  let deferred ← makeDeferred
  match ← Z.unsafeRunSync (deferred.fail "expected") "deferred-fail" with
  | .success true => pure ()
  | _ => failTest "Deferred.fail did not complete the cell"
  match ← Z.unsafeRunSync deferred.await "deferred-await-failure" with
  | .failure (.fail "expected") => pure ()
  | _ => failTest "Deferred.await did not preserve its typed failure"

def testDeferredCompleteUsesEffectExit : IO Unit := do
  let deferred ← makeDeferred
  let source : Z Unit String Nat := Z.fail "from-effect"
  match ← Z.unsafeRunSync (deferred.complete source) "deferred-complete" with
  | .success true => pure ()
  | _ => failTest "Deferred.complete did not accept the source effect exit"
  match ← Z.unsafeRunSync deferred.await "deferred-await-complete" with
  | .failure (.fail "from-effect") => pure ()
  | _ => failTest "Deferred.complete did not preserve the source failure"

/-- Adapted from ZIO `PromiseSpec` polling cases. -/
def testDeferredPollsEachTerminalExit : IO Unit := do
  let pending ← makeDeferred
  match ← Z.unsafeRunSync pending.poll "deferred-poll-pending" with
  | .success none => pure ()
  | _ => failTest "Deferred.poll did not report an unresolved cell"

  let failed ← makeDeferred
  let _ ← Z.unsafeRunSync (failed.fail "expected") "deferred-poll-fail"
  match ← Z.unsafeRunSync failed.poll "deferred-poll-failed" with
  | .success (some (.failure (.fail "expected"))) => pure ()
  | _ => failTest "Deferred.poll did not preserve a typed failure"

  let interrupted ← makeDeferred
  let _ ← Z.unsafeRunSync interrupted.interrupt "deferred-poll-interrupt"
  match ← Z.unsafeRunSync interrupted.poll "deferred-poll-interrupted" with
  | .success (some (.failure .interrupt)) => pure ()
  | _ => failTest "Deferred.poll did not preserve interruption"

def testDeferredInterruptsAwaiter : IO Unit := do
  let deferred ← makeDeferred
  let finalized ← IO.mkRef 0
  let resumed ← IO.mkRef false
  let resumedEffect : Z Unit String Unit :=
    (Z.fromIO (resumed.set true)).widenError
  let fiber ← Z.unsafeFork
    ((deferred.await *> resumedEffect).ensuring <|
      Z.fromIO (finalized.modify (· + 1)))
    "deferred-interrupt"
  IO.sleep 10
  fiber.requestInterrupt
  match ← fiberExitWithin fiber with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"interrupted Deferred awaiter returned {exit}"
  | none => failTest "interrupted Deferred awaiter did not complete"
  match ← Z.unsafeRunSync (deferred.succeed 7) "deferred-after-interrupt" with
  | .success true => pure ()
  | _ => failTest "Deferred did not complete after an awaiter interruption"
  IO.sleep 10
  assertTrue "a cancelled Deferred awaiter resumed after completion"
    (!(← resumed.get))
  assertTrue "Deferred awaiter finalizer ran more than once"
    ((← finalized.get) == 1)

def deferredTests : List (String × IO Unit) := [
  ("testDeferredCompletesWaitingFiber", testDeferredCompletesWaitingFiber),
  ("testDeferredCompletesAllWaitingFibers",
    testDeferredCompletesAllWaitingFibers),
  ("testDeferredKeepsFirstCompletion", testDeferredKeepsFirstCompletion),
  ("testDeferredPreservesFailure", testDeferredPreservesFailure),
  ("testDeferredCompleteUsesEffectExit", testDeferredCompleteUsesEffectExit),
  ("testDeferredPollsEachTerminalExit", testDeferredPollsEachTerminalExit),
  ("testDeferredInterruptsAwaiter", testDeferredInterruptsAwaiter)
]
