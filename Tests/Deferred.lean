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
  ("testDeferredKeepsFirstCompletion", testDeferredKeepsFirstCompletion),
  ("testDeferredPreservesFailure", testDeferredPreservesFailure),
  ("testDeferredCompleteUsesEffectExit", testDeferredCompleteUsesEffectExit),
  ("testDeferredInterruptsAwaiter", testDeferredInterruptsAwaiter)
]
