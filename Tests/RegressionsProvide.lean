import Tests.Support

/-!
Regression tests for `KeyedLayer.provide`.

The cancellation action registered by `provide` ends with `IO.wait waiter`, so
`HEIO.asyncInterrupt` must never run that action from inside the `waiter` task
itself. The stress test below interrupts a `provide` scope at the moment its
program fiber completes, which is when the two meet.
-/

open Z

private def emptyKeyedLayer :
    KeyedLayer Unit Empty ([] : List Entry.{0}) :=
  ⟨Layer.succeed Services.empty⟩

private partial def spinUntilNanos (deadline : Nat) : IO Unit := do
  if (← IO.monoNanosNow) ≥ deadline then
    pure ()
  else
    spinUntilNanos deadline

private def spinNanos (nanos : Nat) : IO Unit := do
  spinUntilNanos ((← IO.monoNanosNow) + nanos)

/--
Run one `provide` scope whose program completes `delayNanos` before the outer
interruption request, and report whether both the interrupting caller and the
provided scope finished.
-/
private def provideInterruptRaceOnce
    (index : Nat)
    (delayNanos : Nat) : IO Bool := do
  let registered ← IO.mkRef false
  let resume ← IO.mkRef (none : Option (Observer Empty Unit))
  let program : Z (Environment ([] : List Entry.{0})) Empty Unit :=
    Z.async fun callback => do
      resume.set (some callback)
      registered.set true
  let outer ← Z.unsafeFork
    (emptyKeyedLayer.provide program s!"provide-race-{index}")
    s!"provide-race-outer-{index}"
  waitForFlag "the provided program" registered
  -- Give the registration time to return and the waiter task time to block on
  -- the program fiber.
  IO.sleep 1
  let racer ← IO.asTask (prio := Task.Priority.dedicated) do
    match ← resume.get with
    | none => pure ()
    | some callback =>
        callback (.success ())
        spinNanos delayNanos
        outer.requestInterrupt
  let interrupted ← taskResultWithin racer 3000
  let exit ← fiberExitWithin outer 3000
  pure (interrupted.isSome && exit.isSome)

/--
Interrupting `KeyedLayer.provide` while its program is completing must finish.
-/
def testProvideInterruptAtCompletionDoesNotDeadlock : IO Unit := do
  for index in [0:200] do
    unless ← provideInterruptRaceOnce index ((index % 50) * 2000) do
      -- The deadlocked scope also blocks process exit, so report the failure
      -- before throwing: otherwise nothing is ever printed.
      IO.eprintln s!"testProvideInterruptAtCompletionDoesNotDeadlock: \
        interrupting provide at iteration {index} never completed"
      failTest
        "interrupting provide while its program completed deadlocked"

def provideRegressionTests : List (String × IO Unit) := [
  ("testProvideInterruptAtCompletionDoesNotDeadlock",
    testProvideInterruptAtCompletionDoesNotDeadlock)
]
