import Z

/-!
Shared helpers for the regression suite.

Several regressions reproduce as a deadlock or a livelock, so every wait here
is bounded: a broken run fails with a message instead of hanging the suite.
-/

def failTest {A : Type} (message : String) : IO A :=
  throw (IO.userError message)

def assertTrue (message : String) (condition : Bool) : IO Unit :=
  unless condition do
    failTest message

def runProgram [ToString A] (name : String) (program : Z Unit E A) : IO (Exit E A) := do
  Z.unsafeRunSync program name

partial def waitForFlag
    (name : String)
    (flag : IO.Ref Bool)
    (attempts : Nat := 1000) : IO Unit := do
  if ← flag.get then
    pure ()
  else if attempts == 0 then
    failTest s!"timed out while waiting for {name}"
  else
    IO.sleep 1
    waitForFlag name flag (attempts - 1)

/-- Poll `flag` and report whether it became true before `attempts` expired. -/
partial def flagBecameTrue
    (flag : IO.Ref Bool)
    (attempts : Nat := 1000) : IO Bool := do
  if ← flag.get then
    pure true
  else if attempts == 0 then
    pure false
  else
    IO.sleep 1
    flagBecameTrue flag (attempts - 1)

/-- Poll a fiber for its exit, giving up once `attempts` expire. -/
partial def fiberExitWithin
    (fiber : Fiber E A)
    (attempts : Nat := 2000) : IO (Option (Exit E A)) := do
  match ← fiber.state.get with
  | .done exit => pure (some exit)
  | _ =>
      if attempts == 0 then
        pure none
      else
        IO.sleep 1
        fiberExitWithin fiber (attempts - 1)

/-- Wait for a task, giving up once `attempts` expire. -/
partial def taskResultWithin
    (task : Task A)
    (attempts : Nat := 2000) : IO (Option A) := do
  if ← BaseIO.toIO (IO.hasFinished task) then
    pure (some task.get)
  else if attempts == 0 then
    pure none
  else
    IO.sleep 1
    taskResultWithin task (attempts - 1)
