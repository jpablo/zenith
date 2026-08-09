import Z

open Fiber

def failTest {A : Type} (message : String) : IO A :=
  throw (IO.userError message)

def assertTrue (message : String) (condition : Bool) : IO Unit :=
  unless condition do
    failTest message

def runProgram [ToString A] (name : String) (program : Z Unit E A) : IO (Exit E A) := do
  match <- Z.unsafeRunSync program name with
  | some exit => pure exit
  | none => failTest s!"{name}: the fiber did not return an exit value"

def observerRaceOnce (index : Nat) : IO Bool := do
  let fiber : Fiber Empty Nat <- Fiber.empty s!"observer-race-{index}"
  let task <- IO.asTask (pure ())
  fiber.setTask task
  let called <- IO.mkRef false
  let waiter <- IO.asTask (fiber.awaitAsync fun _ => called.set true)
  IO.sleep 0
  fiber.complete (.success index)
  let _ <- IO.wait waiter
  let wasCalled <- called.get
  match <- fiber.state.get with
  | .done _ => pure wasCalled
  | _ => pure false

def testFinalizerFailure : IO Unit := do
  let program : Z Unit Empty Nat :=
    (Z.succeedNow 7).ensuring (Z.die (IO.userError "finalizer failed"))
  match <- runProgram "finalizer-failure" program with
  | .failure (.die _) => pure ()
  | _ => failTest "ensuring did not return the finalizer defect"

def testIOErrorCatch : IO Unit := do
  let program : Z Unit Empty String := do
    try
      throw (IO.userError "boom")
    catch _ =>
      pure "recovered"
  match <- runProgram "io-error-catch" program with
  | .success "recovered" => pure ()
  | _ => failTest "the IO.Error catch handler did not recover"

def testExitEquality : IO Unit := do
  let interrupted : Exit Empty Empty := .failure .interrupt
  let defect : Exit Empty Empty := .failure (.die (IO.userError "defect"))
  assertTrue "different exits compare as equal" (interrupted != defect)

def testCompleteBeforeTask : IO Unit := do
  let fiber : Fiber Empty Nat <- Fiber.empty "complete-before-task"
  fiber.complete (.success 42)
  let task <- IO.asTask (pure ())
  fiber.setTask task
  match <- fiber.state.get with
  | .done (.success 42) => pure ()
  | _ => failTest "setTask replaced an exit value that was already complete"

def testAsyncRegistrationFailure : IO Unit := do
  let program : Z Unit Empty Nat := Z.async fun _ =>
    throw (IO.userError "async registration failed")
  match <- runProgram "async-registration-failure" program with
  | .failure (.die _) => pure ()
  | _ => failTest "an async registration failure did not complete the fiber"

def testAsyncInterruption : IO Unit := do
  let pending : Z Unit Empty Nat := Z.async fun _ => pure ()
  let program : Z Unit Empty (Exit Empty Nat) := do
    let fiber <- pending.fork "pending"
    fiber.interrupt
  match <- runProgram "async-interruption" program with
  | .success (.failure .interrupt) => pure ()
  | _ => failTest "interrupting a pending async effect did not complete the fiber"

def testObserverRace : IO Unit := do
  for index in [0:100] do
    assertTrue s!"observer race failed at iteration {index}" (<- observerRaceOnce index)

def testGraphVizEscaping : IO Unit := do
  let escaped := GraphViz.escapeHtml "<unsafe&label>\"'"
  assertTrue "Graphviz HTML text was not escaped"
    (escaped == "&lt;unsafe&amp;label&gt;&quot;&#39;")
  let dotFile := "/tmp/zenith-regression.dot"
  let handle <- IO.FS.Handle.mk dotFile IO.FS.Mode.write
  handle.putStrLn "digraph D {"
  handle.putStrLn (GraphViz.formatNode "id\"with-quote" "effect<&" [("label", "<unsafe&label>")])
  handle.putStrLn "}"
  handle.flush

def testChildDiagramLifetime : IO Unit := do
  let program : Z Unit Empty Unit := do
    let _ <- (Z.sleep 50 *> Z.succeedNow ()).fork "child"
    pure ()
  let dotFile := "/tmp/zenith-child-regression.dot"
  let _ <- Z.unsafeRunSync program "parent" (some dotFile)
  IO.sleep 75
  let contents <- IO.FS.readFile dotFile
  assertTrue "a child fiber wrote after the Graphviz footer"
    (contents.endsWith "}\n")

structure HighGithub : Type 1 where
  getIssues : String -> Z Unit IO.Error (List Nat)

def highGithubSeed : IO Nat :=
  pure 42

def highGithubLayer : Layer Unit IO.Error HighGithub where
  build _ := do
    let seed <- HEIO.liftIO.{1} Cause.die highGithubSeed
    pure {
      getIssues := fun _ => Z.succeedNow' [seed.down]
    }

def highGithubProgram : Z HighGithub IO.Error Nat := do
  let issues <- Z.serviceWithZ fun github =>
    github.getIssues "lean"
  pure issues.length

def testHighUniverseEnvironment : IO Unit := do
  match <- highGithubLayer.run () highGithubProgram "high-environment" with
  | some (.success 1) => pure ()
  | _ => failTest "the high-universe service did not run"

def failingHighGithubLayer : Layer Unit IO.Error HighGithub :=
  Layer.failCause (.fail (IO.userError "layer build failed"))

def testHighUniverseLayerFailure : IO Unit := do
  match <- failingHighGithubLayer.run () highGithubProgram "high-layer-failure" with
  | some (.failure (.fail _)) => pure ()
  | _ => failTest "the high-universe layer failure was not preserved"

def stringLayer : Layer Nat IO.Error String :=
  Layer.fromZ <| Z.serviceWith fun value : Nat => toString value

def stringProgram : Z String IO.Error String :=
  Z.serviceWith id

def testLayerFromZ : IO Unit := do
  match <- stringLayer.run 42 stringProgram "layer-from-z" with
  | some (.success "42") => pure ()
  | _ => failTest "Layer.fromZ did not build and provide its output"

def main : IO Unit := do
  testFinalizerFailure
  testIOErrorCatch
  testExitEquality
  testCompleteBeforeTask
  testAsyncRegistrationFailure
  testAsyncInterruption
  testObserverRace
  testGraphVizEscaping
  testChildDiagramLifetime
  testHighUniverseEnvironment
  testHighUniverseLayerFailure
  testLayerFromZ
  IO.println "All regression tests passed."
