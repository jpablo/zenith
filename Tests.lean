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

def highGithubLayer : Layer Unit IO.Error HighGithub :=
  Layer.fromHEIO fun _ => do
    let seed <- HEIO.liftIO.{1} Cause.die highGithubSeed
    pure {
      getIssues := fun _ => Z.succeedNow [seed.down]
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

def recordLayerEvent
    (events : IO.Ref (List String))
    (event : String) : HEIO (Cause IO.Error) Unit :=
  HEIO.bind
    (HEIO.liftIO.{0} Cause.die <|
      events.modify fun current => current ++ [event])
    fun _ => HEIO.pure ()

def trackedLayer
    (events : IO.Ref (List String))
    (name : String) : Layer Unit IO.Error String :=
  Layer.acquireRelease
    (fun _ =>
      HEIO.bind (recordLayerEvent events s!"acquire-{name}") fun _ =>
        HEIO.pure name)
    (fun _ _ => recordLayerEvent events s!"release-{name}")

def testLayerReleaseOrder : IO Unit := do
  let events <- IO.mkRef []
  let layer := (trackedLayer events "left").zipWith
    (trackedLayer events "right")
    (·, ·)
  let program : Z (String × String) IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match <- layer.run () program "layer-release-order" with
  | some (.success ()) => pure ()
  | _ => failTest "the composed layer did not run"
  assertTrue "layer resources were not released in reverse order"
    ((<- events.get) == [
      "acquire-left",
      "acquire-right",
      "release-right",
      "release-left"
    ])

def testLayerReleaseAfterProgramFailure : IO Unit := do
  let events <- IO.mkRef []
  let program : Z String IO.Error Unit :=
    (Z.fail (IO.userError "program failed")).map impossible
  match <- (trackedLayer events "service").run
      () program "layer-program-failure" with
  | some (.failure (.fail _)) => pure ()
  | _ => failTest "the program failure was not preserved"
  assertTrue "the layer resource was not released after program failure"
    ((<- events.get) == ["acquire-service", "release-service"])

def testLayerCleanupAfterAcquisitionFailure : IO Unit := do
  let events <- IO.mkRef []
  let failingRight : Layer Unit IO.Error String :=
    Layer.fromHEIO fun _ =>
      HEIO.bind (recordLayerEvent events "acquire-right") fun _ =>
        HEIO.throw (.fail (IO.userError "right acquisition failed"))
  let layer := (trackedLayer events "left").zipWith failingRight (·, ·)
  let program : Z (String × String) IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match <- layer.run () program "layer-acquisition-failure" with
  | some (.failure (.fail _)) => pure ()
  | _ => failTest "the layer acquisition failure was not preserved"
  assertTrue "an earlier resource was not released after acquisition failure"
    ((<- events.get) == [
      "acquire-left",
      "acquire-right",
      "release-left"
    ])

def testLayerReleaseFailure : IO Unit := do
  let events <- IO.mkRef []
  let layer : Layer Unit IO.Error String :=
    Layer.acquireRelease
      (fun _ =>
        HEIO.bind (recordLayerEvent events "acquire") fun _ =>
          HEIO.pure "service")
      (fun _ _ =>
        HEIO.bind (recordLayerEvent events "release") fun _ =>
          HEIO.throw (.die (IO.userError "release failed")))
  let program : Z String IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match <- layer.run () program "layer-release-failure" with
  | some (.failure (.die _)) => pure ()
  | _ => failTest "the layer release failure was not returned"
  assertTrue "the failing release action did not run exactly once"
    ((<- events.get) == ["acquire", "release"])

def testHighUniverseLayerRelease : IO Unit := do
  let events <- IO.mkRef []
  let layer : Layer Unit IO.Error HighGithub :=
    Layer.acquireRelease
      (fun _ =>
        HEIO.bind (recordLayerEvent events "acquire-high") fun _ =>
          HEIO.pure {
            getIssues := fun _ => Z.succeedNow [1]
          })
      (fun _ _ => recordLayerEvent events "release-high")
  match <- layer.run () highGithubProgram "high-layer-release" with
  | some (.success 1) => pure ()
  | _ => failTest "the scoped high-universe layer did not run"
  assertTrue "the high-universe layer resource was not released"
    ((<- events.get) == ["acquire-high", "release-high"])

def testHighUniverseLayerSharing : IO Unit := do
  let events <- IO.mkRef []
  let source : Layer Unit IO.Error HighGithub :=
    Layer.acquireRelease
      (fun _ =>
        HEIO.bind (recordLayerEvent events "acquire-shared") fun _ =>
          HEIO.pure {
            getIssues := fun _ => Z.succeedNow [1]
          })
      (fun _ _ => recordLayerEvent events "release-shared")
  let sharedSource := source.share fun shared =>
    shared.zipWithPar shared fun first _ => first
  match <- sharedSource.run () highGithubProgram "high-layer-sharing" with
  | some (.success 1) => pure ()
  | some (.success value) =>
      failTest s!"the shared high-universe layer returned {value}"
  | some (.failure cause) =>
      failTest s!"the shared high-universe layer failed: {cause}"
  | none => failTest "the shared high-universe layer returned no result"
  assertTrue "the shared layer was not acquired and released once"
    ((<- events.get) == ["acquire-shared", "release-shared"])

def testHighUniverseParallelLayers : IO Unit := do
  let left : Layer Unit IO.Error HighGithub :=
    Layer.fromHEIO fun _ => HEIO.pure {
      getIssues := fun _ => Z.succeedNow [1]
    }
  let right : Layer Unit IO.Error HighGithub :=
    Layer.fromHEIO fun _ => HEIO.pure {
      getIssues := fun _ => Z.succeedNow [2]
    }
  let combined := left.zipWithPar right fun first _ => first
  match <- combined.run () highGithubProgram "high-layer-parallel" with
  | some (.success 1) => pure ()
  | _ => failTest "parallel high-universe layers did not run"

def observeParallelStart (counter : Std.Mutex Nat) : IO Nat := do
  counter.atomically do modify (· + 1)
  IO.sleep 50
  counter.atomically get

def testParallelLayerOverlap : IO Unit := do
  let counter <- Std.Mutex.new 0
  let branch : Layer Unit IO.Error Nat :=
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die (observeParallelStart counter))
        fun value => HEIO.pure value.down
  let combined := branch.zipWithPar branch (·, ·)
  let program : Z (Nat × Nat) IO.Error (Nat × Nat) :=
    Z.serviceWith id
  match <- combined.run () program "layer-parallel-overlap" with
  | some (.success (2, 2)) => pure ()
  | _ => failTest "parallel layer acquisitions did not overlap"

def testParallelLayerFailureCleanup : IO Unit := do
  let released <- IO.mkRef false
  let left : Layer Unit IO.Error String :=
    Layer.acquireRelease
      (fun _ => HEIO.pure "left")
      (fun _ _ =>
        HEIO.bind (HEIO.liftIO.{0} Cause.die (released.set true)) fun _ =>
          HEIO.pure ())
  let right : Layer Unit IO.Error String :=
    Layer.failCause (.fail (IO.userError "right failed"))
  let combined := left.zipWithPar right (·, ·)
  let program : Z (String × String) IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match <- combined.run () program "layer-parallel-failure" with
  | some (.failure (.fail _)) => pure ()
  | _ => failTest "the parallel layer failure was not preserved"
  assertTrue "the successful parallel acquisition was not released"
    (<- released.get)

def testAcquireReleaseZLayer : IO Unit := do
  let events <- IO.mkRef []
  let acquire : Z Unit IO.Error String :=
    Z.attempt do
      events.modify fun current => current ++ ["acquire-z"]
      pure "service"
  let release (_ : String) : Z Unit Empty Unit :=
    Z.succeed <| events.modify fun current => current ++ ["release-z"]
  let layer := Layer.acquireReleaseZ acquire release
  let program : Z String IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match <- layer.run () program "layer-acquire-release-z" with
  | some (.success ()) => pure ()
  | _ => failTest "the acquireReleaseZ layer did not run"
  assertTrue "acquireReleaseZ did not release its resource"
    ((<- events.get) == ["acquire-z", "release-z"])

def testZDoEnvironmentComposition : IO Unit := do
  let program : Z (String × Nat) Empty (Nat × String) := zdo
    let nat <- Z.environment Nat
    let string <- Z.environment String
    pure (nat, string)
  let closed := program.provideEnvironment ("service", 42)
  match <- runProgram "zdo-environment-composition" closed with
  | .success (42, "service") => pure ()
  | _ => failTest "zdo did not project the declared environment"

def testZDoControlFlow : IO Unit := do
  let branchProgram : Z (String × Nat) Empty String := zdo
    if false then
      let _ <- Z.environment Nat
      pure "number"
    else
      Z.environment String
  match <- runProgram "zdo-terminal-branch"
      (branchProgram.provideEnvironment ("service", 42)) with
  | .success "service" => pure ()
  | _ => failTest "zdo did not adapt a terminal branch action"

  let catchProgram : Z (String × Nat) IO.Error String := zdo
    try
      let _ <- Z.environment Nat
      throw (IO.userError "expected")
    catch _ =>
      Z.environment String
  match <- runProgram "zdo-catch"
      (catchProgram.provideEnvironment ("recovered", 42)) with
  | .success "recovered" => pure ()
  | _ => failTest "zdo did not adapt a catch handler action"

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
  testLayerReleaseOrder
  testLayerReleaseAfterProgramFailure
  testLayerCleanupAfterAcquisitionFailure
  testLayerReleaseFailure
  testHighUniverseLayerRelease
  testHighUniverseLayerSharing
  testHighUniverseParallelLayers
  testParallelLayerOverlap
  testParallelLayerFailureCleanup
  testAcquireReleaseZLayer
  testZDoEnvironmentComposition
  testZDoControlFlow
  IO.println "All regression tests passed."
