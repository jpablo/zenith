import Z
import Tests.Support
import Tests.Regressions
import Tests.RegressionsProvide
import Tests.RegressionsKeyed
import Tests.HEIO
import Tests.Primitives
import Tests.Scope
import Examples.GithubIssueSync
import Examples.StableServiceKeysDemo
import Examples.TodoReport
import Std.Data.HashSet

open Fiber

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

def testAsyncImmediateResumeWins : IO Unit := do
  let program : Z Unit Empty Nat := Z.async fun callback => do
    callback (.success 7)
    callback (.success 8)
    throw (IO.userError "registration failed after completion")
  match <- runProgram "async-immediate-resume" program with
  | .success 7 => pure ()
  | _ => failTest "an immediate async effect did not keep its first exit"

def testAsyncDelayedResume : IO Unit := do
  let completed <- IO.mkRef false
  let program : Z Unit Empty Nat := Z.async fun callback => do
    let _ <- IO.asTask do
      IO.sleep 5
      completed.set true
      callback (.success 42)
    pure ()
  match <- runProgram "async-delayed-resume" program with
  | .success 42 => pure ()
  | _ => failTest "unsafeRunSync did not wait for a delayed async exit"
  assertTrue "unsafeRunSync returned before the delayed callback"
    (<- completed.get)

def testAsyncInterruption : IO Unit := do
  let pending : Z Unit Empty Nat := Z.async fun _ => pure ()
  let program : Z Unit Empty (Exit Empty Nat) := do
    let fiber <- pending.fork "pending"
    fiber.interrupt
  match <- runProgram "async-interruption" program with
  | .success (.failure .interrupt) => pure ()
  | _ => failTest "interrupting a pending async effect did not complete the fiber"

def testAsyncInterruptCanceler : IO Unit := do
  let registering ← IO.mkRef false
  let cancelled ← IO.mkRef false
  let pending : Z Unit Empty Unit := Z.asyncInterrupt fun _ => do
    registering.set true
    IO.sleep 20
    pure (cancelled.set true)
  let fiber ← Z.unsafeFork pending "async-interrupt-canceler"
  waitForFlag "cancellable async registration" registering
  fiber.requestInterrupt
  match ← fiber.await with
  | .failure .interrupt => pure ()
  | _ => failTest "cancellable async interruption returned the wrong exit"
  fiber.awaitTask
  assertTrue "cancellable async interruption did not run its canceler"
    (← cancelled.get)

def testAsyncInterruptCancelerFailure : IO Unit := do
  let registered ← IO.mkRef false
  let pending : Z Unit Empty Unit := Z.asyncInterrupt fun _ => do
    registered.set true
    pure (throw (IO.userError "canceler failed"))
  let fiber ← Z.unsafeFork pending "async-interrupt-canceler-failure"
  waitForFlag "failing cancellable async registration" registered
  fiber.requestInterrupt
  match ← fiber.await with
  | .failure (.die _) => pure ()
  | _ => failTest "a cancellable async canceler defect did not complete the fiber"
  fiber.awaitTask

def testFromAsyncResult : IO Unit := do
  let success : Std.Async.Async Nat := pure 42
  match ← runProgram "from-async-success" (Z.fromAsync success) with
  | .success 42 => pure ()
  | _ => failTest "fromAsync did not preserve a successful result"

  let failure : Std.Async.Async Nat :=
    throw (IO.userError "expected asynchronous failure")
  match ← runProgram "from-async-failure" (Z.fromAsync failure) with
  | .failure (.fail _) => pure ()
  | _ => failTest "fromAsync did not put IO.Error in the typed error channel"

  let registrationFailure : Z Unit IO.Error Nat :=
    Z.fromAsyncInterrupt do
      throw (IO.userError "expected registration failure")
  match ← runProgram "from-async-registration-failure"
      registrationFailure with
  | .failure (.fail _) => pure ()
  | _ => failTest "fromAsyncInterrupt did not preserve a registration error"

def testFromAsyncInterruption : IO Unit := do
  let registered ← IO.mkRef false
  let cancelled ← IO.mkRef false
  let completion ← IO.Promise.new (α := Except IO.Error Unit)
  let pending : Z Unit IO.Error Unit :=
    Z.fromAsyncInterrupt do
      registered.set true
      let task := Std.Async.AsyncTask.ofPromise completion
      let cancel := do
        cancelled.set true
        completion.resolve (.error (IO.userError "cancelled"))
      pure (task, cancel)
  let fiber ← Z.unsafeFork pending "from-async-interruption"
  waitForFlag "Std.Async adapter registration" registered
  fiber.requestInterrupt
  match ← fiber.await with
  | .failure .interrupt => pure ()
  | _ => failTest "interrupting fromAsyncInterrupt returned the wrong exit"
  fiber.awaitTask
  assertTrue "fromAsyncInterrupt did not run its cancellation action"
    (← cancelled.get)

def testAsyncSleepCompletion : IO Unit := do
  let program : Z Unit Empty Nat := Z.sleep 5 *> Z.succeedNow 42
  match ← runProgram "asynchronous-sleep-completion" program with
  | .success 42 => pure ()
  | _ => failTest "the asynchronous timer did not resume its Zenith fiber"

def testAsyncSleepInterruption : IO Unit := do
  let fiber ← Z.unsafeFork (Z.sleep 2000) "asynchronous-sleep-interruption"
  IO.sleep 25
  let before ← IO.monoMsNow.toIO
  fiber.requestInterrupt
  match ← fiber.await with
  | .failure .interrupt => pure ()
  | _ => failTest "interrupting the asynchronous timer returned the wrong exit"
  fiber.awaitTask
  let elapsed := (← IO.monoMsNow.toIO) - before
  assertTrue s!"interrupting Z.sleep took {elapsed} ms" (elapsed < 1000)

private def concurrentSleeps
    (count : Nat)
    (duration : UInt32) : Z Unit Empty Unit := do
  let fibers ← (List.range count).mapM fun index =>
    (Z.sleep duration).fork s!"concurrent-sleep-{index}"
  for fiber in fibers do
    fiber.join

def testAsyncSleepConcurrency : IO Unit := do
  let before ← IO.monoMsNow.toIO
  match ← runProgram "asynchronous-sleep-concurrency"
      (concurrentSleeps 100 100) with
  | .success () => pure ()
  | _ => failTest "concurrent asynchronous timers did not complete"
  let elapsed := (← IO.monoMsNow.toIO) - before
  assertTrue s!"100 concurrent 100 ms sleeps took {elapsed} ms"
    (elapsed < 750)

private def failingAsyncDiagram : ExecutionDiagram (IO Unit) :=
  { ExecutionDiagram.empty with
    enabled := true
    async := fun _ _ _ =>
      throw (IO.userError "asynchronous diagram write failed") }

private partial def waitForFiberExit
    (fiber : Fiber E A)
    (attempts : Nat := 1000) : IO (Option (Exit E A)) := do
  match <- fiber.state.get with
  | .done exit => pure (some exit)
  | _ =>
      if attempts == 0 then
        pure none
      else
        IO.sleep 1
        waitForFiberExit fiber (attempts - 1)

private partial def waitForCallback
    (callbackRef : IO.Ref (Option (Observer E A)))
    (attempts : Nat := 1000) : IO Unit := do
  if (<- callbackRef.get).isSome then
    pure ()
  else if attempts == 0 then
    failTest "timed out while waiting for callback registration"
  else
    IO.sleep 1
    waitForCallback callbackRef (attempts - 1)

private def runFailingAsyncResume
    (name : String)
    (effect : IO.Ref (Option (Observer Empty Unit)) ->
      ZCore Unit Empty Unit) : IO Unit := do
  let callbackRef <- IO.mkRef (none : Option (Observer Empty Unit))
  let startTime <- IO.monoMsNow.toIO
  let fiber <- ZCore.unsafeRunFiber failingAsyncDiagram
    (effect callbackRef) Environment.empty "" name startTime
  waitForCallback callbackRef
  match <- callbackRef.get with
  | none => failTest s!"{name}: callback was not registered"
  | some callback =>
      try callback (.success ())
      catch _ => pure ()
  match <- waitForFiberExit fiber with
  | some (.failure (.die _)) => pure ()
  | _ => failTest s!"{name}: callback defect did not complete the fiber"

def testAsyncResumeDefect : IO Unit :=
  runFailingAsyncResume "async-resume-defect" fun callbackRef =>
    ZCore.async fun callback => callbackRef.set (some callback)

def testAsyncInterruptResumeDefect : IO Unit :=
  runFailingAsyncResume "async-interrupt-resume-defect" fun callbackRef =>
    ZCore.asyncInterrupt fun callback => do
      callbackRef.set (some callback)
      pure IO.unit

def testUnsafeRunSyncHasNoPollingDelay : IO Unit := do
  let before <- IO.monoMsNow.toIO
  for index in [0:4] do
    let _ <- Z.unsafeRunSync (Z.succeedNow index) s!"direct-wait-{index}"
  let elapsed := (<- IO.monoMsNow.toIO) - before
  assertTrue s!"four immediate runs took {elapsed} ms" (elapsed < 250)

def testInterpreterLoggingIsDisabledByDefault : IO Unit := do
  assertTrue "the interpreter wrote a trace with default settings"
    !ENABLE_LOG
  assertTrue "runtime logging was enabled by default"
    !(<- RuntimeLog.isEnabled)
  let (output, _) <- IO.FS.withIsolatedStreams do
    RuntimeLog.setEnabled true
    try
      log "logging-test" "enabled"
      let _ <- Z.unsafeRunSync (Z.succeedNow ()) "logging-interpreter-test"
    finally
      RuntimeLog.setEnabled false
  assertTrue "runtime logging could not be enabled"
    (output.contains "[logging-test] enabled")
  assertTrue "the interpreter did not use logging enabled before its run"
    (output.contains "[runLoop]")

def testFiberIdsAreUnique : IO Unit := do
  let ids <- IO.mkRef ({} : Std.HashSet String)
  let fibers <- IO.mkRef ([] : List (Fiber Empty Unit))
  let duplicate <- IO.mkRef false
  for _ in [0:1000] do
    let fiber <- Z.unsafeFork (Z.succeedNow ()) "unique-id"
    fibers.modify (fiber :: ·)
    let fiberId := toString fiber.fiberId
    let isDuplicate <- ids.modifyGet fun current =>
      if current.contains fiberId then
        (true, current)
      else
        (false, current.insert fiberId)
    if isDuplicate then
      duplicate.set true
  for fiber in (<- fibers.get) do
    let _ <- fiber.await
  assertTrue "the runtime generated duplicate fiber IDs"
    !(<- duplicate.get)

def testHEIOAsyncInterruption : IO Unit := do
  let registered ← IO.mkRef false
  let cancelled ← IO.mkRef false
  let finalized ← IO.mkRef false
  let pending : HEIO (Cause IO.Error) (ULift.{1} Unit) :=
    HEIO.asyncInterrupt Cause.die fun _ => do
      registered.set true
      IO.sleep 20
      pure (cancelled.set true)
  let finalizer : HEIO (Cause IO.Error) Unit :=
    HEIO.bind
      (HEIO.liftIO.{0} Cause.die (finalized.set true))
      fun _ => HEIO.pure ()
  let interruption ← HEIO.Interruption.new
  let worker ← IO.asTask <|
    HEIO.toIOResultInterruptible
      interruption (Cause.interrupt : Cause IO.Error)
      (pending.ensuring finalizer)
  waitForFlag "HEIO asynchronous registration" registered
  interruption.request
  match ← IO.wait worker with
  | .ok (.error .interrupt) => pure ()
  | _ => failTest "HEIO interruption returned the wrong result"
  assertTrue "HEIO interruption did not run its cancellation action"
    (← cancelled.get)
  assertTrue "HEIO interruption did not run its protected finalizer"
    (← finalized.get)

def testPreInterruptedLayerBuild : IO Unit := do
  let acquired ← IO.mkRef false
  let layer : Layer Unit IO.Error Unit :=
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die (acquired.set true))
        fun _ => HEIO.pure ()
  let interruption ← HEIO.Interruption.new
  interruption.request
  let build := (layer.build ()).map (ULift.up :
    Layer.Resource IO.Error Unit ->
      ULift.{0} (Layer.Resource IO.Error Unit))
  match ← HEIO.toIOResultInterruptible
      interruption (Cause.interrupt : Cause IO.Error) build with
  | .error .interrupt => pure ()
  | _ => failTest "a pre-interrupted layer build returned the wrong result"
  assertTrue "a layer started after interruption was requested"
    !(← acquired.get)

def testParallelLayerInterruption : IO Unit := do
  let leftStarted ← IO.mkRef false
  let rightStarted ← IO.mkRef false
  let leftCancelled ← IO.mkRef false
  let rightCancelled ← IO.mkRef false
  let pendingLayer
      (started : IO.Ref Bool)
      (cancelled : IO.Ref Bool) : Layer Unit IO.Error Unit :=
    Layer.fromHEIO fun _ =>
      let pending : HEIO (Cause IO.Error) (ULift.{0} Unit) :=
        HEIO.asyncInterrupt Cause.die fun _ => do
          started.set true
          pure (cancelled.set true)
      HEIO.bind pending fun _ => HEIO.pure ()
  let combined :=
    (pendingLayer leftStarted leftCancelled).zipWithPar
      (pendingLayer rightStarted rightCancelled)
      (fun _ _ => ())
  let build := (combined.build ()).map (ULift.up :
    Layer.Resource IO.Error Unit ->
      ULift.{0} (Layer.Resource IO.Error Unit))
  let interruption ← HEIO.Interruption.new
  let worker ← IO.asTask <|
    HEIO.toIOResultInterruptible
      interruption (Cause.interrupt : Cause IO.Error) build
  waitForFlag "left parallel layer acquisition" leftStarted
  waitForFlag "right parallel layer acquisition" rightStarted
  interruption.request
  match ← IO.wait worker with
  | .ok (.error .interrupt) => pure ()
  | _ => failTest "parallel layer interruption returned the wrong result"
  assertTrue "the left parallel acquisition was not cancelled"
    (← leftCancelled.get)
  assertTrue "the right parallel acquisition was not cancelled"
    (← rightCancelled.get)

def testObserverRace : IO Unit := do
  for index in [0:100] do
    assertTrue s!"observer race failed at iteration {index}" (<- observerRaceOnce index)

def testGraphVizEscaping : IO Unit := do
  let escaped := GraphViz.escapeHtml "<unsafe&label>\"'"
  assertTrue "Graphviz HTML text was not escaped"
    (escaped == "&lt;unsafe&amp;label&gt;&quot;&#39;")
  assertTrue "Graphviz did not quote an identifier"
    (GraphViz.quoteId "id\"with-quote" == "\"id\\\"with-quote\"")
  let node := GraphViz.formatNode
    "id\"with-quote"
    "effect<&"
    [("label", "<unsafe&label>")]
    "red&white"
  assertTrue "Graphviz did not quote the node identifier"
    (node.startsWith "\"id\\\"with-quote\" [shape=none")
  assertTrue "Graphviz did not escape the node text"
    (node.contains "effect&lt;&amp;")
  assertTrue "Graphviz did not escape an extra field"
    (node.contains "&lt;unsafe&amp;label&gt;")
  assertTrue "Graphviz did not escape the color attribute"
    (node.contains "BGCOLOR=\"red&amp;white\"")

def testGraphVizDiagramEvents : IO Unit :=
  IO.FS.withTempFile fun handle path => do
    let diagram := GraphViz.graphvizIO handle
    let interruption : Interruption := {
      interrupted := ← IO.mkRef true
      isInterruptible := ← IO.mkRef true
      isInterrupting := false
      interruptDelivered := ← IO.mkRef false
      interruptHandler := ← IO.mkRef IO.unit
    }
    diagram.header
    diagram.currentNode
      "label<&" "effect<&" "node\"id" interruption 10 15 3 "blue"
    diagram.errorHandler (some "node\"id") "handler"
    diagram.continue_ (some "handler") "continue"
    diagram.interruption "interrupt" "next" 20 10
    diagram.done "fiber" "next" "green" "done<&"
    diagram.syncTry "fiber" "sync" 0
    diagram.onSuccess "sync" "success"
    diagram.async "fiber" "async" 0
    diagram.fork "fiber" "success" "child" 20 10 "child-box"
    diagram.onSuccessAndFailure "success" "both"
    diagram.setInterruptStatus "both" "original" "generated"
    diagram.widenEnv "generated" "wide"
    diagram.provideEnvironment "fiber" "wide" "provided" "orange"
    diagram.footer
    handle.flush
    let contents ← IO.FS.readFile path
    assertTrue "Graphviz output did not contain its header"
      (contents.startsWith "digraph D {")
    assertTrue "Graphviz output did not contain its footer"
      (contents.endsWith "}\n")
    assertTrue "Graphviz output did not escape a runtime label"
      (contents.contains "label&lt;&amp;")
    assertTrue "Graphviz output did not quote an arrow endpoint"
      (contents.contains "\"node\\\"id\" -> \"handler\"")
    assertTrue "Graphviz output did not contain the recovery edge"
      (contents.contains "λ (recover)")
    assertTrue "Graphviz output did not contain the interruption node"
      (contents.contains "interrupted!")
    assertTrue "Graphviz output did not contain the child fiber node"
      (contents.contains "new fiber")
    assertTrue "Graphviz output did not contain the environment node"
      (contents.contains "Environment")

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
  | .success 1 => pure ()
  | _ => failTest "the high-universe service did not run"

def failingHighGithubLayer : Layer Unit IO.Error HighGithub :=
  Layer.failCause (.fail (IO.userError "layer build failed"))

def testHighUniverseLayerFailure : IO Unit := do
  match <- failingHighGithubLayer.run () highGithubProgram "high-layer-failure" with
  | .failure (.fail _) => pure ()
  | _ => failTest "the high-universe layer failure was not preserved"

def stringLayer : Layer Nat IO.Error String :=
  Layer.fromZ <| Z.serviceWith fun value : Nat => toString value

def stringProgram : Z String IO.Error String :=
  Z.serviceWith id

def testLayerFromZ : IO Unit := do
  match <- stringLayer.run 42 stringProgram "layer-from-z" with
  | .success "42" => pure ()
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
  | .success () => pure ()
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
  | .failure (.fail _) => pure ()
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
  | .failure (.fail _) => pure ()
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
  | .failure (.die _) => pure ()
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
  | .success 1 => pure ()
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
  | .success 1 => pure ()
  | .success value =>
      failTest s!"the shared high-universe layer returned {value}"
  | .failure cause =>
      failTest s!"the shared high-universe layer failed: {cause}"
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
  | .success 1 => pure ()
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
  | .success (2, 2) => pure ()
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
  | .failure (.fail _) => pure ()
  | _ => failTest "the parallel layer failure was not preserved"
  assertTrue "the successful parallel acquisition was not released"
    (<- released.get)

def testParallelLayerFailureCancelsSibling : IO Unit := do
  let leftStarted ← IO.mkRef false
  let leftCancelled ← IO.mkRef false
  let left : Layer Unit IO.Error String :=
    Layer.fromHEIO fun _ =>
      let pending : HEIO (Cause IO.Error) (ULift.{0} Unit) :=
        HEIO.asyncInterrupt Cause.die fun callback => do
          leftStarted.set true
          let _ ← IO.asTask do
            IO.sleep 100
            callback (.ok ())
          pure (leftCancelled.set true)
      HEIO.bind pending fun _ => HEIO.pure "left"
  let right : Layer Unit IO.Error String :=
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die
          (waitForFlag "pending parallel sibling" leftStarted))
        fun _ => HEIO.throw (.fail (IO.userError "right failed"))
  let combined := left.zipWithPar right (·, ·)
  let program : Z (String × String) IO.Error Unit :=
    Z.serviceWith fun _ => ()
  match ← combined.run () program "layer-parallel-fail-fast" with
  | .failure (.fail _) => pure ()
  | _ => failTest "parallel layer failure did not preserve its typed error"
  assertTrue "parallel layer failure did not cancel its pending sibling"
    (← leftCancelled.get)

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
  | .success () => pure ()
  | _ => failTest "the acquireReleaseZ layer did not run"
  assertTrue "acquireReleaseZ did not release its resource"
    ((<- events.get) == ["acquire-z", "release-z"])

structure IssueSyncScenario where
  config : Except GithubIssueSync.ConfigError GithubIssueSync.SyncConfig
  github : Except GithubIssueSync.GithubError (List GithubIssueSync.Issue)
  storeFailure : Option Nat := none
  auditFails : Bool := false

def recordIssueSyncEvent
    (events : IO.Ref (List String))
    (event : String) : Z Unit Empty Unit :=
  Z.succeed <| events.modify fun current => current ++ [event]

def makeIssueSyncServices
    (events : IO.Ref (List String))
    (scenario : IssueSyncScenario) :
    GithubIssueSync.ConfigService ×
      GithubIssueSync.GithubService ×
      GithubIssueSync.IssueStore ×
      GithubIssueSync.Audit :=
  let config : GithubIssueSync.ConfigService := {
    load := (zdo
      let _ ← recordIssueSyncEvent events "config"
      match scenario.config with
      | .ok value => Z.succeedNow value
      | .error error => Z.fail error :
        Z Unit GithubIssueSync.ConfigError GithubIssueSync.SyncConfig)
  }
  let github : GithubIssueSync.GithubService := {
    openIssues := fun organization => (zdo
      let _ ← recordIssueSyncEvent events s!"github:{organization}"
      match scenario.github with
      | .ok issues => Z.succeedNow issues
      | .error error => Z.fail error :
        Z Unit GithubIssueSync.GithubError (List GithubIssueSync.Issue))
  }
  let store : GithubIssueSync.IssueStore := {
    save := fun issue => zdo
      let _ ← recordIssueSyncEvent events s!"store:{issue.id}"
      if scenario.storeFailure == some issue.id then
        Z.fail (.writeFailed issue.id)
      else
        pure ()
  }
  let audit : GithubIssueSync.Audit := {
    recordFailure := fun message => zdo
      let _ ← recordIssueSyncEvent events s!"audit:{message}"
      if scenario.auditFails then
        Z.fail .unavailable
      else
        pure ()
    finish := recordIssueSyncEvent events "finish"
  }
  (config, github, store, audit)

def issueSyncProgram
    (events : IO.Ref (List String))
    (scenario : IssueSyncScenario) :
    Z Unit Empty Nat :=
  let (config, github, store, audit) :=
    makeIssueSyncServices events scenario
  (GithubIssueSync.application config github store audit).provideEnvironment
    Z.Services.empty

def testGithubIssueSync : IO Unit := do
  let issues : List GithubIssueSync.Issue := [
    { id := 1, title := "first" },
    { id := 2, title := "second" }
  ]

  let successEvents ← IO.mkRef ([] : List String)
  let successScenario : IssueSyncScenario := {
    config := .ok { organization := "lean", dryRun := false }
    github := .ok issues
  }
  match ← runProgram "issue-sync-success"
      (issueSyncProgram successEvents successScenario) with
  | .success 2 => pure ()
  | _ => failTest "the issue sync success path failed"
  assertTrue "the issue sync success events are incorrect"
    ((← successEvents.get) ==
      ["config", "github:lean", "store:1", "store:2", "finish"])

  let dryRunEvents ← IO.mkRef ([] : List String)
  let dryRunScenario : IssueSyncScenario := {
    config := .ok { organization := "lean", dryRun := true }
    github := .ok issues
  }
  match ← runProgram "issue-sync-dry-run"
      (issueSyncProgram dryRunEvents dryRunScenario) with
  | .success 2 => pure ()
  | _ => failTest "the issue sync dry-run path failed"
  assertTrue "the dry-run path wrote issues"
    ((← dryRunEvents.get) == ["config", "github:lean", "finish"])

  let sourceFailureEvents ← IO.mkRef ([] : List String)
  let sourceFailureScenario : IssueSyncScenario := {
    config := .ok { organization := "lean", dryRun := false }
    github := .error .unavailable
  }
  match ← runProgram "issue-sync-source-recovery"
      (issueSyncProgram sourceFailureEvents sourceFailureScenario) with
  | .success 0 => pure ()
  | _ => failTest "the issue sync did not recover from a source failure"
  assertTrue "the source recovery or finalizer events are incorrect"
    ((← sourceFailureEvents.get) ==
      ["config", "github:lean", "audit:GitHub unavailable", "finish"])

  let auditFailureEvents ← IO.mkRef ([] : List String)
  let auditFailureScenario : IssueSyncScenario := {
    config := .error .unavailable
    github := .ok issues
    auditFails := true
  }
  match ← runProgram "issue-sync-audit-recovery"
      (issueSyncProgram auditFailureEvents auditFailureScenario) with
  | .success 0 => pure ()
  | _ => failTest "a later catch did not recover from the audit failure"
  assertTrue "the finalizer did not run after the audit failure"
    ((← auditFailureEvents.get) ==
      ["config", "audit:configuration unavailable", "finish"])

  let rawFailureEvents ← IO.mkRef ([] : List String)
  let rawFailureScenario : IssueSyncScenario := {
    config := .ok { organization := "lean", dryRun := false }
    github := .ok issues
    storeFailure := some 2
  }
  let (rawConfig, rawGithub, rawStore, _) :=
    makeIssueSyncServices rawFailureEvents rawFailureScenario
  let rawProgram : Z Unit GithubIssueSync.SourceErrors Nat :=
    (GithubIssueSync.rawApplication rawConfig rawGithub rawStore)
      |>.provideEnvironment Z.Services.empty
  match ← runProgram "issue-sync-raw-failure" rawProgram with
  | .failure (.fail (.inr (.inr (.writeFailed 2)))) => pure ()
  | _ => failTest "the raw issue sync did not expose the store failure"
  assertTrue "the raw failure ran an unexpected finalizer"
    ((← rawFailureEvents.get) ==
      ["config", "github:lean", "store:1", "store:2"])

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

def testZDoInferredEnvironment : IO Unit := do
  let combined := zdo[Empty]
    let nat <- Z.environment Nat
    let string <- Z.environment String
    pure (nat, string)
  let combinedClosed : Z Unit Empty (Nat × String) :=
    combined.provideEnvironment (42, "service")
  match <- runProgram "zdo-inferred-environment" combinedClosed with
  | .success (42, "service") => pure ()
  | _ => failTest "zdo did not infer and project the combined environment"

  let repeated := zdo[Empty]
    let first <- Z.environment Nat
    let second <- Z.environment Nat
    pure (first, second)
  let repeatedClosed : Z Unit Empty (Nat × Nat) :=
    repeated.provideEnvironment 42
  match <- runProgram "zdo-inferred-duplicate" repeatedClosed with
  | .success (42, 42) => pure ()
  | _ => failTest "zdo did not remove a repeated environment requirement"

  let reordered := zdo[Empty]
    let string <- Z.environment String
    let nat <- Z.environment Nat
    pure (string, nat)
  let reorderedClosed : Z Unit Empty (String × Nat) :=
    reordered.provideEnvironment (42, "reordered")
  match <- runProgram "zdo-inferred-reordered" reorderedClosed with
  | .success ("reordered", 42) => pure ()
  | _ => failTest "zdo did not normalize reordered environment requirements"

  let grouped := zdo[Empty]
    Z.environment (String × Nat)
  let groupedClosed : Z Unit Empty (String × Nat) :=
    grouped.provideEnvironment (42, "grouped")
  match <- runProgram "zdo-inferred-grouped" groupedClosed with
  | .success ("grouped", 42) => pure ()
  | _ => failTest "zdo did not normalize a grouped environment requirement"

  let highService : HighGithub := {
    getIssues := fun _ => Z.succeedNow [1, 2]
  }
  let highProgram := zdo[IO.Error]
    let issues <- Z.serviceWithZ fun github : HighGithub =>
      github.getIssues "lean"
    let suffix <- Z.environment String
    pure (issues.length + suffix.length)
  let highClosed : Z Unit IO.Error Nat :=
    highProgram.provideEnvironment (highService, "ok")
  match <- runProgram "zdo-inferred-high-environment" highClosed with
  | .success 4 => pure ()
  | _ => failTest "zdo did not infer a high-universe environment"

def testZDoInferredControlFlow : IO Unit := do
  let branchProgram (selectNat : Bool) := zdo[Empty]
    if selectNat then
      let _ <- Z.environment Nat
      pure "number"
    else
      Z.environment String
  let branchClosed : Z Unit Empty String :=
    (branchProgram false).provideEnvironment (42, "service")
  match <- runProgram "zdo-inferred-branch" branchClosed with
  | .success "service" => pure ()
  | _ => failTest "zdo did not infer the environment of an if branch"

  let matchProgram (selection : Option Bool) := zdo[Empty]
    match selection with
    | some true =>
        let _ <- Z.environment Nat
        pure "number"
    | _ => Z.environment String
  let matchClosed : Z Unit Empty String :=
    (matchProgram none).provideEnvironment (42, "matched")
  match <- runProgram "zdo-inferred-match" matchClosed with
  | .success "matched" => pure ()
  | _ => failTest "zdo did not infer the environment of a match branch"

  let catchProgram := zdo[IO.Error]
    try
      let _ <- Z.environment Nat
      throw (IO.userError "expected")
    catch _ =>
      Z.environment String
  let catchClosed : Z Unit IO.Error String :=
    catchProgram.provideEnvironment (42, "recovered")
  match <- runProgram "zdo-inferred-catch" catchClosed with
  | .success "recovered" => pure ()
  | _ => failTest "zdo did not infer the environment of a catch handler"

  let loopProgram := zdo[Empty]
    for _ in [1, 2] do
      let _ <- Z.environment Nat
      pure ()
    Z.environment String
  let loopClosed : Z Unit Empty String :=
    loopProgram.provideEnvironment (42, "looped")
  match <- runProgram "zdo-inferred-loop" loopClosed with
  | .success "looped" => pure ()
  | _ => failTest "zdo did not infer the environment of a loop"

  let returnProgram (stopEarly : Bool) := zdo[Empty]
    if stopEarly then
      return "early"
    let _ <- Z.environment Nat
    Z.environment String
  let returnClosed : Z Unit Empty String :=
    (returnProgram true).provideEnvironment (42, "late")
  match <- runProgram "zdo-inferred-return" returnClosed with
  | .success "early" => pure ()
  | _ => failTest "zdo did not preserve return during environment inference"

def testErrorChannelJoin : IO Unit := do
  let leftFailure : Z Unit String Unit := Z.fail "left"
  let leftProgram : Z Unit (String ⊕ IO.Error) Unit :=
    Z.flatMapJoin leftFailure fun _ => Z.attempt (pure ())
  match <- runProgram "error-join-left" leftProgram with
  | .failure (.fail (.inl "left")) => pure ()
  | _ => failTest "error join did not inject the left error"

  let leftSuccess : Z Unit String Unit := Z.succeedNow ()
  let rightProgram : Z Unit (String ⊕ IO.Error) Unit :=
    Z.flatMapMeetJoin leftSuccess fun _ =>
      Z.attempt (throw (IO.userError "right"))
  match <- runProgram "error-join-right" rightProgram with
  | .failure (.fail (.inr _)) => pure ()
  | _ => failTest "error join did not inject the right error"

def testZDoInferredErrors : IO Unit := do
  let leftFailure : Z Unit String Nat := Z.fail "left"
  let leftProgram := zdo
    let value <- leftFailure
    let _ <- Z.attempt (pure ())
    pure value
  let leftProgram : Z Unit (IO.Error ⊕ String) Nat := leftProgram
  match <- runProgram "zdo-inferred-error-left" leftProgram with
  | .failure (.fail (.inr "left")) => pure ()
  | _ => failTest "zdo did not inject an inferred String error"

  let stringSuccess : Z Unit String Unit := Z.succeedNow ()
  let failingIO : IO Nat := throw (IO.userError "right")
  let rightProgram := zdo
    let _ <- stringSuccess
    Z.attempt failingIO
  let rightProgram : Z Unit (IO.Error ⊕ String) Nat := rightProgram
  match <- runProgram "zdo-inferred-error-right" rightProgram with
  | .failure (.fail (.inl _)) => pure ()
  | _ => failTest "zdo did not inject an inferred IO.Error"

  let swappedFailure : Z Unit (String ⊕ IO.Error) Nat :=
    Z.failCause (R := Unit)
      (.fail (Sum.inl "swapped" : String ⊕ IO.Error))
  let normalizedProgram := zdo
    swappedFailure
  let normalizedProgram : Z Unit (IO.Error ⊕ String) Nat := normalizedProgram
  match <- runProgram "zdo-normalized-existing-error-sum" normalizedProgram with
  | .failure (.fail (.inr "swapped")) => pure ()
  | _ => failTest "zdo did not normalize an existing error sum"

  let handledBody : Z Nat String Nat := Z.fail "handled"
  let handledProgram := zdo
    handledBody.catchAllMeet fun _ =>
      (Z.environment String).map String.length
  let handledClosed : Z Unit Empty Nat :=
    handledProgram.provideEnvironment (42, "done")
  match <- runProgram "zdo-inferred-handled-error" handledClosed with
  | .success 4 => pure ()
  | _ => failTest "zdo retained an error handled by catchAllMeet"

  let combinedProgram := zdo
    let nat <- Z.environment Nat
    let _ <- stringSuccess
    let string <- Z.environment String
    let value <- Z.attempt (pure 2)
    pure (nat + string.length + value)
  let combinedClosed : Z Unit (IO.Error ⊕ String) Nat :=
    combinedProgram.provideEnvironment (42, "ok")
  match <- runProgram "zdo-inferred-environment-and-error" combinedClosed with
  | .success 46 => pure ()
  | _ => failTest "zdo did not infer both environment and error parameters"

def testZDoInferredCatch : IO Unit := do
  let typedCaught := zdo
    try
      let _ <- Z.environment Nat
      let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
      pure 0
    catch _ =>
      (Z.environment String).map String.length
  let typedCaughtClosed : Z Unit Empty Nat :=
    typedCaught.provideEnvironment (42, "done")
  match <- runProgram "zdo-inferred-typed-catch" typedCaughtClosed with
  | .success 4 => pure ()
  | _ => failTest "zdo retained a handled typed error"

  let failingHandler := zdo
    try
      let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
      pure 0
    catch _ =>
      Z.attempt (throw (IO.userError "handler"))
  let failingHandler : Z Unit IO.Error Nat := failingHandler
  match <- runProgram "zdo-inferred-catch-handler-error" failingHandler with
  | .failure (.fail _) => pure ()
  | _ => failTest "zdo did not expose a catch handler error"

  let defectCaught := zdo
    try
      let _ : Nat <- throw (IO.userError "defect")
      pure 0
    catch _ =>
      Z.succeedNow 7
  let defectCaught : Z Unit Empty Nat := defectCaught
  match <- runProgram "zdo-inferred-defect-catch" defectCaught with
  | .success 7 => pure ()
  | _ => failTest "zdo did not catch an IO.Error defect"

  let earlyReturn (stop : Bool) := zdo
    try
      if stop then return 7
      let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
      pure ()
    catch _ =>
      pure ()
    pure 9
  let earlyReturn : Z Unit Empty Nat := earlyReturn true
  match <- runProgram "zdo-inferred-catch-return" earlyReturn with
  | .success 7 => pure ()
  | _ => failTest "zdo did not forward an early return through catch"

  let handlerReturn := zdo
    try
      let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
      pure ()
    catch _ =>
      return 8
    pure 9
  let handlerReturn : Z Unit Empty Nat := handlerReturn
  match <- runProgram "zdo-inferred-handler-return" handlerReturn with
  | .success 8 => pure ()
  | _ => failTest "zdo did not forward a return from a catch handler"

  let mutableState := zdo
    let mut value := 0
    try
      value := 1
      let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
      pure ()
    catch _ =>
      value := 2
      pure ()
    pure value
  let mutableState : Z Unit Empty Nat := mutableState
  match <- runProgram "zdo-inferred-catch-state" mutableState with
  | .success 2 => pure ()
  | _ => failTest "zdo did not forward mutable state through catch"

  let loopControl := zdo
    let mut total := 0
    for value in [1, 2, 3] do
      try
        if value == 1 then continue
        if value == 3 then break
        total := total + value
        pure ()
      catch _ =>
        pure ()
    pure total
  let loopControl : Z Unit Empty Nat := loopControl
  match <- runProgram "zdo-inferred-catch-loop-control" loopControl with
  | .success 2 => pure ()
  | _ => failTest "zdo did not forward loop control through catch"

  let patternCatch := zdo
    try
      let _ : Nat <-
        (Z.fail (some "handled") : Z Unit (Option String) Nat)
      pure 0
    catch
      | some text => pure text.length
      | none => pure 0
  let patternCatch : Z Unit Empty Nat := patternCatch
  match <- runProgram "zdo-inferred-pattern-catch" patternCatch with
  | .success 7 => pure ()
  | _ => failTest "zdo did not elaborate a catch pattern"

def testZDoInferredMultipleCatch : IO Unit := do
  let orderedChain := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    catch _ =>
      let _ : Nat ← (Z.fail 5 : Z Unit Nat Nat)
      pure 1
    catch number =>
      pure (number + 1)
  let orderedChain : Z Unit Empty Nat := orderedChain
  match ← runProgram "zdo-inferred-multiple-catch-order" orderedChain with
  | .success 6 => pure ()
  | _ => failTest "a later catch did not handle an earlier handler error"

  let secondCalled ← IO.mkRef false
  let firstRecovery := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    catch _ =>
      pure 7
    catch _ =>
      let _ ← Z.succeed <| secondCalled.set true
      pure 8
  let firstRecovery : Z Unit Empty Nat := firstRecovery
  match ← runProgram "zdo-inferred-multiple-catch-skip" firstRecovery with
  | .success 7 => pure ()
  | _ => failTest "a later catch changed an earlier successful recovery"
  assertTrue "a later catch ran after an earlier successful recovery"
    (!(← secondCalled.get))

  let escapingError := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    catch _ =>
      let _ : Nat ← (Z.fail 5 : Z Unit Nat Nat)
      pure 1
    catch _ =>
      let _ : Nat ← (Z.fail true : Z Unit Bool Nat)
      pure 2
  let escapingError : Z Unit Bool Nat := escapingError
  match ← runProgram "zdo-inferred-multiple-catch-error" escapingError with
  | .failure (.fail true) => pure ()
  | _ => failTest "zdo did not expose the last catch handler error"

  let environmentChain := zdo
    try
      let _ ← Z.environment Nat
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure false
    catch _ =>
      let _ ← Z.environment String
      let _ : Nat ← (Z.fail 5 : Z Unit Nat Nat)
      pure false
    catch _ =>
      Z.environment Bool
  let environmentChain : Z (Bool × Nat × String) Empty Bool :=
    environmentChain
  let environmentChainClosed : Z Unit Empty Bool :=
    environmentChain.provideEnvironment (true, 42, "handler")
  match ← runProgram "zdo-inferred-multiple-catch-environment"
      environmentChainClosed with
  | .success true => pure ()
  | _ => failTest "zdo did not combine multiple catch environments"

  let caughtThenFinalized := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    catch _ =>
      let _ : Nat ← (Z.fail 5 : Z Unit Nat Nat)
      pure 1
    catch number =>
      pure (number + 1)
    finally
      Z.attempt (pure ())
  let caughtThenFinalized : Z Unit IO.Error Nat := caughtThenFinalized
  match ← runProgram "zdo-inferred-multiple-catch-finally"
      caughtThenFinalized with
  | .success 6 => pure ()
  | _ => failTest "zdo did not compose multiple catches with finally"

def testZDoInferredFinally : IO Unit := do
  let environmentProgram := zdo
    try
      Z.environment Nat
    finally
      (Z.environment String).map (fun _ => ())
  let environmentProgram : Z (Nat × String) Empty Nat := environmentProgram
  let environmentProgramClosed : Z Unit Empty Nat :=
    environmentProgram.provideEnvironment (42, "finalizer")
  match ← runProgram "zdo-inferred-finally-environment"
      environmentProgramClosed with
  | .success 42 => pure ()
  | _ => failTest "zdo did not infer the finalizer environment"

  let successEvents ← IO.mkRef ([] : List String)
  let successProgram := zdo
    try
      let _ ← Z.succeed <| successEvents.modify (fun events =>
        events ++ ["body"])
      pure 1
    finally
      Z.succeed <| successEvents.modify (fun events =>
        events ++ ["finalizer"])
  let successProgram : Z Unit Empty Nat := successProgram
  match ← runProgram "zdo-inferred-finally-success" successProgram with
  | .success 1 => pure ()
  | _ => failTest "zdo finally changed a successful value"
  assertTrue "zdo finally did not run after success"
    ((← successEvents.get) == ["body", "finalizer"])

  let failureEvents ← IO.mkRef ([] : List String)
  let bodyFailure := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    finally
      Z.succeed <| failureEvents.modify (fun events =>
        events ++ ["finalizer"])
  let bodyFailure : Z Unit String Nat := bodyFailure
  match ← runProgram "zdo-inferred-finally-body-failure" bodyFailure with
  | .failure (.fail "body") => pure ()
  | _ => failTest "zdo finally did not preserve the body failure"
  assertTrue "zdo finally did not run after failure"
    ((← failureEvents.get) == ["finalizer"])

  let finalizerFailure := zdo
    try
      let _ : Nat ← (Z.fail "body" : Z Unit String Nat)
      pure 0
    finally
      let _ : Unit ← (Z.fail 5 : Z Unit Nat Unit)
      pure ()
  let finalizerFailure : Z Unit (Nat ⊕ String) Nat := finalizerFailure
  match ← runProgram "zdo-inferred-finalizer-failure" finalizerFailure with
  | .failure (.fail (.inl 5)) => pure ()
  | _ => failTest "the finalizer failure did not take precedence"

  let caughtThenFinalized := zdo
    try
      let _ : Nat ← (Z.fail "handled" : Z Unit String Nat)
      pure 0
    catch _ =>
      pure 1
    finally
      Z.attempt (pure ())
  let caughtThenFinalized : Z Unit IO.Error Nat := caughtThenFinalized
  match ← runProgram "zdo-inferred-catch-finally" caughtThenFinalized with
  | .success 1 => pure ()
  | _ => failTest "zdo finally retained an error handled by catch"

  let returnEvents ← IO.mkRef ([] : List String)
  let returnProgram (stop : Bool) := zdo
    try
      if stop then return 7
      pure ()
    finally
      Z.succeed <| returnEvents.modify (fun events =>
        events ++ ["finalizer"])
    pure 9
  let returnProgram : Z Unit Empty Nat := returnProgram true
  match ← runProgram "zdo-inferred-finally-return" returnProgram with
  | .success 7 => pure ()
  | _ => failTest "zdo finally did not forward an early return"
  assertTrue "zdo finally did not run before an early return"
    ((← returnEvents.get) == ["finalizer"])

  let finalizerCount ← IO.mkRef 0
  let loopProgram := zdo
    let mut total := 0
    for value in [1, 2, 3] do
      try
        if value == 1 then continue
        if value == 3 then break
        total := total + value
        pure ()
      finally
        Z.succeed <| finalizerCount.modify (fun count => count + 1)
    pure total
  let loopProgram : Z Unit Empty Nat := loopProgram
  match ← runProgram "zdo-inferred-finally-loop-control" loopProgram with
  | .success 2 => pure ()
  | _ => failTest "zdo finally did not forward loop control"
  assertTrue "zdo finally did not run before loop control resumed"
    ((← finalizerCount.get) == 3)

def suite : List (String × IO Unit) := [
  ("testFinalizerFailure", testFinalizerFailure),
  ("testIOErrorCatch", testIOErrorCatch),
  ("testExitEquality", testExitEquality),
  ("testCompleteBeforeTask", testCompleteBeforeTask),
  ("testAsyncRegistrationFailure", testAsyncRegistrationFailure),
  ("testAsyncImmediateResumeWins", testAsyncImmediateResumeWins),
  ("testAsyncDelayedResume", testAsyncDelayedResume),
  ("testAsyncInterruption", testAsyncInterruption),
  ("testAsyncInterruptCanceler", testAsyncInterruptCanceler),
  ("testAsyncInterruptCancelerFailure", testAsyncInterruptCancelerFailure),
  ("testFromAsyncResult", testFromAsyncResult),
  ("testFromAsyncInterruption", testFromAsyncInterruption),
  ("testAsyncSleepCompletion", testAsyncSleepCompletion),
  ("testAsyncSleepInterruption", testAsyncSleepInterruption),
  ("testAsyncSleepConcurrency", testAsyncSleepConcurrency),
  ("testAsyncResumeDefect", testAsyncResumeDefect),
  ("testAsyncInterruptResumeDefect", testAsyncInterruptResumeDefect),
  ("testUnsafeRunSyncHasNoPollingDelay", testUnsafeRunSyncHasNoPollingDelay),
  ("testInterpreterLoggingIsDisabledByDefault",
    testInterpreterLoggingIsDisabledByDefault),
  ("testFiberIdsAreUnique", testFiberIdsAreUnique),
  ("testHEIOAsyncInterruption", testHEIOAsyncInterruption),
  ("testPreInterruptedLayerBuild", testPreInterruptedLayerBuild),
  ("testParallelLayerInterruption", testParallelLayerInterruption),
  ("testObserverRace", testObserverRace),
  ("testGraphVizEscaping", testGraphVizEscaping),
  ("testGraphVizDiagramEvents", testGraphVizDiagramEvents),
  ("testChildDiagramLifetime", testChildDiagramLifetime),
  ("testHighUniverseEnvironment", testHighUniverseEnvironment),
  ("testHighUniverseLayerFailure", testHighUniverseLayerFailure),
  ("testLayerFromZ", testLayerFromZ),
  ("testLayerReleaseOrder", testLayerReleaseOrder),
  ("testLayerReleaseAfterProgramFailure", testLayerReleaseAfterProgramFailure),
  ("testLayerCleanupAfterAcquisitionFailure",
    testLayerCleanupAfterAcquisitionFailure),
  ("testLayerReleaseFailure", testLayerReleaseFailure),
  ("testHighUniverseLayerRelease", testHighUniverseLayerRelease),
  ("testHighUniverseLayerSharing", testHighUniverseLayerSharing),
  ("testHighUniverseParallelLayers", testHighUniverseParallelLayers),
  ("testParallelLayerOverlap", testParallelLayerOverlap),
  ("testParallelLayerFailureCleanup", testParallelLayerFailureCleanup),
  ("testParallelLayerFailureCancelsSibling",
    testParallelLayerFailureCancelsSibling),
  ("testAcquireReleaseZLayer", testAcquireReleaseZLayer),
  ("testGithubIssueSync", testGithubIssueSync),
  ("testZDoEnvironmentComposition", testZDoEnvironmentComposition),
  ("testZDoControlFlow", testZDoControlFlow),
  ("testZDoInferredEnvironment", testZDoInferredEnvironment),
  ("testZDoInferredControlFlow", testZDoInferredControlFlow),
  ("testErrorChannelJoin", testErrorChannelJoin),
  ("testZDoInferredErrors", testZDoInferredErrors),
  ("testZDoInferredCatch", testZDoInferredCatch),
  ("testZDoInferredMultipleCatch", testZDoInferredMultipleCatch),
  ("testZDoInferredFinally", testZDoInferredFinally),
  ("stableServiceKeysDemo", StableServiceKeys.demo),
  ("todoReport", TodoReport.test)
]
    |>.append regressionTests
    |>.append provideRegressionTests
    |>.append keyedRegressionTests
    |>.append heioPrimitiveTests
    |>.append primitiveTests
    |>.append scopeTests

/--
Run the whole suite, or only the tests named on the command line:
`lake exe tests testAsyncInterruptionRunsFinalizer`.
-/
def main (args : List String) : IO Unit := do
  let selected :=
    if args.isEmpty then
      suite
    else
      suite.filter fun (name, _) => args.contains name
  if selected.isEmpty then
    throw (IO.userError s!"no test matched {args}")
  for (name, test) in selected do
    try
      test
    catch error =>
      throw (IO.userError s!"{name}: {error}")
  IO.println s!"All {selected.length} regression tests passed."
