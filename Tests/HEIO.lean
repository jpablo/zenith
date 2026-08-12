import Tests.Support

/-!
Direct tests for the `HEIO` runtime primitives.

Most production uses reach these primitives through `Layer` or the Zenith
interpreter. These tests keep the primitive result-channel and interruption
rules visible when those higher-level implementations change.
-/

private def runHEIO {E A : Type} (action : HEIO E A) : IO (Except E A) :=
  HEIO.toIOResult
    (HEIO.map (ULift.up : A → ULift.{0} A) action)

private def runHEIOInterruptible {E A : Type}
    (interruption : HEIO.Interruption)
    (onInterrupt : E)
    (action : HEIO E A) : IO (Except E A) :=
  HEIO.toIOResultInterruptible interruption onInterrupt
    (HEIO.map (ULift.up : A → ULift.{0} A) action)

private def recordHEIO
    (events : IO.Ref (List String))
    (event : String) : HEIO String Unit :=
  HEIO.map (fun _ => ()) <|
    HEIO.liftIO.{0} (fun error => toString error) <|
      events.modify (fun current => current ++ [event])

def testHEIOResultChannels : IO Unit := do
  match ← runHEIO (HEIO.pure 42 : HEIO String Nat) with
  | .ok 42 => pure ()
  | _ => failTest "HEIO.pure did not return its value"

  match ← runHEIO (HEIO.throw "typed failure" : HEIO String Nat) with
  | .error "typed failure" => pure ()
  | _ => failTest "HEIO.throw did not use the typed error channel"

  let recovered : HEIO String Nat :=
    HEIO.tryCatch (HEIO.throw "recover" : HEIO String Nat) fun error =>
      HEIO.pure error.length
  match ← runHEIO recovered with
  | .ok 7 => pure ()
  | _ => failTest "HEIO.tryCatch did not recover a typed failure"

  let mapped : HEIO Nat Nat :=
    HEIO.mapError String.length
      (HEIO.throw "mapped" : HEIO String Nat)
  match ← runHEIO mapped with
  | .error 6 => pure ()
  | _ => failTest "HEIO.mapError did not map the typed failure"

  let liftedFailure : HEIO String (ULift.{0} Unit) :=
    HEIO.liftIO.{0} (fun error => toString error) <|
      throw (IO.userError "lifted failure")
  match ← HEIO.toIOResult liftedFailure with
  | .error message =>
      assertTrue "HEIO.liftIO lost the IO error message"
        (message.contains "lifted failure")
  | _ => failTest "HEIO.liftIO did not map an IO failure"

def testHEIOFoldSemantics : IO Unit := do
  let success : HEIO String Nat :=
    HEIO.fold (HEIO.pure 4 : HEIO String Nat)
      (fun _ => HEIO.pure 0)
      (fun value => HEIO.pure (value + 1))
  match ← runHEIO success with
  | .ok 5 => pure ()
  | _ => failTest "HEIO.fold did not select the success branch"

  let failure : HEIO String Nat :=
    HEIO.fold (HEIO.throw "bad" : HEIO String Nat)
      (fun error => HEIO.pure error.length)
      (fun _ => HEIO.pure 0)
  match ← runHEIO failure with
  | .ok 3 => pure ()
  | _ => failTest "HEIO.fold did not select the failure branch"

  let signal ← HEIO.Interruption.new
  let unhandled : HEIO String Nat :=
    HEIO.fold (HEIO.interrupt : HEIO String Nat)
      (fun _ => HEIO.pure 0)
      (fun _ => HEIO.pure 1)
  match ← runHEIOInterruptible signal "outer interruption" unhandled with
  | .error "outer interruption" => pure ()
  | _ => failTest "HEIO.fold handled an interruption as a typed result"

  let handled : HEIO String Nat :=
    HEIO.foldAll (HEIO.interrupt : HEIO String Nat)
      (fun _ => HEIO.pure 0)
      (HEIO.pure 9)
      (fun _ => HEIO.pure 1)
  match ← runHEIO handled with
  | .ok 9 => pure ()
  | _ => failTest "HEIO.foldAll did not handle interruption"

def testHEIOEnsuringOutcomes : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let success : HEIO String Nat :=
    (HEIO.pure 1).ensuring (recordHEIO events "success-finalizer")
  match ← runHEIO success with
  | .ok 1 => pure ()
  | _ => failTest "HEIO.ensuring changed a successful result"

  let failure : HEIO String Nat :=
    (HEIO.throw "body" : HEIO String Nat).ensuring
      (recordHEIO events "failure-finalizer")
  match ← runHEIO failure with
  | .error "body" => pure ()
  | _ => failTest "HEIO.ensuring changed the original failure"

  let signal ← HEIO.Interruption.new
  signal.request
  let interruptFinalizer : HEIO String Unit := do
    HEIO.checkInterrupted
    recordHEIO events "interrupt-finalizer"
  let interrupted : HEIO String Nat :=
    (HEIO.interrupt : HEIO String Nat).ensuring interruptFinalizer
  match ← runHEIOInterruptible signal "interrupted" interrupted with
  | .error "interrupted" => pure ()
  | _ => failTest "HEIO.ensuring changed an interrupted result"

  assertTrue "HEIO.ensuring skipped or reordered a finalizer"
    ((← events.get) == [
      "success-finalizer",
      "failure-finalizer",
      "interrupt-finalizer"
    ])

def testHEIOFinalizerPrecedence : IO Unit := do
  let failedBody : HEIO String Nat :=
    (HEIO.throw "body" : HEIO String Nat).ensuring
      (HEIO.throw "finalizer")
  match ← runHEIO failedBody with
  | .error "finalizer" => pure ()
  | _ => failTest "a HEIO finalizer failure did not replace the body failure"

  let interruptedBody : HEIO String Nat :=
    (HEIO.interrupt : HEIO String Nat).ensuring
      (HEIO.throw "finalizer")
  match ← runHEIO interruptedBody with
  | .error "finalizer" => pure ()
  | _ =>
      failTest
        "a HEIO finalizer failure did not replace the interrupted result"

def testHEIOCauseFinalizerComposition : IO Unit := do
  let defect := IO.userError "finalizer defect"
  let failedBody : HEIO (Cause String) Nat :=
    (HEIO.throw (.fail "body") : HEIO (Cause String) Nat)
      |>.ensuringCause (HEIO.throw (.die defect))
  match ← runHEIO failedBody with
  | .error (.sequential (.fail "body") (.die error)) =>
      assertTrue "HEIO changed the finalizer defect" (error == defect)
  | _ => failTest "HEIO did not combine the body and finalizer causes"

  let interruptedBody : HEIO (Cause String) Nat :=
    (HEIO.interrupt : HEIO (Cause String) Nat)
      |>.ensuringCause (HEIO.throw (.fail "finalizer"))
  match ← runHEIO interruptedBody with
  | .error (.sequential .interrupt (.fail "finalizer")) => pure ()
  | _ => failTest "HEIO did not combine interruption and finalizer failure"

def testHEIOReferencesAndTasks : IO Unit := do
  let referenceProgram : HEIO String Nat := do
    let reference ← HEIO.mkRef 1
    let previous ← reference.swap 2
    let current ← reference.get
    reference.set 3
    let latest ← reference.get
    pure (previous * 100 + current * 10 + latest)
  match ← runHEIO referenceProgram with
  | .ok 123 => pure ()
  | _ => failTest "HEIO reference operations returned the wrong values"

  let taskProgram : HEIO String (HEIO.Result String Nat) := do
    let task ← HEIO.fork (HEIO.pure 42 : HEIO String Nat)
    HEIO.wait task
  match ← runHEIO taskProgram with
  | .ok (.ok 42) => pure ()
  | _ => failTest "HEIO.fork and HEIO.wait lost the task result"

def testHEIOInterruptionScopes : IO Unit := do
  let parent ← HEIO.Interruption.new
  parent.request
  let propagated : HEIO String Nat :=
    HEIO.withChildInterruption fun _ => do
      HEIO.checkInterrupted
      pure 1
  match ← runHEIOInterruptible parent "parent interruption" propagated with
  | .error "parent interruption" => pure ()
  | _ => failTest "parent interruption did not reach the HEIO child scope"

  let isolatedParent ← HEIO.Interruption.new
  let isolated : HEIO String Unit :=
    HEIO.withChildInterruption fun child => do
      let _ ← HEIO.liftIO.{0} (fun error => toString error) child.request
      HEIO.foldAll HEIO.checkInterrupted
        (fun error => HEIO.throw error)
        (HEIO.pure ())
        (fun _ => HEIO.throw "the child did not observe its interruption")
  match ← runHEIOInterruptible isolatedParent "parent interruption" isolated with
  | .ok () => pure ()
  | _ => failTest "the child interruption scope did not recover"
  assertTrue "requesting a HEIO child interrupted its parent"
    !(← BaseIO.toIO isolatedParent.isRequested)

def heioPrimitiveTests : List (String × IO Unit) := [
  ("testHEIOResultChannels", testHEIOResultChannels),
  ("testHEIOFoldSemantics", testHEIOFoldSemantics),
  ("testHEIOEnsuringOutcomes", testHEIOEnsuringOutcomes),
  ("testHEIOFinalizerPrecedence", testHEIOFinalizerPrecedence),
  ("testHEIOCauseFinalizerComposition",
    testHEIOCauseFinalizerComposition),
  ("testHEIOReferencesAndTasks", testHEIOReferencesAndTasks),
  ("testHEIOInterruptionScopes", testHEIOInterruptionScopes)
]
