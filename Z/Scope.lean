import Z.Interpreter

/-!
Dynamic resource scopes.

`Scope` owns finalizers for resources acquired by `Z.acquireRelease`.
`Z.scoped` creates a scope, supplies it to an effect, and closes it on every
exit path.
-/

private abbrev ScopeFinalizer := IO (Option (Cause Empty))

private inductive ScopeState where
  | open (finalizers : List ScopeFinalizer)
  | closed

/-- A dynamic lifetime that owns registered resource finalizers. -/
structure Scope where
  private state : IO.Ref ScopeState

namespace Scope

private structure Closeable where
  scope : Scope

private def runFinalizerSafely
    (finalizer : ScopeFinalizer) : IO (Option (Cause Empty)) := do
  try
    finalizer
  catch error =>
    pure (some (.die error))

private def appendSequential
    (previous current : Option (Cause Empty)) : Option (Cause Empty) :=
  match previous, current with
  | none, cause | cause, none => cause
  | some left, some right => some (.sequential left right)

private def runFinalizers :
    List ScopeFinalizer -> Option (Cause Empty) -> IO (Option (Cause Empty))
  | [], combined => pure combined
  | finalizer :: rest, combined => do
      let current ← runFinalizerSafely finalizer
      runFinalizers rest (appendSequential combined current)

private def make : IO Closeable := do
  pure { scope := { state := ← IO.mkRef (.open []) } }

/--
Register a finalizer. Registration prepends it so closure uses reverse order.
If closure won the race, run the finalizer immediately.
-/
private def addFinalizerIO
    (self : Scope)
    (finalizer : ScopeFinalizer) : IO (Option (Cause Empty)) := do
  let runNow ← self.state.modifyGet fun
    | .open finalizers => (false, .open (finalizer :: finalizers))
    | .closed => (true, .closed)
  if runNow then runFinalizerSafely finalizer else pure none

/-- Close a scope exactly once and run all its finalizers. -/
private def Closeable.closeIO
    (self : Closeable) : IO (Option (Cause Empty)) := do
  let finalizers ← self.scope.state.modifyGet fun
    | .open finalizers => (finalizers, .closed)
    | .closed => ([], .closed)
  runFinalizers finalizers none

/--
Remove one `Scope` requirement and rebuild the complete scoped environment.
This lets `Z.scoped` infer its remaining environment.
-/
class Remove
    (Scoped : Type u)
    (Rest : outParam (Type v)) where
  insert : Rest -> Scope -> Scoped

namespace Remove

instance (priority := 1000) : Remove Scope Unit where
  insert _ scope := scope

instance (priority := 900) : Remove (Scope × Tail) Tail where
  insert tail scope := (scope, tail)

instance (priority := 800) : Remove (Head × Scope) Head where
  insert head scope := (head, scope)

instance (priority := 100) [tail : Remove Tail Rest] :
    Remove (Head × Tail) (Head × Rest) where
  insert environment scope :=
    match environment with
    | (head, rest) => (head, tail.insert rest scope)

end Remove

private def runFinalizer
    (finalizer : Z Unit Empty Unit) : ScopeFinalizer := do
  match ← Z.unsafeRunSync finalizer "scope-finalizer" with
  | .success () => pure none
  | .failure cause => pure (some cause)

private def finalizerResult
    (action : IO (Option (Cause Empty))) : ZCore Unit Empty Unit :=
  ZCore.flatMap (ZCore.succeed' action) fun
    | none => ZCore.succeedNow' ()
    | some cause => ZCore.done' (.failure cause)

/-- Register a Zenith finalizer in this scope. -/
def addFinalizer
    (self : Scope)
    (finalizer : Z R Empty Unit) : Z R Empty Unit :=
  Z.fromCore fun environment =>
    finalizerResult <|
      self.addFinalizerIO <|
        runFinalizer (finalizer.provideEnvironment environment)

private def Closeable.close
    (self : Closeable) : Z R Empty Unit :=
  Z.fromCore fun _ => finalizerResult self.closeIO

end Scope

namespace Z

/-- Register a finalizer in the current scope. -/
def addFinalizer
    [meet : Environment.Meet R Scope Scoped]
    (finalizer : Z R Empty Unit) : Z Scoped Empty Unit :=
  Z.fromCore fun environment =>
    let finalizerEnvironment := meet.left environment
    let scope := meet.right environment
    let closedFinalizer :=
      finalizer.provideEnvironment finalizerEnvironment
    Scope.finalizerResult <|
      scope.addFinalizerIO <| Scope.runFinalizer closedFinalizer

/--
Acquire a resource and register its release action in the current scope.
Acquisition and registration are one uninterruptible region.
-/
def acquireRelease
    [meet : Environment.Meet R Scope Scoped]
    (acquire : Z R E A)
    (release : A -> Z R Empty Unit) : Z Scoped E A :=
  let masked : Z Scoped E A :=
    (acquire.contramap meet.left).flatMap fun value =>
      (Z.addFinalizer (meet := meet) (release value))
        |>.mapFailure Empty.elim
        |>.map fun _ => value
  masked.uninterruptible

/--
Fork `self` and attach the child lifetime to the current scope.
Scope closure interrupts the child and waits for its final exit.
-/
def forkScoped
    [meet : Environment.Meet R Scope Scoped]
    (self : Z R E A)
    (name : String := "scoped") : Z Scoped Empty (Fiber E A) :=
  Z.acquireRelease (meet := meet) (self.fork name) fun fiber =>
    Z.internal.succeed do
      fiber.requestInterrupt
      let _ ← fiber.await
      pure ()

/--
Create a fresh scope, supply it to `effect`, and close it on every exit path.
-/
def «scoped»
    [remove : Scope.Remove Scoped Rest]
    (effect : Z Scoped E A) : Z Rest E A :=
  Z.withIO Scope.make fun closeable =>
    let body : Z Rest E A :=
      effect.contramap fun environment =>
        remove.insert environment closeable.scope
    body.ensuring closeable.close

end Z
