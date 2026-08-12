import Z.Interpreter

/-!
Dynamic resource scopes.

`Scope` owns finalizers for resources acquired by `Z.acquireRelease`.
`Z.scoped` creates a scope, supplies it to an effect, and closes it on every
exit path.
-/

private inductive ScopeState where
  | open (finalizers : List (IO Unit))
  | closed

/-- A dynamic lifetime that owns registered resource finalizers. -/
structure Scope where
  private state : IO.Ref ScopeState

namespace Scope

private structure Closeable where
  scope : Scope

private def runFinalizers :
    List (IO Unit) -> Option IO.Error -> IO Unit
  | [], none => pure ()
  | [], some error => throw error
  | finalizer :: rest, previousError => do
      let finalError ←
        try
          finalizer
          pure previousError
        catch error =>
          pure (some error)
      runFinalizers rest finalError

private def make : IO Closeable := do
  pure { scope := { state := ← IO.mkRef (.open []) } }

/--
Register a finalizer. Registration prepends it so closure uses reverse order.
If closure won the race, run the finalizer immediately.
-/
private def addFinalizerIO
    (self : Scope)
    (finalizer : IO Unit) : IO Unit := do
  let runNow ← self.state.modifyGet fun
    | .open finalizers => (false, .open (finalizer :: finalizers))
    | .closed => (true, .closed)
  if runNow then finalizer

/-- Close a scope exactly once and run all its finalizers. -/
private def Closeable.closeIO (self : Closeable) : IO Unit := do
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

private def runFinalizer (finalizer : Z Unit Empty Unit) : IO Unit := do
  match ← Z.unsafeRunSync finalizer "scope-finalizer" with
  | .success () => pure ()
  | .failure (.fail error) => nomatch error
  | .failure (.die error) => throw error
  | .failure .interrupt =>
      throw (IO.userError "a scope finalizer was interrupted")

/-- Register a Zenith finalizer in this scope. -/
def addFinalizer
    (self : Scope)
    (finalizer : Z R Empty Unit) : Z R Empty Unit :=
  Z.fromCore fun environment =>
    ZCore.succeed' <|
      self.addFinalizerIO <|
        runFinalizer (finalizer.provideEnvironment environment)

private def Closeable.close
    (self : Closeable) : Z R Empty Unit :=
  Z.internal.succeed self.closeIO

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
    ZCore.succeed' <|
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
