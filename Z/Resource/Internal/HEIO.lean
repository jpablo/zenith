import Z.Cause
import Init.System.Promise

/-!
`HEIO` has the real-world sequencing model of Lean's `IO`, but its error and
success values can live in independent universes.

Standard `IO` actions can enter `HEIO` through `liftIO`. A computation can
return to standard `IO` only after its final value is low-universe.

An interruptible run carries an explicit signal. Interruption is separate from
the typed error channel. `asyncInterrupt` connects the signal to active work,
and `ensuring` masks the signal while its finalizer runs.
-/

/-- The low-level runtime representation of an `HEIO` result. -/
inductive HEIO.Out.{ue, ua}
    (E : Type ue)
    (A : Type ua) : Type (max ue ua) where
  | ok : A -> Void IO.RealWorld -> HEIO.Out E A
  | error : E -> Void IO.RealWorld -> HEIO.Out E A
  | interrupted : Void IO.RealWorld -> HEIO.Out E A

/-- A world-free completed result from an `HEIO` action. -/
inductive HEIO.Result.{ue, ua}
    (E : Type ue)
    (A : Type ua) : Type (max ue ua) where
  | ok : A -> HEIO.Result E A
  | error : E -> HEIO.Result E A
  | interrupted : HEIO.Result E A

/-- A shared interruption request and its registered cancellation actions. -/
structure HEIO.Interruption where
  private mk ::
  interrupted : IO.Ref Bool
  nextHandlerId : IO.Ref Nat
  handlers : IO.Ref (List (Nat × IO Unit))

/-- Runtime context that controls interruption for an `HEIO` action. -/
structure HEIO.Runtime where
  interruption : Option HEIO.Interruption := none
  interruptible : Bool := true

/-- An effect whose error and successful value may live in separate universes. -/
def HEIO.{ue, ua}
    (E : Type ue)
    (A : Type ua) : Type (max ue ua) :=
  HEIO.Runtime -> Void IO.RealWorld -> HEIO.Out E A

namespace HEIO

private def Interruption.newBase : BaseIO Interruption := do
  return .mk (← IO.mkRef false) (← IO.mkRef 0) (← IO.mkRef [])

/-- Allocate a new interruption signal. -/
def Interruption.new : IO Interruption :=
  fun world =>
    match Interruption.newBase world with
    | .mk interruption world => .ok interruption world

/-- Report whether interruption has been requested. -/
def Interruption.isRequested (self : Interruption) : BaseIO Bool :=
  self.interrupted.get

private def Interruption.freshHandlerId
    (self : Interruption) : BaseIO Nat :=
  self.nextHandlerId.modifyGet fun current => (current, current + 1)

private def Interruption.addHandler
    (self : Interruption)
    (handlerId : Nat)
    (handler : IO Unit) : BaseIO Unit :=
  self.handlers.modify fun current => (handlerId, handler) :: current

private def Interruption.removeHandler
    (self : Interruption)
    (handlerId : Nat) : BaseIO Unit :=
  self.handlers.modify fun current =>
    current.filter fun (currentId, _) => currentId != handlerId

private def Interruption.requestBase (self : Interruption) : BaseIO Unit := do
  let first ← self.interrupted.modifyGet fun wasInterrupted =>
    (!wasInterrupted, true)
  if first then
    let handlers ← self.handlers.get
    let tasks ← handlers.mapM fun (_, handler) => IO.asTask handler
    for task in tasks do
      let _ ← IO.wait task
      pure ()

/-- Request interruption and run each registered cancellation action once. -/
def Interruption.request (self : Interruption) : IO Unit :=
  fun world =>
    match self.requestBase world with
    | .mk _ world => .ok () world

private structure ParentRegistration where
  parent : Interruption
  handlerId : Nat

private def Runtime.installChild
    (self : Runtime)
    (child : Interruption) : BaseIO (Option ParentRegistration) := do
  if !self.interruptible then
    return none
  match self.interruption with
  | none => return none
  | some parent =>
      let handlerId ← parent.freshHandlerId
      let requestChild : IO Unit := do child.request
      parent.addHandler handlerId requestChild
      if ← parent.isRequested then
        child.requestBase
      return some { parent, handlerId }

private def newInterruption : HEIO E Interruption :=
  fun _ world =>
    match Interruption.newBase world with
    | .mk interruption world => .ok interruption world

private def installChild
    (child : Interruption) : HEIO E (Option ParentRegistration) :=
  fun runtime world =>
    match runtime.installChild child world with
    | .mk registration world => .ok registration world

private def removeParent
    (registration : Option ParentRegistration) : HEIO E Unit :=
  fun _ world =>
    match registration with
    | none => .ok () world
    | some registration =>
        match registration.parent.removeHandler
            registration.handlerId world with
        | .mk _ world => .ok () world

private def locallyInterruption
    (interruption : Interruption)
    (action : HEIO E A) : HEIO E A :=
  fun runtime => action { runtime with interruption := some interruption }

private def Runtime.shouldInterrupt (self : Runtime) : BaseIO Bool := do
  if !self.interruptible then
    return false
  match self.interruption with
  | none => return false
  | some interruption => interruption.isRequested

/-- A mutable reference that can contain a value from any universe. -/
structure Ref.{u} (A : Type u) : Type u where
  private mk ::
  ref : ST.RefPointed.type
  h : Nonempty A

private inductive STOut.{u} (A : Type u) : Type u where
  | mk : A -> Void IO.RealWorld -> STOut A

private def HST.{u} (A : Type u) : Type u :=
  Void IO.RealWorld -> STOut A

private noncomputable def inhabitedFromRef
    (reference : Ref A) : HST A :=
  let _ : Inhabited A := Classical.inhabited_of_nonempty reference.h
  fun world => .mk default world

private def liftHST (action : HST A) : HEIO E A :=
  fun _ world =>
    match action world with
    | .mk value world => .ok value world

namespace Prim

@[extern "lean_st_mk_ref"]
opaque mkRef
    (value : A) : HST (Ref A) :=
  fun world => .mk {
      ref := Classical.choice ST.RefPointed.property
      h := Nonempty.intro value
    } world

@[extern "lean_st_ref_get"]
opaque Ref.get
    (reference : @& HEIO.Ref A) : HST A :=
  inhabitedFromRef reference

@[extern "lean_st_ref_set"]
opaque Ref.set
    (reference : @& HEIO.Ref A)
    (value : A) : HST Unit :=
  fun world => .mk () world

@[extern "lean_st_ref_swap"]
opaque Ref.swap
    (reference : @& HEIO.Ref A)
    (value : A) : HST A :=
  inhabitedFromRef reference

@[extern "lean_io_as_task"]
opaque asTask
    (action : HST A)
    (priority := Task.Priority.default) : HST (Task A) :=
  fun world =>
    match action world with
    | .mk value world => .mk (Task.pure value) world

@[extern "lean_io_wait"]
opaque wait
    (task : Task A) : HST A :=
  fun world => .mk task.get world

end Prim

/-- Create a universe-polymorphic reference. -/
def mkRef (value : A) : HEIO E (Ref A) :=
  liftHST (Prim.mkRef value)

/-- Read a universe-polymorphic reference. -/
def Ref.get (reference : Ref A) : HEIO E A :=
  liftHST (Prim.Ref.get reference)

/-- Replace the value in a universe-polymorphic reference. -/
def Ref.set (reference : Ref A) (value : A) : HEIO E Unit :=
  liftHST (Prim.Ref.set reference value)

/-- Replace and return the value in a universe-polymorphic reference. -/
def Ref.swap (reference : Ref A) (value : A) : HEIO E A :=
  liftHST (Prim.Ref.swap reference value)

private def toHSTResult
    (action : HEIO E A)
    (runtime : Runtime) : HST (Result E A) :=
  fun world =>
    match action runtime world with
    | .ok value world => .mk (.ok value) world
    | .error error world => .mk (.error error) world
    | .interrupted world => .mk .interrupted world

/-- Start an `HEIO` action in a runtime task. -/
def fork
    (action : HEIO E A)
    (priority := Task.Priority.default) :
    HEIO E (Task (Result E A)) :=
  fun runtime world =>
    match Prim.asTask (toHSTResult action runtime) priority world with
    | .mk task world => .ok task world

/-- Wait for a task that was created by `HEIO.fork`. -/
def wait (task : Task A) : HEIO E A :=
  liftHST (Prim.wait task)

/-- Create a successful `HEIO` action. -/
def pure (value : A) : HEIO E A :=
  fun _ world => .ok value world

/-- End an `HEIO` action with interruption. -/
def interrupt : HEIO E A :=
  fun _ world => .interrupted world

/-- End with interruption when the current runtime has a pending request. -/
def checkInterrupted : HEIO E Unit :=
  fun runtime world =>
    match runtime.shouldInterrupt world with
    | .mk true world => .interrupted world
    | .mk false world => .ok () world

/-- Run `next` after a successful result. -/
def bind
    (self : HEIO E A)
    (next : A -> HEIO E B) : HEIO E B :=
  fun runtime world =>
    match self runtime world with
    | .ok value world => next value runtime world
    | .error error world => .error error world
    | .interrupted world => .interrupted world

/-- Transform a successful result. -/
def map
    (f : A -> B)
    (self : HEIO E A) : HEIO E B :=
  bind self fun value => pure (f value)

/-- End an `HEIO` action with a typed error. -/
def throw (error : E) : HEIO E A :=
  fun _ world => .error error world

/-- Recover from a typed error while preserving interruption. -/
def tryCatch
    (self : HEIO E A)
    (handler : E -> HEIO E A) : HEIO E A :=
  fun runtime world =>
    match self runtime world with
    | .ok value world => .ok value world
    | .error error world => handler error runtime world
    | .interrupted world => .interrupted world

/-- Transform typed errors while preserving success and interruption. -/
def mapError
    (f : E -> E₁)
    (self : HEIO E A) : HEIO E₁ A :=
  fun runtime world =>
    match self runtime world with
    | .ok value world => .ok value world
    | .error error world => .error (f error) world
    | .interrupted world => .interrupted world

/-- Handle both outcomes and permit the result universe and error type to change. -/
def fold
    (self : HEIO E A)
    (failure : E -> HEIO E₁ B)
    (success : A -> HEIO E₁ B) : HEIO E₁ B :=
  fun runtime world =>
    match self runtime world with
    | .ok value world => success value runtime world
    | .error error world => failure error runtime world
    | .interrupted world => .interrupted world

/-- Handle success, typed failure, and interruption. -/
def foldAll
    (self : HEIO E A)
    (failure : E -> HEIO E₁ B)
    (interrupted : HEIO E₁ B)
    (success : A -> HEIO E₁ B) : HEIO E₁ B :=
  fun runtime world =>
    match self runtime world with
    | .ok value world => success value runtime world
    | .error error world => failure error runtime world
    | .interrupted world => interrupted runtime world

/--
Run `finalizer` after either outcome. If both actions fail, return the
finalizer failure.
-/
def ensuring
    (self : HEIO E A)
    (finalizer : HEIO E Unit) : HEIO E A :=
  fun runtime world =>
    match self runtime world with
    | .ok value world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .ok value world
        | .error error world => .error error world
        | .interrupted world => .interrupted world
    | .error originalError world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .error originalError world
        | .error finalizerError world => .error finalizerError world
        | .interrupted world => .interrupted world
    | .interrupted world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .interrupted world
        | .error finalizerError world => .error finalizerError world
        | .interrupted world => .interrupted world

/--
Run a cause-valued finalizer and preserve failures from both actions.
The cause tree records the body outcome before the finalizer outcome.
-/
def ensuringCause
    (self : HEIO (Cause E) A)
    (finalizer : HEIO (Cause E) Unit) : HEIO (Cause E) A :=
  fun runtime world =>
    match self runtime world with
    | .ok value world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .ok value world
        | .error cause world => .error cause world
        | .interrupted world => .interrupted world
    | .error originalCause world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .error originalCause world
        | .error finalizerCause world =>
            .error (.sequential originalCause finalizerCause) world
        | .interrupted world =>
            .error (.sequential originalCause .interrupt) world
    | .interrupted world =>
        match finalizer { runtime with interruptible := false } world with
        | .ok _ world => .interrupted world
        | .error finalizerCause world =>
            .error (.sequential .interrupt finalizerCause) world
        | .interrupted world =>
            .error (.sequential .interrupt .interrupt) world
/--
Run an action with a child interruption scope. Parent interruption reaches the
child, but requesting the child does not interrupt the parent.
-/
def withChildInterruption
    (use : Interruption -> HEIO E A) : HEIO E A :=
  bind newInterruption fun child =>
    bind (installChild child) fun registration =>
      (locallyInterruption child (use child)).ensuring
        (removeParent registration)

instance : Monad (HEIO E) where
  pure := pure
  bind := bind

instance : MonadExceptOf E (HEIO E) where
  throw := throw
  tryCatch := tryCatch

/-- Run an exception-free standard action inside a selected `HEIO` universe. -/
def liftBaseIO.{u}
    (action : BaseIO A) : HEIO E (ULift.{u} A) :=
  fun _ world =>
    match action world with
    | .mk value world => .ok (ULift.up value) world

/-- Run a standard `IO` action inside a selected `HEIO` universe. -/
def liftIO.{u}
    (mapError : IO.Error -> E)
    (action : IO A) : HEIO E (ULift.{u} A) :=
  fun _ world =>
    match action world with
    | .ok value world => .ok (ULift.up value) world
    | .error error world => .error (mapError error) world

/--
Register a low-universe asynchronous action and its cancellation action. The
result is lifted into the selected universe. Interruption wins only after the
cancellation action completes.

The cancellation action runs from the interruption handler or from the
registering action once it has returned, never from the callback, so a
registration that resumes its callback synchronously cannot block.
-/
def asyncInterrupt.{u}
    {E A : Type}
    (mapError : IO.Error -> E)
    (register : (Except E A -> IO Unit) -> IO (IO Unit)) :
    HEIO E (ULift.{u} A) :=
  fun runtime world =>
    let action : IO (Result E A) := do
      let interruption : Interruption ←
        if runtime.interruptible then
          match runtime.interruption with
          | some interruption => (return interruption : IO Interruption)
          | none => Interruption.new
        else
          Interruption.new
      let completed ← IO.mkRef false
      let completion ← IO.Promise.new (α := Option (Result E A))
      let cancelReady ← IO.Promise.new (α := IO Unit)
      let handlerId ← interruption.freshHandlerId
      let claim : BaseIO Bool :=
        completed.modifyGet fun wasCompleted => (!wasCompleted, true)
      let complete (result : Result E A) : IO Unit := do
        interruption.removeHandler handlerId
        completion.resolve (some result)
      let cancel : IO Unit := do
        if ← interruption.isRequested then
          if ← claim then
            match ← IO.wait cancelReady.result? with
            | some cancelAction =>
                try
                  cancelAction
                  complete .interrupted
                catch error =>
                  complete (.error (mapError error))
            | none => complete .interrupted
      let callback (result : Except E A) : IO Unit := do
        unless ← interruption.isRequested do
          if ← claim then
            complete <| match result with
              | .ok value => .ok value
              | .error error => .error error
      interruption.addHandler handlerId cancel
      if ← interruption.isRequested then
        cancelReady.resolve (return ())
        cancel
      else
        try
          let cancelAction ← register callback
          cancelReady.resolve cancelAction
          if ← interruption.isRequested then
            cancel
        catch error =>
          cancelReady.resolve (return ())
          if ← interruption.isRequested then
            cancel
          else if ← claim then
            complete (.error (mapError error))
      match ← IO.wait completion.result? with
      | some (some result) => return result
      | _ => return .error (mapError (IO.userError
          "the HEIO asynchronous action did not return a result"))
    match action world with
    | .ok (.ok value) world => .ok (ULift.up value) world
    | .ok (.error error) world => .error error world
    | .ok .interrupted world => .interrupted world
    | .error error world => .error (mapError error) world

/--
Return to standard `IO` after the final result has returned to `Type`.
-/
def toIO.{u}
    (mapError : E -> IO.Error)
    (action : HEIO E (ULift.{u} A)) : IO A :=
  fun world =>
    match action {} world with
    | .ok value world => .ok value.down world
    | .error error world => .error (mapError error) world
    | .interrupted world =>
        .error (IO.userError "an uninterruptible HEIO action was interrupted") world

/--
Return both success and failure as a standard low-universe `IO` value.
-/
def toIOResult.{u}
    (action : HEIO E (ULift.{u} A)) : IO (Except E A) :=
  fun world =>
    match action {} world with
    | .ok value world => .ok (.ok value.down) world
    | .error error world => .ok (.error error) world
    | .interrupted world =>
        .error (IO.userError "an uninterruptible HEIO action was interrupted") world

/-- Run an `HEIO` action with an external interruption signal. -/
def toIOResultInterruptible.{u}
    (interruption : Interruption)
    (onInterrupt : E)
    (action : HEIO E (ULift.{u} A)) : IO (Except E A) :=
  fun world =>
    match action { interruption := some interruption } world with
    | .ok value world => .ok (.ok value.down) world
    | .error error world => .ok (.error error) world
    | .interrupted world => .ok (.error onInterrupt) world

end HEIO
