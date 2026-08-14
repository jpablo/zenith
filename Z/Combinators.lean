import Z.Core
import Std.Async.Timer

namespace ZCore

  variable (self : _root_.ZCore R E A)

  def nodeId : NodeId :=
    self.metadata.nodeId

  def setNodeId (nodeId : NodeId) : _root_.ZCore R E A :=
    self.updateMetadata fun md => {md with nodeId := nodeId}

  def resetNodeId : _root_.ZCore R E A :=
    self.setNodeId ""

  def label : String :=
    self.metadata.label

  def ensureNodeId (nodeId : NodeId) : _root_.ZCore R E A :=
    if self.metadata.nodeId.isEmpty then
      self.setNodeId nodeId
    else
      self

  def failCause (cause : Cause E) : _root_.ZCore R E A :=
    ZCore.done' <| .failure cause

  /--
  Run `finalizer` after `self` without masking interruption.

  The interpreter uses this to restore its own interrupt-status bookkeeping.
  Masking there would re-enter `setInterruptStatus` for every restore.
  -/
  def ensuringUnmasked
      (finalizer : _root_.ZCore R Empty A₀) : _root_.ZCore R E A :=
    let finalizer := finalizer.withLabel "🏁 finalizer"
    .withLabel (label := "👮‍♀️ ensuring") <|
      self.foldCauseM
        (fun cause =>
          finalizer.foldCauseM
            (fun finalizerCause =>
              ZCore.failCause <|
                .sequential cause (finalizerCause.map impossible))
            (fun _ => .failCause cause))
        (fun value =>
          finalizer.foldCauseM
            (fun finalizerCause =>
              ZCore.failCause (finalizerCause.map impossible))
            (fun _ => .succeedNow' value))

  /--
  Run `finalizer` after `self`, whatever its outcome.

  A finalizer runs uninterruptibly, so an interrupt that arrives while it is
  in progress cannot abandon the rest of the cleanup.
  -/
  def ensuring
      (finalizer : _root_.ZCore R Empty A₀) : _root_.ZCore R E A :=
    self.ensuringUnmasked
      (ZCore.setInterruptStatus finalizer .uninterruptible)

  instance : ToString (_root_.ZCore R E A) :=
    ⟨(·.showHead)⟩

end ZCore

namespace Z

  variable (self : Z R E A)

  /-- Fail with the complete structured cause. -/
  def failCause (cause : Cause E) : Z R E Empty :=
    Z.internal.done <| Exit.failure cause

  namespace internal

  /-- Build a failed effect with a context-selected environment type. -/
  def fail [ToString E] (userError : E): Z R E Empty :=
    (failCause (Cause.fail userError)).withLabel s!"fail ({userError})"

  end internal

  /-- Fail with one typed error. -/
  def fail [ToString E] (userError : E): Z Unit E Empty :=
    internal.fail userError

  /-- Terminate with an untyped `IO.Error` defect. -/
  def die (ioe : IO.Error) : Z R Empty Empty :=
    failCause (Cause.die ioe)

  /-- Lift a typed-error handler so it preserves defects and interruption. -/
  def errorHandlerCause
      (errorHandler : E -> Z R E₁ A₁)
      (cause : Cause E) : Z R E₁ A₁ :=
    match cause.failureOrCause with
    | .inl error => errorHandler error
    | .inr unhandled => internal.done <| .failure unhandled

  /-- Handle a typed failure or a successful value with effectful handlers. -/
  def foldM (errorHandler : E -> Z R E₁ A₁) (next : A -> Z R E₁ A₁) : Z R E₁ A₁ :=
    (self.foldCauseM (errorHandlerCause errorHandler) next).withLabel "foldM"

  /- ---- Monad instances ------------ -/

  instance : Monad (Z R E) where
    pure a := internal.succeedNow a |>.withLabel "pure"
    bind z f := z.flatMap f |>.withLabel "do"

  instance : ToString (Z R E A) := ⟨fun _ => "Z"⟩
  instance : ToString (URIO R A) := inferInstanceAs (ToString (Z R Empty A))

  instance : Monad ZTask    := inferInstanceAs (Monad (Z Unit IO.Error))
  instance : Monad UIO      := inferInstanceAs (Monad (Z Unit Empty))
  instance : Monad (URIO R) := inferInstanceAs (Monad (Z R Empty))

  instance : MonadExceptOf E (Z R E) where
    throw    := fun e => Z.failCause (R := R) <| .fail e
    tryCatch := fun z errorHandler => Z.foldM z errorHandler pure

  instance : MonadExceptOf IO.Error (Z R Empty) where
    throw    := fun ioe => Z.die (R := R) ioe
    tryCatch := fun z errorHandler =>
      z.foldCauseM
        (fun cause =>
          match cause with
          | .die ioe => errorHandler ioe
          | unhandled =>
              (Z.failCause (R := R) (unhandled.map Empty.elim)).map
                impossible)
        pure

  -- instance : MonadExceptOf IO.Error (Z R (Cause E)) where
  --   throw    := fun ioe => Z.die ioe
  --   tryCatch := fun z errorHandler => 
  --     z.foldM
  --       (fun
  --         | .die ioe => errorHandler ioe
  --         | _ => z
  --       ) 
  --       pure

  /-- A successful effect whose result is `Unit`. -/
  def unit : Z R E Unit :=
    pure ()

  /-- Handle a typed failure or successful value with pure functions. -/
  def fold (errorHandler : E -> A₁) (next : A -> A₁) : Z R E A₁ :=
    self.foldM (errorHandler ∘> pure) (next ∘> pure)

  /-- Handle a complete cause or successful value with pure functions. -/
  def foldCause (errorHandler : Cause E -> A₁) (next : A -> A₁) : Z R Empty A₁ :=
    self.foldCauseM (errorHandler ∘> pure) (next ∘> pure)

  /-- Capture the final success or failure as an `Exit` value. -/
  def exit : Z R Empty (Exit E A) :=
    self.foldCause Exit.failure Exit.success |>.withLabel "exit"

  /-- Handle a typed failure with an effect while preserving defects and interruption. -/
  def catchAll [conversion : A <: A₁]
      (errorHandler : E -> Z R E₁ A₁) : Z R E₁ A₁ :=
    self.foldM errorHandler (pure <| conversion.coe ·) |>.withLabel "catchAll"

  /-- Handle an error with an effect that has a different environment. -/
  def catchAllMeet
      [meet : Environment.Meet R R₁ R₂]
      [conversion : A <: A₁]
      (errorHandler : E -> Z R₁ E₁ A₁) : Z R₂ E₁ A₁ :=
    (self.contramap meet.left).catchAll fun error =>
      (errorHandler error).contramap meet.right

  /-- Handle an `IO.Error` defect with an effect that has a different environment. -/
  def catchIOErrorMeet
      (self : Z R Empty A)
      [meet : Environment.Meet R R₁ R₂]
      [conversion : A <: A₁]
      (errorHandler : IO.Error -> Z R₁ E₁ A₁) : Z R₂ E₁ A₁ :=
    (self.contramap meet.left).foldCauseM
      (fun cause =>
        match cause with
        | .die error => (errorHandler error).contramap meet.right
        | unhandled =>
            (Z.failCause (R := R₂) (unhandled.map Empty.elim)).map
              impossible)
      (fun value => pure (conversion.coe value))

  /-- Run two effects in sequence and combine their successful values. -/
  def zipWith (other : Z R E A₁) (f : A -> A₁ -> A₃) : Z R E A₃ := do
    return f (<- self) (<- other)

  /-- Run two effects in sequence and return both successful values. -/
  def zip (other : Z R E A₁) : Z R E (A × A₁) := do
    self.zipWith other (·, ·) |>.withLabel "zip"

  /-- Expose every failure, defect, and interruption as a typed `Cause`. -/
  def sandbox [ToString E]: Z R (Cause E) A :=
    self.foldCauseM (fun e => fail e) pure

  /-- Convert typed failures into defects with `f`. -/
  def orDieWith (f : E -> IO.Error) : Z R Empty A :=
    self.foldM (fun e => die (R := R) <| f e) pure

  /-- Convert `IO.Error` typed failures into defects. -/
  def orDie (self : Z R IO.Error A): Z R Empty A :=
    self.orDieWith id |>.withLabel "orDie"

  /-- Run `self` once and then repeat it `n` additional times. -/
  def repeatN (n : Nat) (self : Z R E A): Z R E Unit :=
    .withLabel (label := s!"repeatN : {n}") $
    self.flatMap fun _ =>
      if n > 0 then
        repeatN (n - 1) self
      else
        Z.unit

  /-- Convert `some value` to success and `none` to an `IO.Error` failure. -/
  def fromOption (v : Option A): Z Unit IO.Error A :=
    match v with
    | some a => Z.succeed a
    | none => Z.fail <| IO.userError "none found!"

  /-- Similar to Z.fromIO, but exposes the IO.Error in the error channel  -/
  def internal.attempt (io : IO A) (md := mempty): Z R IO.Error A  :=
    let infallible : IO (IO.Error ⊕ A) :=
      try
        return .inr (<- io)
      catch
        | ioError => return .inl ioError
    Z.internal.succeed infallible md
      |>.flatMap fun
      | .inr a => Z.internal.succeedNow a
      | .inl e => Z.internal.fail (R := R) e

  /-- Lift `IO` and expose an `IO.Error` as a typed failure. -/
  def attempt (io : IO A) (md := mempty): Z Unit IO.Error A :=
    internal.attempt io md

  /--
  Start a `Std.Async` task with an interruption action and expose its
  `IO.Error` in the typed error channel.

  The interruption action must stop or detach the underlying operation. It is
  also used when an enclosing Zenith fiber is interrupted.
  -/
  def fromAsyncInterrupt
      (start : IO (Std.Async.AsyncTask A × IO Unit))
      (md := Metadata.withLabel "fromAsyncInterrupt") :
      Z Unit IO.Error A :=
    Z.asyncInterrupt (md := md) fun observer => do
      let started : Except IO.Error (Std.Async.AsyncTask A × IO Unit) ←
        try
          pure (.ok (← start))
        catch error =>
          pure (.error error)
      match started with
      | .error error =>
          observer (.failure (.fail error))
          pure IO.unit
      | .ok (task, cancel) =>
          IO.chainTask task fun
            | .ok value => observer (.success value)
            | .error error => observer (.failure (.fail error))
          pure cancel

  /--
  Run a `Std.Async` computation as a Zenith effect.

  The `Std.Async` computation continues after Zenith interruption because a
  general `Std.Async` value has no cancellation action. Use
  `fromAsyncInterrupt` when the operation provides one.
  -/
  def fromAsync
      (action : Std.Async.Async A)
      (md := Metadata.withLabel "fromAsync") : Z Unit IO.Error A :=
    fromAsyncInterrupt (md := md) do
      let task ← action.toIO
      pure (task, IO.unit)

  /-- Pause without occupying a task worker. Interruption stops the timer. -/
  def sleep (ms : UInt32) : Z Unit Empty Unit :=
    (fromAsyncInterrupt do
      let duration := Std.Time.Millisecond.Offset.ofNat ms.toNat
      let sleeperTask ← (Std.Async.Sleep.mk duration).toIO
      let sleeper ← sleeperTask.block
      let task ← sleeper.wait.toIO
      pure (task, sleeper.stop)).orDie
      |>.withLabel s!"😴 sleep : {toString ms}ms"

  /-- Run an effectful operation with the required service. -/
  def serviceWithM (operation : S -> Z Unit E A) : Z S E A :=
    Z.fromCore fun service => (operation service).close ()

  /-- Compute a pure result from the required service. -/
  def serviceWith (operation : S -> A) : Z S E A :=
    Z.fromCore fun service => ZCore.succeedNow' (operation service)

  /-- Read a low-universe service as the successful value. -/
  def service (A) : Z A Empty A :=
    serviceWith id

  /-- Run `self` repeatedly until it fails, dies, or is interrupted. -/
  partial def forever : Z R E A :=
    self *> forever

  /-- Run `finalizer` after `self`, whatever its final exit. -/
  def ensuring (finalizer : Z R Empty A₀): Z R E A :=
    Z.fromCore fun environment =>
      (self.close environment).ensuring (finalizer.close environment)

  /--
  Run a finalizer with different environment and error requirements.

  If the protected effect and finalizer both fail, preserve their causes in
  execution order.
  -/
  def ensuringMeetJoin
      [meet : Environment.Meet R R₁ R₂]
      [join : ErrorChannel.Join E E₁ E₂]
      (finalizer : Z R₁ E₁ A₀) : Z R₂ E₂ A :=
    let effect := self.contramap meet.left
    let finalizer := finalizer.contramap meet.right
    effect.foldCauseM
      (fun cause =>
        finalizer.foldCauseM
          (fun finalizerCause =>
            (Z.failCause (R := R₂) <| .sequential
              (cause.map join.left)
              (finalizerCause.map join.right)).map impossible)
          (fun _ =>
            (Z.failCause (R := R₂) (cause.map join.left)).map impossible))
      (fun value =>
        finalizer.foldCauseM
          (fun finalizerCause =>
            (Z.failCause (R := R₂) (finalizerCause.map join.right)).map
              impossible)
          (fun _ => Z.internal.succeedNow value))

  /-- Make `self` interruptible while it runs. -/
  def interruptible : Z R E A :=
    self.setInterruptStatus .interruptible |>.withLabel "🛡 ↓ interruptible"

  /-- Defer interruption while `self` runs. -/
  def uninterruptible : Z R E A :=
    self.setInterruptStatus .uninterruptible |>.withLabel "🛡 ↑ uninterruptible"

end Z
