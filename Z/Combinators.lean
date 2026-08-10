import Z.Core

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

  def ensuring
      (finalizer : _root_.ZCore R Empty A₀) : _root_.ZCore R E A :=
    let finalizer := finalizer.withLabel "🏁 finalizer"
    let finalizerFailure (cause : Cause Empty) : _root_.ZCore R E A :=
      ZCore.failCause (cause.map impossible)
    .withLabel (label := "👮‍♀️ ensuring") <|
      self.foldCauseZ
        (fun cause =>
          finalizer.foldCauseZ
            finalizerFailure
            (fun _ => .failCause cause))
        (fun value =>
          finalizer.foldCauseZ
            finalizerFailure
            (fun _ => .succeedNow' value))

  instance : ToString (_root_.ZCore R E A) :=
    ⟨(·.showHead)⟩

end ZCore

namespace Z

  variable (self : Z R E A)

  def failCause (cause : Cause E) : Z R E Empty :=
    Z.internal.done <| Exit.failure cause

  namespace internal

  /-- Build a failed effect with a context-selected environment type. -/
  def fail [ToString E] (userError : E): Z R E Empty :=
    (failCause (Cause.fail userError)).withLabel s!"fail ({userError})"

  end internal

  def fail [ToString E] (userError : E): Z Unit E Empty :=
    internal.fail userError

  def die (ioe : IO.Error) : Z R Empty Empty :=
    failCause (Cause.die ioe)

  def errorHandlerCause (errorHandler : E -> Z R E₁ A₁): Cause E -> Z R E₁ A₁ := fun
    | .fail  e   => errorHandler e
    | .die ioe   => internal.done <| .failure <| .die ioe
    | .interrupt => internal.done <| .failure .interrupt

  def foldZ (errorHandler : E -> Z R E₁ A₁) (next : A -> Z R E₁ A₁) : Z R E₁ A₁ :=
    (self.foldCauseZ (errorHandlerCause errorHandler) next).withLabel "foldZ"

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
    tryCatch := fun z errorHandler => Z.foldZ z errorHandler pure

  instance : MonadExceptOf IO.Error (Z R Empty) where
    throw    := fun ioe => Z.die (R := R) ioe
    tryCatch := fun z errorHandler =>
      z.foldCauseZ
        (fun
          | .die ioe => errorHandler ioe
          | .interrupt => Z.failCause (R := R) (E := Empty) .interrupt
          | .fail e => nomatch e)
        pure

  -- instance : MonadExceptOf IO.Error (Z R (Cause E)) where
  --   throw    := fun ioe => Z.die ioe
  --   tryCatch := fun z errorHandler => 
  --     z.foldZ 
  --       (fun
  --         | .die ioe => errorHandler ioe
  --         | _ => z
  --       ) 
  --       pure

  def unit : Z R E Unit :=
    pure ()

  def fold (errorHandler : E -> A₁) (next : A -> A₁) : Z R E A₁ :=
    self.foldZ (errorHandler ∘> pure) (next ∘> pure)

  def foldCause (errorHandler : Cause E -> A₁) (next : A -> A₁) : Z R Empty A₁ :=
    self.foldCauseZ (errorHandler ∘> pure) (next ∘> pure)

  def exit : Z R Empty (Exit E A) :=
    self.foldCause Exit.failure Exit.success |>.withLabel "exit"

  /-- aka flatMapFailure  -/
  def catchAll [conversion : A <: A₁]
      (errorHandler : E -> Z R E₁ A₁) : Z R E₁ A₁ :=
    self.foldZ errorHandler (pure <| conversion.coe ·) |>.withLabel "catchAll"

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
    (self.contramap meet.left).foldCauseZ
      (fun
        | .die error => (errorHandler error).contramap meet.right
        | .interrupt =>
            (Z.failCause (R := R₂) (E := E₁) .interrupt).map impossible
        | .fail error => nomatch error)
      (fun value => pure (conversion.coe value))

  def zipWith (other : Z R E A₁) (f : A -> A₁ -> A₃) : Z R E A₃ := do
    return f (<- self) (<- other)

  def zip (other : Z R E A₁) : Z R E (A × A₁) := do
    self.zipWith other (·, ·) |>.withLabel "zip"

  def sandbox [ToString E]: Z R (Cause E) A :=
    self.foldCauseZ (fun e => fail e) pure

  def orDieWith (f : E -> IO.Error) : Z R Empty A :=
    self.foldZ (fun e => die (R := R) <| f e) pure

  def orDie (self : Z R IO.Error A): Z R Empty A :=
    self.orDieWith id |>.withLabel "orDie"

  def repeatN (n : Nat) (self : Z R E A): Z R E Unit :=
    .withLabel (label := s!"repeatN : {n}") $
    self.flatMap fun _ =>
      if n > 0 then
        repeatN (n - 1) self
      else
        Z.unit

  def getOrFail (v : Option A): Z Unit IO.Error A := 
    match v with
    | some a => Z.succeedNow a
    | none => Z.fail <| IO.userError "none found!"

  /-- Similar to Z.succeed, but exposes the IO.Error in the error channel  -/
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

  def attempt (io : IO A) (md := mempty): Z Unit IO.Error A :=
    internal.attempt io md

  def sleep (ms : UInt32) : Z Unit Empty Unit :=
    Z.succeed (IO.sleep ms) {label := s!"😴 sleep : {toString ms}ms"}

  def serviceWithZ (operation : S -> Z Unit E A) : Z S E A :=
    Z.fromCore fun service => (operation service).close ()

  def serviceWith (operation : S -> A) : Z S E A :=
    Z.fromCore fun service => ZCore.succeedNow' (operation service)

  def service (A) : Z A Empty A :=
    serviceWith id

  partial def forever : Z R E A :=
    self *> forever

  def ensuring (finalizer : Z R Empty A₀): Z R E A :=
    let finalizer := finalizer.withLabel "🏁 finalizer"
    let finalizerFailure (cause : Cause Empty) : Z R E A :=
      Z.failCause (R := R) (E := E) (cause.map impossible)
    .withLabel (label := s!"👮‍♀️ ensuring") $
      self.foldCauseZ
        (fun cause =>
          finalizer.foldCauseZ finalizerFailure
            (fun _ => .failCause (R := R) cause))
        (fun a     => finalizer.foldCauseZ finalizerFailure (fun _ => pure a))

  /--
  Run a finalizer with different environment and error requirements.

  A finalizer failure takes precedence over a failure from the protected
  effect, as required by Lean's `try/finally` semantics.
  -/
  def ensuringMeetJoin
      [meet : Environment.Meet R R₁ R₂]
      [join : ErrorChannel.Join E E₁ E₂]
      (finalizer : Z R₁ E₁ A₀) : Z R₂ E₂ A :=
    let effect := self.contramap meet.left
    let finalizer := finalizer.contramap meet.right
    effect.foldCauseZ
      (fun cause =>
        finalizer.foldCauseZ
          (fun finalizerCause =>
            (Z.failCause (R := R₂) (finalizerCause.map join.right)).map
              impossible)
          (fun _ =>
            (Z.failCause (R := R₂) (cause.map join.left)).map impossible))
      (fun value =>
        finalizer.foldCauseZ
          (fun finalizerCause =>
            (Z.failCause (R := R₂) (finalizerCause.map join.right)).map
              impossible)
          (fun _ => Z.internal.succeedNow value))

  def interruptible : Z R E A :=
    self.setInterruptStatus .interruptible |>.withLabel "🛡 ↓ interruptible"

  def uninterruptible : Z R E A :=
    self.setInterruptStatus .uninterruptible |>.withLabel "🛡 ↑ uninterruptible"

end Z
