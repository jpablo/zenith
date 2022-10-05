import Z.Core

namespace Z

  variable (self : Z R E A)

  def nodeId : NodeId :=
    self.metadata.nodeId

  def setNodeId (nodeId : NodeId) : Z R E A :=
    self.updateMetadata fun md => {md with nodeId := nodeId}

  def resetNodeId : Z R E A :=
    self.setNodeId ""

  def label : String :=
    self.metadata.label

  def ensureNodeId (nodeId : NodeId) : NodeId × (Z R E A) :=
    if self.metadata.nodeId.isEmpty then
      (nodeId, self.setNodeId nodeId)
    else
      (self.metadata.nodeId, self)
      
  def ensureNodeId' (nodeId : NodeId) : Z R E A :=
    if self.metadata.nodeId.isEmpty then
      self.setNodeId nodeId
    else
      self

  def failCause (cause : Cause E) : Z R E Empty :=
    Z.done' <| Exit.failure cause

  def fail' [ToString E] (userError : E): Z R E Empty :=
    (failCause (Cause.fail userError)).withLabel s!"fail ({userError})"

  def fail [ToString E] (userError : E): Z Unit E Empty :=
    fail' userError

  def die (ioe : IO.Error) : Z Unit Empty Empty :=
    failCause (Cause.die ioe)

  def errorHandlerCause (errorHandler : E -> Z R E₁ A₁): Cause E -> Z R E₁ A₁ := fun
    | .fail  e   => errorHandler e
    | .die ioe   => done' <| .failure <| .die ioe
    | .interrupt => done' <| .failure .interrupt

  def foldZ (errorHandler : E -> Z R E₁ A₁) (next : A -> Z R E₁ A₁) : Z R E₁ A₁ :=
    (self.foldCauseZ (errorHandlerCause errorHandler) next).withLabel "foldZ"

  /- ---- Monad instances ------------ -/

  instance : Monad (Z R E) where
    pure a := succeedNow' a |>.withLabel "pure"
    bind z f := z.flatMap f |>.withLabel "do"

  instance : ToString (Z R E A) := ⟨(·.showHead)⟩
  instance : ToString (URIO R A) := inferInstanceAs (ToString (Z R Empty A))

  instance : Monad ZTask    := inferInstanceAs (Monad (Z Unit IO.Error))
  instance : Monad UIO      := inferInstanceAs (Monad (Z Unit Empty))
  instance : Monad (URIO R) := inferInstanceAs (Monad (Z R Empty))

  instance : MonadExceptOf E (Z R E) where
    throw    := fun e => Z.failCause <| .fail e
    tryCatch := fun z errorHandler => Z.foldZ z errorHandler pure

  instance : MonadExceptOf IO.Error (Z R Empty) where
    throw    := fun ioe => Z.die ioe
    tryCatch := fun z _ => Z.foldZ z impossible pure

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
    self.foldZ (pure ∘ errorHandler) (pure ∘ next)

  def foldCause (errorHandler : Cause E -> A₁) (next : A -> A₁) : Z R Empty A₁ :=
    self.foldCauseZ (pure ∘ errorHandler) (pure ∘ next)

  def exit : Z R Empty (Exit E A) :=
    self.foldCause Exit.failure Exit.success |>.withLabel "exit"

  /-- aka flatMapFailure  -/
  def catchAll [A <: A₁] (errorHandler : E -> Z R E₁ A₁) : Z R E₁ A₁ :=
    self.foldZ errorHandler (pure .) |>.withLabel "catchAll"

  def mapFailure [ToString E₁] (f : E -> E₁) : Z R E₁ A :=
    self.catchAll fun e => .fail (f e)

  def zipWith (other : Z R E A₁) (f : A -> A₁ -> A₃) : Z R E A₃ := do
    return f (<- self) (<- other)

  def zip (other : Z R E A₁) : Z R E (A × A₁) := do
    self.zipWith other (., .) |>.withLabel "zip"

  def sandbox [ToString E]: Z R (Cause E) A :=
    self.foldCauseZ (fun e => Z.fail e) pure

  def orDieWith (f : E -> IO.Error) : Z R Empty A :=
    self.foldZ (fun e => Z.die <| f e) pure

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
    | some a => Z.succeedNow' a
    | none => Z.fail' <| IO.userError "none found!"

  /-- Similar to Z.succeed, but exposes the IO.Error in the error channel  -/
  def attempt' (io : IO A) (md := mempty): Z R IO.Error A  :=
    let infallible : IO (IO.Error ⊕ A) :=
      try
        return .inr (<- io)
      catch
        | ioError => return .inl ioError
    Z.succeed' infallible md
      |>.flatMap fun
      | .inr a => Z.succeedNow' a
      | .inl e => Z.fail' e

  def attempt (io : IO A) (md := mempty): Z Unit IO.Error A :=
    attempt' io md

  def sleep (ms : UInt32) : Z Unit Empty Unit :=
    Z.succeed (IO.sleep ms) {label := s!"😴 sleep : {toString ms}ms"}

  def serviceWithZ (f : S -> Z R E A): Z (R × S) E A := do
    let environment <- Z.environment (R × S) |>.widenError
    Z.contramap (·.1) <| f <| environment.get S

  def serviceWith (f : S -> A) : Z S E A :=
      Z.contramap ((), ·) (Z.serviceWithZ fun s => Z.succeedNow (f s))

  def service (A) : Z A Empty A :=
    serviceWith id

  partial def forever : Z R E A :=
    self *> forever

  def ensuring (finalizer : Z R Empty A₀): Z R E A :=
    let finalizer := finalizer.withLabel "🏁 finalizer"
    .withLabel (label := s!"👮‍♀️ ensuring") $
      self.foldCauseZ
        (fun cause => finalizer.foldCauseZ (fun _ => .failCause cause) (fun _ => .failCause cause))
        (fun a     => finalizer.foldCauseZ (fun _ => pure a) (fun _ => pure a))

  def interruptible : Z R E A :=
    self.setInterruptStatus .interruptible |>.withLabel "🛡 ↓ interruptible"

  def uninterruptible : Z R E A :=
    self.setInterruptStatus .uninterruptible |>.withLabel "🛡 ↑ uninterruptible"

end Z
