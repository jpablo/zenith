import Z.Coercions
import Z.Runtime.Fiber
import Z.Runtime.InterruptStatus
import Z.Environment
import Z.Runtime.Metadata

/-!
`ZCore` is the executable instruction tree. Its type parameters stay in
`Type`, which lets the existing interpreter and fiber storage remain
unchanged.

`Z` is the public, universe-polymorphic environment wrapper. Supplying an
environment closes a `Z` value into a `ZCore Unit` tree.
-/

/-- The low-level executable instruction tree for a closed Zenith effect. -/
inductive ZCore : Type -> Type -> Type -> Type 1 where
  | private internal.done
      (exit : Exit E A)
      (md := mempty) : ZCore R E A

  | private internal.sync
      (io : IO A)
      (md := mempty) : ZCore R E A

  | private internal.async
      (registerCallback : Observer E A -> IO Unit)
      (md := mempty) : ZCore R E A

  | private internal.asyncInterrupt
      (registerCallback : Observer E A -> IO (IO Unit))
      (md := mempty) : ZCore R E A

  | private internal.onSuccess
      (effect : ZCore R E A)
      (next : A -> ZCore R E A₁)
      (md := mempty) : ZCore R E A₁

  | private internal.onSuccessAndFailure
      (effect : ZCore R E A)
      (errorHandler : Cause E -> ZCore R E₁ A₁)
      (next : A -> ZCore R E₁ A₁)
      (md : Metadata) : ZCore R E₁ A₁

  | private internal.fork
      (effect : ZCore R E A)
      (name : String)
      (md : Metadata) : ZCore R Empty (Fiber E A)

  | private internal.setInterruptStatus
      (effect : ZCore R E A)
      (interruptStatus : InterruptStatus)
      (md := mempty) : ZCore R E A

  | private internal.contramap
      (f : R₀ -> R)
      (effect : ZCore R E A)
      (md := mempty) : ZCore R₀ E A

  | private internal.currentEnvironment
      (md := mempty) : ZCore R Empty (Environment R)

  | private internal.provideEnvironment
      (effect : ZCore R E A)
      (env : Environment R)
      (md := mempty) : ZCore Unit E A

namespace ZCore

/-- Transform the metadata stored on the outer instruction. -/
def updateMetadata
    (f : Metadata -> Metadata)
    (self : ZCore R E A) : ZCore R E A :=
  match self with
  | internal.done e md => internal.done e (f md)
  | internal.sync io md => internal.sync io (f md)
  | internal.async cb md => internal.async cb (f md)
  | internal.asyncInterrupt cb md => internal.asyncInterrupt cb (f md)
  | internal.onSuccess e n md => internal.onSuccess e n (f md)
  | internal.fork e n md => internal.fork e n (f md)
  | internal.onSuccessAndFailure e h n md =>
      internal.onSuccessAndFailure e h n (f md)
  | internal.setInterruptStatus e i md =>
      internal.setInterruptStatus e i (f md)
  | internal.contramap g e md => internal.contramap g e (f md)
  | internal.currentEnvironment md => internal.currentEnvironment (f md)
  | internal.provideEnvironment e r md =>
      internal.provideEnvironment e r (f md)

/-- Return the name of the outer instruction for diagnostics. -/
def showHead : ZCore R E A -> String
  | internal.done _ _ => "done"
  | internal.sync _ _ => "sync"
  | internal.async _ _ => "async"
  | internal.asyncInterrupt _ _ => "asyncInterrupt"
  | internal.onSuccess _ _ _ => "onSuccess"
  | internal.fork .. => "fork"
  | internal.onSuccessAndFailure .. => "onSuccessAndFailure"
  | internal.setInterruptStatus _ _ _ => "setInterruptStatus"
  | internal.contramap _ _ _ => "widenEnv"
  | internal.currentEnvironment _ => "currentEnvironment"
  | internal.provideEnvironment _ _ _ => "provideEnvironment"

/-- Return the metadata stored on the outer instruction. -/
def metadata : ZCore R E A -> Metadata
  | internal.done _ md => md
  | internal.sync _ md => md
  | internal.async _ md => md
  | internal.asyncInterrupt _ md => md
  | internal.onSuccess _ _ md => md
  | internal.fork _ _ md => md
  | internal.onSuccessAndFailure _ _ _ md => md
  | internal.setInterruptStatus _ _ md => md
  | internal.contramap _ _ md => md
  | internal.currentEnvironment md => md
  | internal.provideEnvironment _ _ md => md

/-- Replace the execution label stored on the outer instruction. -/
def withLabel (self : ZCore R E A) (label : String) : ZCore R E A :=
  self.updateMetadata fun md => { md with label := label }

/-- Create a completed success with any selected environment and error types. -/
def succeedNow'
    (value : A)
    (md := Metadata.withLabel "succeedNow") : ZCore R E A :=
  internal.done (.success value) md

/-- Transform the successful value of an instruction tree. -/
def map (f : A -> B) (self : ZCore R E A) : ZCore R E B :=
  internal.onSuccess self (f ∘> succeedNow') |>.withLabel "map"

/-- Adapt the environment required by an instruction tree. -/
def contramap
    (f : R₀ -> R₁)
    (effect : ZCore R₁ E A)
    (md := mempty) : ZCore R₀ E A :=
  internal.contramap f effect md

/-- Create a completed result with any selected environment type. -/
def done'
    (exit : Exit E A)
    (md := mempty) : ZCore R E A :=
  internal.done exit md

/-- Create a completed result that needs no environment. -/
def done
    (exit : Exit E A)
    (md := mempty) : ZCore Unit E A :=
  done' exit md

/-- Transform each typed failure in an instruction tree. -/
def mapFailure
    (f : E₀ -> E)
    (self : ZCore R E₀ A) : ZCore R E A :=
  internal.onSuccessAndFailure self
    (fun cause => done' (.failure (cause.map f)))
    (.success ∘> done')
    mempty

/-- Lift raw `IO` into an instruction tree with selected type parameters. -/
def succeed'
    (io : IO A)
    (md := Metadata.withLabel "succeed") : ZCore R E A :=
  internal.sync io md

/-- Start an instruction tree in a child fiber. -/
@[match_pattern]
def fork
    (effect : ZCore R E A)
    (name : String)
    (md := mempty) : ZCore R Empty (Fiber E A) :=
  internal.fork effect name md

/-- Create an instruction tree from callback registration. -/
def async
    (registerCallback : Observer E A -> IO Unit)
    (md := mempty) : ZCore R E A :=
  internal.async registerCallback md

/-- Register an asynchronous action that returns its interruption action. -/
def asyncInterrupt
    (registerCallback : Observer E A -> IO (IO Unit))
    (md := mempty) : ZCore R E A :=
  internal.asyncInterrupt registerCallback md

/-- Continue with `next` after a successful instruction result. -/
def flatMap
    (effect : ZCore R E A)
    (next : A -> ZCore R E A₁)
    (md := Metadata.withLabel "flatMap") : ZCore R E A₁ :=
  internal.onSuccess effect next md

/-- Run an instruction tree with the selected interruption status. -/
def setInterruptStatus
    (effect : ZCore R E A)
    (interruptStatus : InterruptStatus)
    (md := mempty) : ZCore R E A :=
  internal.setInterruptStatus effect interruptStatus md

/-- Handle either a complete failure cause or a successful value. -/
def foldCauseM
    (effect : ZCore R E A)
    (errorHandler : Cause E -> ZCore R E₁ A₁)
    (next : A -> ZCore R E₁ A₁)
    (md := Metadata.withLabel "foldCauseM") : ZCore R E₁ A₁ :=
  internal.onSuccessAndFailure effect errorHandler next md

/-- Read the current low-universe instruction environment. -/
def environment
    (R : Type)
    (md := Metadata.withLabel "environment") :
    ZCore R Empty (Environment R) :=
  internal.currentEnvironment md

/-- Supply the complete environment required by an instruction tree. -/
def provideEnvironment
    (effect : ZCore R E A)
    (env : Environment R)
    (md := mempty) : ZCore Unit E A :=
  internal.provideEnvironment effect env md

end ZCore

/--
An effect that needs an environment `R`, can fail with `E`, and can succeed
with `A`.

The environment can live in any universe. Error and success values stay in
`Type` so fibers can store them with the standard Lean runtime primitives.
-/
structure Z.{ur}
    (R : Type ur)
    (E A : Type) : Type (max 1 ur) where
  private mk ::
  close : R -> ZCore Unit E A

/-- An effect with no dependencies that can fail with `IO.Error`. -/
def ZTask (A : Type) : Type 1 := Z Unit IO.Error A

/-- An effect that cannot fail. -/
def URIO (R : Type u) (A : Type) : Type (max 1 u) := Z R Empty A

/-- An effect with no dependencies and no typed failures. -/
def UIO (A : Type) : Type 1 := Z Unit Empty A

namespace Z

/-- Build a public effect from an environment-closing function. -/
def fromCore
    (close : R -> ZCore Unit E A) : Z R E A :=
  ⟨close⟩

/-- Transform the execution metadata attached to `self`. -/
def updateMetadata
    (f : Metadata -> Metadata)
    (self : Z R E A) : Z R E A :=
  ⟨fun environment => (self.close environment).updateMetadata f⟩

/-- Replace the execution label attached to `self`. -/
def withLabel (self : Z R E A) (label : String) : Z R E A :=
  self.updateMetadata fun md => { md with label := label }

namespace internal

/-- Build a successful effect with context-selected environment and error types. -/
def succeedNow
    (value : A)
    (md := Metadata.withLabel "succeedNow") : Z R E A :=
  ⟨fun _ => ZCore.succeedNow' value md⟩

end internal

/-- Create a pure successful effect. -/
def succeed (value : A) : Z Unit Empty A :=
  internal.succeedNow value

/-- Transform the successful value of `self`. -/
def map (f : A -> B) (self : Z R E A) : Z R E B :=
  ⟨fun environment => (self.close environment).map f⟩

/-- Adapt an effect to a larger or differently shaped environment. -/
def contramap
    (f : R₀ -> R₁)
    (effect : Z R₁ E A)
    (md := mempty) : Z R₀ E A :=
  ⟨fun environment =>
    ZCore.contramap id (effect.close (f environment)) md⟩

namespace internal

/-- Build a completed effect with a context-selected environment type. -/
def done
    (exit : Exit E A)
    (md := mempty) : Z R E A :=
  ⟨fun _ => ZCore.done' exit md⟩

end internal

/-- Create an already-completed effect from an `Exit` value. -/
def done
    (exit : Exit E A)
    (md := mempty) : Z Unit E A :=
  internal.done exit md

/-- Transform every typed failure stored in the cause of `self`. -/
def mapFailure
    (f : E₀ -> E)
    (self : Z R E₀ A) : Z R E A :=
  ⟨fun environment => (self.close environment).mapFailure f⟩

namespace internal

/-- Build an IO-backed effect with context-selected environment and error types. -/
def succeed
    (io : IO A)
    (md := Metadata.withLabel "succeed") : Z R E A :=
  ⟨fun _ => ZCore.succeed' io md⟩

end internal

/--
Lift raw `IO` into Zenith without a typed error channel.

An `IO.Error` stays a defect. Use `Z.attempt` to expose it as `IO.Error` in
the typed error channel.
-/
def fromIO
    (io : IO A)
    (md := Metadata.withLabel "fromIO") : Z Unit Empty A :=
  internal.succeed io md

/-- Start `effect` in a child fiber and return its handle. -/
def fork
    (effect : Z R E A)
    (name : String)
    (md := mempty) : Z R Empty (Fiber E A) :=
  ⟨fun environment => ZCore.fork (effect.close environment) name md⟩

/-- Create an effect from a callback registration function. -/
def async
    (registerCallback : Observer E A -> IO Unit)
    (md := mempty) : Z R E A :=
  ⟨fun _ => ZCore.async registerCallback md⟩

/-- Register an asynchronous effect with an action that cancels its work. -/
def asyncInterrupt
    (registerCallback : Observer E A -> IO (IO Unit))
    (md := mempty) : Z R E A :=
  ⟨fun _ => ZCore.asyncInterrupt registerCallback md⟩

/-- Run `next` after a successful result from `effect`. -/
def flatMap
    (effect : Z R E A)
    (next : A -> Z R E A₁)
    (md := Metadata.withLabel "flatMap") : Z R E A₁ :=
  ⟨fun environment =>
    ZCore.flatMap
      (effect.close environment)
      (fun value => (next value).close environment)
      md⟩

/-- Compose two actions and infer their combined environment requirement. -/
def flatMapMeet
    [meet : Environment.Meet R₁ R₂ R]
    (effect : Z R₁ E A)
    (next : A -> Z R₂ E B) : Z R E B :=
  (effect.contramap meet.left).flatMap fun value =>
    (next value).contramap meet.right

/-- Compose effects with different error types and infer their joined error. -/
def flatMapJoin
    [join : ErrorChannel.Join E₁ E₂ E]
    (effect : Z R E₁ A)
    (next : A -> Z R E₂ B) : Z R E B :=
  (effect.mapFailure join.left).flatMap fun value =>
    (next value).mapFailure join.right

/-- Compose effects with different environment and error requirements. -/
def flatMapMeetJoin
    [meet : Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (effect : Z R₁ E₁ A)
    (next : A -> Z R₂ E₂ B) : Z R E B :=
  (effect.contramap meet.left).mapFailure join.left |>.flatMap fun value =>
    (next value).contramap meet.right |>.mapFailure join.right

/-- Run `effect` with the selected interruption status. -/
def setInterruptStatus
    (effect : Z R E A)
    (interruptStatus : InterruptStatus)
    (md := mempty) : Z R E A :=
  ⟨fun environment =>
    ZCore.setInterruptStatus
      (effect.close environment)
      interruptStatus
      md⟩

/-- Handle success and the complete structured failure cause of an effect. -/
def foldCauseM
    (effect : Z R E A)
    (errorHandler : Cause E -> Z R E₁ A₁)
    (next : A -> Z R E₁ A₁)
    (md := Metadata.withLabel "foldCauseM") : Z R E₁ A₁ :=
  ⟨fun environment =>
    ZCore.foldCauseM
      (effect.close environment)
      (fun cause => (errorHandler cause).close environment)
      (fun value => (next value).close environment)
      md⟩

/--
Read a low-universe environment as a result. High-universe services should use
`serviceWith` or `serviceWithM`, which do not return the service from a fiber.
-/
def environment
    (R : Type)
    (md := Metadata.withLabel "environment") :
    Z R Empty (Environment R) :=
  ⟨fun environment =>
    ZCore.map (fun _ => environment) (ZCore.environment Unit md)⟩

/-- Supply the complete environment required by `effect`. -/
def provideEnvironment
    (effect : Z R E A)
    (environment : Environment R)
    (md := mempty) : Z Unit E A :=
  ⟨fun _ => ZCore.provideEnvironment (effect.close environment) () md⟩

/-- Run raw `IO`, then select the next Zenith effect from its value. -/
def flatMapIO
    (io : IO A)
    (f : A -> Z R E B) : Z R E B :=
  (internal.succeed io).flatMap f

/-- Change all three `Z` parameters with explicit conversion functions. -/
def adapt
    (environment : R₀ -> R₁)
    (error : E₀ -> E₁)
    (success : A₀ -> A₁)
    (self : Z R₁ E₀ A₀) : Z R₀ E₁ A₁ :=
  self.contramap environment
    |>.mapFailure error
    |>.map success

/-- Widen an action to an explicitly selected environment and error type. -/
def widen
    [environment : Environment.CanProvide R R₁]
    [error : E₀ <: E]
    (self : Z R₁ E₀ A) : Z R E A :=
  (self.contramap environment.provide).mapFailure error.coe

/-- Widen an action into a normalized joined error channel. -/
def widenWithErrorInjection
    [environment : Environment.CanProvide R R₁]
    [error : ErrorChannel.CanInject E₀ E]
    (self : Z R₁ E₀ A) : Z R E A :=
  (self.contramap environment.provide).mapFailure error.inject

instance [conversion : Environment.CanProvide R₀ R₁] :
    CoeTC (Z R₁ E A) (Z R₀ E A) :=
  ⟨contramap conversion.provide⟩

instance [conversion : E₀ <: E] :
    CoeTC (Z R E₀ A) (Z R E A) :=
  ⟨mapFailure conversion.coe⟩

instance [conversion : A <: B] :
    CoeTC (Z R E A) (Z R E B) :=
  ⟨map conversion.coe⟩

instance (priority := low)
    [environment : Environment.CanProvide R₀ R₁]
    [error : E₀ <: E₁]
    [success : A₀ <: A₁] :
    CoeTC (Z R₁ E₀ A₀) (Z R₀ E₁ A₁) :=
  ⟨adapt environment.provide error.coe success.coe⟩

/-- Widen the impossible error channel of `self` to `E`. -/
def widenError (self : Z R Empty A) : Z R E A :=
  self.mapFailure impossible

/--
A raw `IO` action has no typed errors: a throw becomes a defect that the
`foldM` family never observes. It therefore only satisfies the defect-only
channel, and `Z.attempt` exposes an `IO.Error` as a typed failure.
-/
instance : CoeTC (IO A) (Z R Empty A) :=
  ⟨Z.internal.succeed⟩

end Z

/-- Raise an `IO.Error` from a context that promises an impossible result. -/
def ioThrow : IO.Error -> IO Empty :=
  @throw IO.Error IO _ Empty
