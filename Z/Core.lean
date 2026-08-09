import Z.Coercions
import Z.Fiber
import Z.InterruptStatus
import Z.Environment
import Z.Metadata

/-!
`ZCore` is the executable instruction tree. Its type parameters stay in
`Type`, which lets the existing interpreter and fiber storage remain
unchanged.

`Z` is the public, universe-polymorphic environment wrapper. Supplying an
environment closes a `Z` value into a `ZCore Unit` tree.
-/

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

def updateMetadata
    (f : Metadata -> Metadata)
    (self : ZCore R E A) : ZCore R E A :=
  match self with
  | internal.done e md => internal.done e (f md)
  | internal.sync io md => internal.sync io (f md)
  | internal.async cb md => internal.async cb (f md)
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

def showHead : ZCore R E A -> String
  | internal.done _ _ => "done"
  | internal.sync _ _ => "sync"
  | internal.async _ _ => "async"
  | internal.onSuccess _ _ _ => "onSuccess"
  | internal.fork .. => "fork"
  | internal.onSuccessAndFailure .. => "onSuccessAndFailure"
  | internal.setInterruptStatus _ _ _ => "setInterruptStatus"
  | internal.contramap _ _ _ => "widenEnv"
  | internal.currentEnvironment _ => "currentEnvironment"
  | internal.provideEnvironment _ _ _ => "provideEnvironment"

def metadata : ZCore R E A -> Metadata
  | internal.done _ md => md
  | internal.sync _ md => md
  | internal.async _ md => md
  | internal.onSuccess _ _ md => md
  | internal.fork _ _ md => md
  | internal.onSuccessAndFailure _ _ _ md => md
  | internal.setInterruptStatus _ _ md => md
  | internal.contramap _ _ md => md
  | internal.currentEnvironment md => md
  | internal.provideEnvironment _ _ md => md

def withLabel (self : ZCore R E A) (label : String) : ZCore R E A :=
  self.updateMetadata fun md => { md with label := label }

def succeedNow'
    (value : A)
    (md := Metadata.withLabel "succeedNow") : ZCore R E A :=
  internal.done (.success value) md

def map (f : A -> B) (self : ZCore R E A) : ZCore R E B :=
  internal.onSuccess self (f ∘> succeedNow') |>.withLabel "map"

def contramap
    (f : R₀ -> R₁)
    (effect : ZCore R₁ E A)
    (md := mempty) : ZCore R₀ E A :=
  internal.contramap f effect md

def done'
    (exit : Exit E A)
    (md := mempty) : ZCore R E A :=
  internal.done exit md

def done
    (exit : Exit E A)
    (md := mempty) : ZCore Unit E A :=
  done' exit md

def mapFailure
    (f : E₀ -> E)
    (self : ZCore R E₀ A) : ZCore R E A :=
  internal.onSuccessAndFailure self
    (fun cause => done' (.failure (cause.map f)))
    (.success ∘> done')
    mempty

def succeed'
    (io : IO A)
    (md := Metadata.withLabel "succeed") : ZCore R E A :=
  internal.sync io md

@[match_pattern]
def fork
    (effect : ZCore R E A)
    (name : String)
    (md := mempty) : ZCore R Empty (Fiber E A) :=
  internal.fork effect name md

def async
    (registerCallback : Observer E A -> IO Unit)
    (md := mempty) : ZCore R E A :=
  internal.async registerCallback md

def flatMap
    (effect : ZCore R E A)
    (next : A -> ZCore R E A₁)
    (md := Metadata.withLabel "flatMap") : ZCore R E A₁ :=
  internal.onSuccess effect next md

def setInterruptStatus
    (effect : ZCore R E A)
    (interruptStatus : InterruptStatus)
    (md := mempty) : ZCore R E A :=
  internal.setInterruptStatus effect interruptStatus md

def foldCauseZ
    (effect : ZCore R E A)
    (errorHandler : Cause E -> ZCore R E₁ A₁)
    (next : A -> ZCore R E₁ A₁)
    (md := Metadata.withLabel "foldCauseZ") : ZCore R E₁ A₁ :=
  internal.onSuccessAndFailure effect errorHandler next md

def environment
    (R : Type)
    (md := Metadata.withLabel "environment") :
    ZCore R Empty (Environment R) :=
  internal.currentEnvironment md

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

def UIO (A : Type) : Type 1 := Z Unit Empty A

namespace Z

/-- Build a public effect from an environment-closing function. -/
def fromCore
    (close : R -> ZCore Unit E A) : Z R E A :=
  ⟨close⟩

def updateMetadata
    (f : Metadata -> Metadata)
    (self : Z R E A) : Z R E A :=
  ⟨fun environment => (self.close environment).updateMetadata f⟩

def withLabel (self : Z R E A) (label : String) : Z R E A :=
  self.updateMetadata fun md => { md with label := label }

def succeedNow'
    (value : A)
    (md := Metadata.withLabel "succeedNow") : Z R E A :=
  ⟨fun _ => ZCore.succeedNow' value md⟩

def succeedNow (value : A) : Z Unit Empty A :=
  succeedNow' value

def map (f : A -> B) (self : Z R E A) : Z R E B :=
  ⟨fun environment => (self.close environment).map f⟩

def contramap
    (f : R₀ -> R₁)
    (effect : Z R₁ E A)
    (md := mempty) : Z R₀ E A :=
  ⟨fun environment =>
    ZCore.contramap id (effect.close (f environment)) md⟩

def done'
    (exit : Exit E A)
    (md := mempty) : Z R E A :=
  ⟨fun _ => ZCore.done' exit md⟩

def done
    (exit : Exit E A)
    (md := mempty) : Z Unit E A :=
  done' exit md

def mapFailure
    (f : E₀ -> E)
    (self : Z R E₀ A) : Z R E A :=
  ⟨fun environment => (self.close environment).mapFailure f⟩

def succeed'
    (io : IO A)
    (md := Metadata.withLabel "succeed") : Z R E A :=
  ⟨fun _ => ZCore.succeed' io md⟩

def succeed
    (io : IO A)
    (md := Metadata.withLabel "succeed") : Z Unit Empty A :=
  succeed' io md

def fork
    (effect : Z R E A)
    (name : String)
    (md := mempty) : Z R Empty (Fiber E A) :=
  ⟨fun environment => ZCore.fork (effect.close environment) name md⟩

def async
    (registerCallback : Observer E A -> IO Unit)
    (md := mempty) : Z R E A :=
  ⟨fun _ => ZCore.async registerCallback md⟩

def flatMap
    (effect : Z R E A)
    (next : A -> Z R E A₁)
    (md := Metadata.withLabel "flatMap") : Z R E A₁ :=
  ⟨fun environment =>
    ZCore.flatMap
      (effect.close environment)
      (fun value => (next value).close environment)
      md⟩

def setInterruptStatus
    (effect : Z R E A)
    (interruptStatus : InterruptStatus)
    (md := mempty) : Z R E A :=
  ⟨fun environment =>
    ZCore.setInterruptStatus
      (effect.close environment)
      interruptStatus
      md⟩

def foldCauseZ
    (effect : Z R E A)
    (errorHandler : Cause E -> Z R E₁ A₁)
    (next : A -> Z R E₁ A₁)
    (md := Metadata.withLabel "foldCauseZ") : Z R E₁ A₁ :=
  ⟨fun environment =>
    ZCore.foldCauseZ
      (effect.close environment)
      (fun cause => (errorHandler cause).close environment)
      (fun value => (next value).close environment)
      md⟩

/--
Read a low-universe environment as a result. High-universe services should use
`serviceWith` or `serviceWithZ`, which do not return the service from a fiber.
-/
def environment
    (R : Type)
    (md := Metadata.withLabel "environment") :
    Z R Empty (Environment R) :=
  ⟨fun environment =>
    ZCore.map (fun _ => environment) (ZCore.environment Unit md)⟩

def provideEnvironment
    (effect : Z R E A)
    (environment : Environment R)
    (md := mempty) : Z Unit E A :=
  ⟨fun _ => ZCore.provideEnvironment (effect.close environment) () md⟩

def withIO
    (io : IO A)
    (f : A -> Z R E B) : Z R E B :=
  (succeed' io).flatMap f

instance [conversion : R₀ <: R₁] : (Z R₁ E A) <: (Z R₀ E A) :=
  ⟨contramap conversion.coe⟩

instance [conversion : E₀ <: E] : (Z R E₀ A) <: (Z R E A) :=
  ⟨mapFailure conversion.coe⟩

instance [conversion : A <: B] : (Z R E A) <: (Z R E B) :=
  ⟨map conversion.coe⟩

def widenError (self : Z R Empty A) : Z R E A :=
  self.mapFailure impossible

instance : IO A <: Z R E A :=
  ⟨Z.succeed'⟩

end Z

/-- Raise an `IO.Error` from a context that promises an impossible result. -/
def ioThrow : IO.Error -> IO Empty :=
  @throw IO.Error IO _ Empty
