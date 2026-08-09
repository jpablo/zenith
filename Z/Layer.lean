import Z.Interpreter
import Z.HEIO
import Std.Sync.Mutex

/-!
A layer builds a scoped service inside `HEIO`. The input and output can live
in any universe. Layer failures retain the complete `Cause E` value.
-/

structure Layer.Resource.{u}
    (E : Type)
    (A : Type u) : Type u where
  value : A
  release : HEIO (Cause E) Unit

structure Layer.{uin, uout}
    (RIn : Type uin)
    (E : Type)
    (ROut : Type uout) : Type (max uin uout) where
  build : RIn -> HEIO (Cause E) (Layer.Resource E ROut)

namespace Layer

namespace Resource

def make
    (value : A)
    (release : HEIO (Cause E) Unit := HEIO.pure ()) :
    Resource E A :=
  { value, release }

def map
    (f : A -> B)
    (self : Resource E A) : Resource E B :=
  { value := f self.value
    release := self.release }

def mapError
    (f : E -> E₁)
    (self : Resource E A) : Resource E₁ A :=
  { value := self.value
    release := self.release.mapError (Cause.map f) }

end Resource

private def acquireAfter
    (first : Resource E A)
    (second : HEIO (Cause E) (Resource E B)) :
    HEIO (Cause E) (Resource E B) :=
  second.fold
    (fun cause =>
      (HEIO.throw cause).ensuring first.release)
    (fun acquired =>
      HEIO.pure {
        value := acquired.value
        release := acquired.release.ensuring first.release
      })

private inductive MemoState.{u}
    (E : Type)
    (A : Type u) : Type u where
  | empty
  | failed (cause : Cause E)
  | ready (resource : Resource E A)

private def withMutex
    (mutex : Std.BaseMutex)
    (action : HEIO (Cause E) A) : HEIO (Cause E) A :=
  HEIO.bind (HEIO.liftBaseIO.{0} mutex.lock) fun _ =>
    action.ensuring <|
      HEIO.bind (HEIO.liftBaseIO.{0} mutex.unlock) fun _ =>
        HEIO.pure ()

private def buildMemoized
    (self : Layer R E A)
    (environment : R)
    (mutex : Std.BaseMutex)
    (cache : HEIO.Ref (MemoState E A)) :
    HEIO (Cause E) (Resource E A) :=
  withMutex mutex <|
    HEIO.bind cache.get fun
      | .empty =>
          (self.build environment).fold
            (fun cause =>
              HEIO.bind (cache.set (.failed cause)) fun _ =>
                HEIO.throw cause)
            (fun resource =>
              HEIO.bind (cache.set (.ready resource)) fun _ =>
                HEIO.pure (Resource.make resource.value))
      | .failed cause => HEIO.throw cause
      | .ready resource => HEIO.pure (Resource.make resource.value)

private def releaseMemoized
    (mutex : Std.BaseMutex)
    (cache : HEIO.Ref (MemoState E A)) : HEIO (Cause E) Unit :=
  withMutex mutex <|
    HEIO.bind (cache.swap .empty) fun
      | .ready resource => resource.release
      | .empty | .failed _ => HEIO.pure ()

def fromHEIO
    (build : RIn -> HEIO (Cause E) ROut) :
    Layer RIn E ROut :=
  ⟨fun environment =>
    (build environment).map Resource.make⟩

/--
Acquire a service and attach its release action. The release action runs once
after the program, or after a later layer acquisition fails.
-/
def acquireRelease
    (acquire : R -> HEIO (Cause E) A)
    (release : R -> A -> HEIO (Cause E) Unit) :
    Layer R E A :=
  ⟨fun environment =>
    (acquire environment).map fun value =>
      Resource.make value (release environment value)⟩

/--
Create a layer that shares one build of `self` inside its scope. The returned
layer can be used more than once. Its resource is released once when the outer
scope closes.
-/
def memoize
    (self : Layer R E A) : Layer R E (Layer R E A) :=
  ⟨fun environment =>
    HEIO.bind (HEIO.liftBaseIO.{0} Std.BaseMutex.new) fun mutex =>
      HEIO.bind (HEIO.mkRef (MemoState.empty : MemoState E A)) fun cache =>
        let shared : Layer R E A :=
          ⟨fun _ => buildMemoized self environment mutex.down cache⟩
        HEIO.pure <|
          Resource.make shared (releaseMemoized mutex.down cache)⟩

def succeed (value : A) : Layer Unit Empty A :=
  fromHEIO fun _ => HEIO.pure value

def succeedEnvironment
    (environment : Environment A) : Layer Unit Empty A :=
  succeed environment

def failCause (cause : Cause E) : Layer R E A :=
  fromHEIO fun _ => HEIO.throw cause

def suspend
    (layer : Thunk (Layer R E A)) : Layer R E A :=
  ⟨fun environment => layer.get.build environment⟩

def contramap
    (f : R₀ -> R)
    (self : Layer R E A) : Layer R₀ E A :=
  ⟨fun environment => self.build (f environment)⟩

instance [conversion : R₀ <: R] :
    (Layer R E A) <: (Layer R₀ E A) :=
  ⟨contramap conversion.coe⟩

def mapError
    (f : E -> E₁)
    (self : Layer R E A) : Layer R E₁ A :=
  ⟨fun environment =>
    ((self.build environment).mapError (Cause.map f)).map
      (Resource.mapError f)⟩

instance [conversion : E <: E₁] :
    (Layer R E A) <: (Layer R E₁ A) :=
  ⟨mapError conversion.coe⟩

def flatMap
    (self : Layer R E A)
    (next : A -> Layer R E B) : Layer R E B :=
  ⟨fun environment =>
    HEIO.bind (self.build environment) fun acquired =>
      acquireAfter acquired ((next acquired.value).build environment)⟩

/-- Build `self` once and pass its shared form to `use`. -/
def share
    (self : Layer R E A)
    (use : Layer R E A -> Layer R E B) : Layer R E B :=
  self.memoize.flatMap use

def map
    (self : Layer R E A)
    (f : A -> B) : Layer R E B :=
  ⟨fun environment =>
    (self.build environment).map (Resource.map f)⟩

/-- Feed the output of one layer into the next layer. -/
def to
    (self : Layer R E A)
    (next : Layer A E B) : Layer R E B :=
  ⟨fun environment =>
    HEIO.bind (self.build environment) fun acquired =>
      acquireAfter acquired (next.build acquired.value)⟩

/-- Build two layers in sequence and combine their outputs. -/
def zipWith
    (left : Layer R E A)
    (right : Layer R E B)
    (f : A -> B -> C) : Layer R E C :=
  ⟨fun environment =>
    HEIO.bind (left.build environment) fun acquiredLeft =>
      HEIO.map
        (fun acquiredRight =>
          acquiredRight.map (f acquiredLeft.value))
        (acquireAfter acquiredLeft (right.build environment))⟩

/-- Build two independent layers in parallel and combine their outputs. -/
def zipWithPar
    (left : Layer R E A)
    (right : Layer R E B)
    (f : A -> B -> C) : Layer R E C :=
  ⟨fun environment =>
    HEIO.bind (HEIO.fork (left.build environment)) fun leftTask =>
      HEIO.bind (HEIO.fork (right.build environment)) fun rightTask =>
        HEIO.bind (HEIO.wait leftTask) fun leftResult =>
          HEIO.bind (HEIO.wait rightTask) fun rightResult =>
            match leftResult, rightResult with
            | .ok acquiredLeft, .ok acquiredRight =>
                HEIO.pure {
                  value := f acquiredLeft.value acquiredRight.value
                  release :=
                    acquiredRight.release.ensuring acquiredLeft.release
                }
            | .ok acquiredLeft, .error cause =>
                (HEIO.throw cause).ensuring acquiredLeft.release
            | .error cause, .ok acquiredRight =>
                (HEIO.throw cause).ensuring acquiredRight.release
            | .error leftCause, .error _ =>
                HEIO.throw leftCause⟩

def fromFunction (f : R -> A) : Layer R Empty A :=
  fromHEIO fun environment => HEIO.pure (f environment)

private def runZ
    (fiberId : FiberId)
    (effect : Z R E A)
    (environment : R) : HEIO (Cause E) A :=
    HEIO.bind
      (HEIO.liftIO.{0} Cause.die <|
        Z.unsafeRunSync
          (effect.provideEnvironment environment)
          fiberId)
      fun result =>
        match result.down with
        | some (.success value) => HEIO.pure value
        | some (.failure cause) => HEIO.throw cause
        | none =>
            HEIO.throw <| .die <|
              IO.userError "the layer fiber did not return a result"

/-- Build a low-universe layer output with a normal Zenith effect. -/
def fromZ
    (effect : Z R E A) : Layer R E A :=
  fromHEIO fun environment => runZ "layer" effect environment

/--
Acquire a low-universe service with `Z` and release it when the layer scope
closes. The release effect cannot have a typed failure.
-/
def acquireReleaseZ
    (acquire : Z R E A)
    (release : A -> Z R Empty Unit) : Layer R E A :=
  acquireRelease
    (runZ "layer-acquire" acquire)
    (fun environment value =>
      (runZ "layer-release" (release value) environment).mapError
        (Cause.map impossible))

def fromEnvironment
    (effect : Z R E (Environment A)) : Layer R E A :=
  fromZ effect

/--
Build a layer, supply its service to a program, and run the resulting deep
instruction tree.
-/
def run.{uin, uout}
    (self : Layer.{uin, uout} RIn E ROut)
    (input : RIn)
    (program : Z ROut E A)
    (fiberId : FiberId := "main")
    (useDiagram : Option String := none) : IO (Option (Exit E A)) := do
  let builtAndRun : HEIO (Cause E) (ULift.{uout} (Option (Exit E A))) :=
    HEIO.bind (self.build input) fun resource =>
      (HEIO.liftIO.{uout} Cause.die <|
          Z.unsafeRunSync
            (program.provideEnvironment resource.value)
            fiberId
            useDiagram).ensuring resource.release
  match <- HEIO.toIOResult builtAndRun with
  | .ok result => pure result
  | .error cause => pure (some (.failure cause))

end Layer
