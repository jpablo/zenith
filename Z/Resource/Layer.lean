import Z.Runtime.Interpreter
import Z.Resource.Internal.HEIO
import Std.Sync.Mutex

/-!
A layer builds a scoped service inside `HEIO`. The input and output can live
in any universe. Layer failures retain the complete `Cause E` value.
-/

/-- A built layer output together with its release action. -/
structure Layer.Resource.{u}
    (E : Type)
    (A : Type u) : Type u where
  value : A
  release : HEIO (Cause E) Unit

/-- A scoped constructor from an input environment to one output service. -/
structure Layer.{uin, uout}
    (RIn : Type uin)
    (E : Type)
    (ROut : Type uout) : Type (max uin uout) where
  build : RIn -> HEIO (Cause E) (Layer.Resource E ROut)

namespace Layer

namespace Resource

/-- Pair a value with a release action. -/
def make
    (value : A)
    (release : HEIO (Cause E) Unit := HEIO.pure ()) :
    Resource E A :=
  { value, release }

/-- Transform the resource value without changing its release action. -/
def map
    (f : A -> B)
    (self : Resource E A) : Resource E B :=
  { value := f self.value
    release := self.release }

/-- Translate typed failures in the resource release action. -/
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
  (HEIO.bind HEIO.checkInterrupted fun _ => second).foldAll
    (fun cause =>
      (HEIO.throw cause).ensuringCause first.release)
    ((HEIO.interrupt : HEIO (Cause E) (Resource E B)).ensuringCause
      first.release)
    (fun acquired =>
      HEIO.pure {
        value := acquired.value
        release := acquired.release.ensuringCause first.release
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
    action.ensuringCause <|
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

/-- Build a layer from an `HEIO` action with no custom release action. -/
def fromBuild
    (build : RIn -> HEIO (Cause E) ROut) :
    Layer RIn E ROut :=
  ⟨fun environment =>
    HEIO.bind HEIO.checkInterrupted fun _ =>
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
    HEIO.bind HEIO.checkInterrupted fun _ =>
      (acquire environment).map fun value =>
        Resource.make value (release environment value)⟩

/--
Create a layer that shares one build of `self` inside its scope. The returned
layer can be used more than once, and it builds `self` from the input its first
use supplies. Its resource is released once when the outer scope closes.
-/
def memoize
    (self : Layer R E A) : Layer R E (Layer R E A) :=
  ⟨fun _ =>
    HEIO.bind (HEIO.liftBaseIO.{0} Std.BaseMutex.new) fun mutex =>
      HEIO.bind (HEIO.mkRef (MemoState.empty : MemoState E A)) fun cache =>
        let shared : Layer R E A :=
          ⟨fun environment => buildMemoized self environment mutex.down cache⟩
        HEIO.pure <|
          Resource.make shared (releaseMemoized mutex.down cache)⟩

/-- Create a closed layer that provides `value`. -/
def succeed (value : A) : Layer Unit Empty A :=
  fromBuild fun _ => HEIO.pure value

/-- Create a layer that fails with `cause` when built. -/
def failCause (cause : Cause E) : Layer R E A :=
  fromBuild fun _ => HEIO.throw cause

/-- Delay evaluation of `layer` until this layer is built. -/
def suspend
    (layer : Thunk (Layer R E A)) : Layer R E A :=
  ⟨fun environment => layer.get.build environment⟩

/-- Adapt the input environment required to build a layer. -/
def contramap
    (f : R₀ -> R)
    (self : Layer R E A) : Layer R₀ E A :=
  ⟨fun environment => self.build (f environment)⟩

instance [conversion : R₀ <: R] :
    CoeTC (Layer R E A) (Layer R₀ E A) :=
  ⟨contramap conversion.coe⟩

/-- Transform typed failures from layer acquisition and release. -/
def mapError
    (f : E -> E₁)
    (self : Layer R E A) : Layer R E₁ A :=
  ⟨fun environment =>
    ((self.build environment).mapError (Cause.map f)).map
      (Resource.mapError f)⟩

instance [conversion : E <: E₁] :
    CoeTC (Layer R E A) (Layer R E₁ A) :=
  ⟨mapError conversion.coe⟩

/-- Build `self`, then build the next layer from its output. -/
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

/-- Transform a layer output without changing its lifetime. -/
def map
    (self : Layer R E A)
    (f : A -> B) : Layer R E B :=
  ⟨fun environment =>
    (self.build environment).map (Resource.map f)⟩

/-- Change all three `Layer` parameters with explicit conversion functions. -/
def adapt
    (environment : R₀ -> R)
    (error : E -> E₁)
    (output : A -> B)
    (self : Layer R E A) : Layer R₀ E₁ B :=
  self.contramap environment
    |>.mapError error
    |>.map output

instance [conversion : A <: B] :
    CoeTC (Layer R E A) (Layer R E B) :=
  ⟨fun self => self.map conversion.coe⟩

instance (priority := low)
    [environment : R₀ <: R]
    [error : E <: E₁]
    [output : A <: B] :
    CoeTC (Layer R E A) (Layer R₀ E₁ B) :=
  ⟨adapt environment.coe error.coe output.coe⟩

/-- Feed the output of one layer into the next layer. -/
def andThen
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

private inductive ParallelCompletion where
  | leftSuccess
  | leftFailure
  | leftInterrupted
  | rightSuccess
  | rightFailure
  | rightInterrupted
  deriving Inhabited

private def signalParallel
    (completion : IO.Promise ParallelCompletion)
    (result : ParallelCompletion) : HEIO (Cause E) Unit :=
  HEIO.bind
    (HEIO.liftBaseIO.{0} (completion.resolve result))
    fun _ => HEIO.pure ()

private def observeParallel
    (completion : IO.Promise ParallelCompletion)
    (success failure interrupted : ParallelCompletion)
    (action : HEIO (Cause E) A) : HEIO (Cause E) A :=
  action.foldAll
    (fun cause =>
      HEIO.bind (signalParallel completion failure) fun _ =>
        HEIO.throw cause)
    (HEIO.bind (signalParallel completion interrupted) fun _ =>
      HEIO.interrupt)
    (fun value =>
      HEIO.bind (signalParallel completion success) fun _ =>
        HEIO.pure value)

private def waitParallel
    (completion : IO.Promise ParallelCompletion) :
    HEIO (Cause E) ParallelCompletion :=
  HEIO.bind
    (HEIO.liftBaseIO.{0} (IO.wait completion.result?))
    fun result =>
      match result.down with
      | some completion => HEIO.pure completion
      | none => HEIO.throw <| .die <| IO.userError
          "a parallel layer branch did not report completion"

private def releaseResult
    (result : HEIO.Result (Cause E) (Resource E A)) :
    HEIO (Cause E) Unit :=
  match result with
  | .ok resource => resource.release
  | .error _ | .interrupted => HEIO.pure ()

private def releaseParallelResults
    (left : HEIO.Result (Cause E) (Resource E A))
    (right : HEIO.Result (Cause E) (Resource E B)) :
    HEIO (Cause E) Unit :=
  (releaseResult right).ensuringCause (releaseResult left)

private def finishParallel
    (first : ParallelCompletion)
    (left : HEIO.Result (Cause E) (Resource E A))
    (right : HEIO.Result (Cause E) (Resource E B))
    (f : A -> B -> C) : HEIO (Cause E) (Resource E C) :=
  match left, right with
  | .ok acquiredLeft, .ok acquiredRight =>
      HEIO.pure {
        value := f acquiredLeft.value acquiredRight.value
        release := acquiredRight.release.ensuringCause acquiredLeft.release
      }
  | .error leftCause, .error rightCause =>
      HEIO.throw (.parallel leftCause rightCause)
  | _, _ =>
      let release := releaseParallelResults left right
      match first with
      | .leftFailure =>
          match left with
          | .error cause => (HEIO.throw cause).ensuringCause release
          | _ => (HEIO.interrupt :
              HEIO (Cause E) (Resource E C)).ensuringCause release
      | .rightFailure =>
          match right with
          | .error cause => (HEIO.throw cause).ensuringCause release
          | _ => (HEIO.interrupt :
              HEIO (Cause E) (Resource E C)).ensuringCause release
      | .leftInterrupted | .rightInterrupted =>
          (HEIO.interrupt :
            HEIO (Cause E) (Resource E C)).ensuringCause release
      | .leftSuccess =>
          match right with
          | .error cause => (HEIO.throw cause).ensuringCause release
          | _ => (HEIO.interrupt :
              HEIO (Cause E) (Resource E C)).ensuringCause release
      | .rightSuccess =>
          match left with
          | .error cause => (HEIO.throw cause).ensuringCause release
          | _ => (HEIO.interrupt :
              HEIO (Cause E) (Resource E C)).ensuringCause release

/--
Build two independent layers in parallel and combine their outputs. A failure
or interruption cancels the other branch before acquired resources release.
-/
def zipWithPar
    (left : Layer R E A)
    (right : Layer R E B)
    (f : A -> B -> C) : Layer R E C :=
  ⟨fun environment =>
    HEIO.withChildInterruption fun interruption =>
      HEIO.bind
        (HEIO.liftBaseIO.{0}
          (IO.Promise.new (α := ParallelCompletion)))
        fun completionLift =>
          let completion := completionLift.down
          let observedLeft := observeParallel completion
            .leftSuccess .leftFailure .leftInterrupted
            (left.build environment)
          let observedRight := observeParallel completion
            .rightSuccess .rightFailure .rightInterrupted
            (right.build environment)
          HEIO.bind (HEIO.fork observedLeft) fun leftTask =>
            HEIO.bind (HEIO.fork observedRight) fun rightTask =>
              HEIO.bind (waitParallel completion) fun first =>
                let cancelSibling : HEIO (Cause E) Unit :=
                  match first with
                  | .leftSuccess | .rightSuccess => HEIO.pure ()
                  | _ =>
                      HEIO.bind
                        (HEIO.liftIO.{0} Cause.die interruption.request)
                        fun _ => HEIO.pure ()
                HEIO.bind cancelSibling fun _ =>
                  HEIO.bind (HEIO.wait leftTask) fun leftResult =>
                    HEIO.bind (HEIO.wait rightTask) fun rightResult =>
                      finishParallel first leftResult rightResult f⟩

/-- Build a pure, infallible layer from its input environment. -/
def fromFunction (f : R -> A) : Layer R Empty A :=
  fromBuild fun environment => HEIO.pure (f environment)

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
        | .success value => HEIO.pure value
        | .failure cause => HEIO.throw cause

/-- Build a low-universe layer output with a normal Zenith effect. -/
def fromEffect
    (effect : Z R E A) : Layer R E A :=
  fromBuild fun environment => runZ "layer" effect environment

/--
Acquire a low-universe service with `Z` and release it when the layer scope
closes. The release effect cannot have a typed failure.
-/
def acquireReleaseEffect
    (acquire : Z R E A)
    (release : A -> Z R Empty Unit) : Layer R E A :=
  acquireRelease
    (runZ "layer-acquire" acquire)
    (fun environment value =>
      (runZ "layer-release" (release value) environment).mapError
        (Cause.map impossible))

/-- Build a low-universe service environment with a Zenith effect. -/
def fromEnvironment
    (effect : Z R E (Environment A)) : Layer R E A :=
  fromEffect effect

/--
Build a layer, supply its service to a program, and send execution events to
`diagram`.
-/
def runWithDiagram.{uin, uout}
    (self : Layer.{uin, uout} RIn E ROut)
    (input : RIn)
    (program : Z ROut E A)
    (diagram : ExecutionDiagram (IO Unit))
    (fiberId : FiberId := "main") : IO (Exit E A) := do
  let builtAndRun : HEIO (Cause E) (ULift.{uout} (Exit E A)) :=
    HEIO.bind (self.build input) fun resource =>
      let runProgram :=
        HEIO.bind
          (HEIO.liftIO.{uout} Cause.die <|
            Z.unsafeRunSyncWithDiagram
              (program.provideEnvironment resource.value)
              diagram
              fiberId)
          fun result =>
            match result.down with
            | .success _ => HEIO.pure result
            | .failure cause => HEIO.throw cause
      runProgram.ensuringCause resource.release
  match <- HEIO.toIOResult builtAndRun with
  | .ok result => pure result
  | .error cause => pure (.failure cause)

/--
Build a layer, supply its service to a program, and run the resulting deep
instruction tree without visual tracing.
-/
def run.{uin, uout}
    (self : Layer.{uin, uout} RIn E ROut)
    (input : RIn)
    (program : Z ROut E A)
    (fiberId : FiberId := "main") : IO (Exit E A) :=
  runWithDiagram self input program ExecutionDiagram.empty fiberId

end Layer
