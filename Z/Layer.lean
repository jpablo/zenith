import Z.Interpreter
import Z.HEIO

/-!
A layer builds a service inside `HEIO`. The input and output can live in any
universe. Layer failures retain the complete `Cause E` value.
-/

structure Layer.{uin, uout}
    (RIn : Type uin)
    (E : Type)
    (ROut : Type uout) : Type (max uin uout) where
  build : RIn -> HEIO (Cause E) ROut

namespace Layer

def fromHEIO
    (build : RIn -> HEIO (Cause E) ROut) :
    Layer RIn E ROut :=
  ⟨build⟩

def succeed (value : A) : Layer Unit Empty A :=
  fromHEIO fun _ => HEIO.pure value

def succeedEnvironment
    (environment : Environment A) : Layer Unit Empty A :=
  succeed environment

def failCause (cause : Cause E) : Layer R E A :=
  fromHEIO fun _ => HEIO.throw cause

def suspend
    (layer : Thunk (Layer R E A)) : Layer R E A :=
  fromHEIO fun environment => layer.get.build environment

def contramap
    (f : R₀ -> R)
    (self : Layer R E A) : Layer R₀ E A :=
  fromHEIO fun environment => self.build (f environment)

instance [conversion : R₀ <: R] :
    (Layer R E A) <: (Layer R₀ E A) :=
  ⟨contramap conversion.coe⟩

def mapError
    (f : E -> E₁)
    (self : Layer R E A) : Layer R E₁ A :=
  fromHEIO fun environment =>
    (self.build environment).mapError (Cause.map f)

instance [conversion : E <: E₁] :
    (Layer R E A) <: (Layer R E₁ A) :=
  ⟨mapError conversion.coe⟩

def flatMap
    (self : Layer R E A)
    (next : A -> Layer R E B) : Layer R E B :=
  fromHEIO fun environment =>
    HEIO.bind (self.build environment) fun value =>
      (next value).build environment

def map
    (self : Layer R E A)
    (f : A -> B) : Layer R E B :=
  self.flatMap fun value =>
    fromHEIO fun _ => HEIO.pure (f value)

/-- Feed the output of one layer into the next layer. -/
def to
    (self : Layer R E A)
    (next : Layer A E B) : Layer R E B :=
  fromHEIO fun environment =>
    HEIO.bind (self.build environment) next.build

/--
Build two layers and combine their outputs. This first implementation is
sequential. It can become parallel after `HEIO` has high-universe task support.
-/
def zipWith
    (left : Layer R E A)
    (right : Layer R E B)
    (f : A -> B -> C) : Layer R E C :=
  fromHEIO fun environment =>
    HEIO.bind (left.build environment) fun a =>
      HEIO.bind (right.build environment) fun b =>
        HEIO.pure (f a b)

def fromFunction (f : R -> A) : Layer R Empty A :=
  fromHEIO fun environment => HEIO.pure (f environment)

/-- Build a low-universe layer output with a normal Zenith effect. -/
def fromZ
    (effect : Z R E A) : Layer R E A :=
  Layer.fromHEIO fun environment =>
    HEIO.bind
      (HEIO.liftIO.{0} Cause.die <|
        Z.unsafeRunSync
          (effect.provideEnvironment environment)
          "layer")
      fun result =>
        match result.down with
        | some (.success value) => HEIO.pure value
        | some (.failure cause) => HEIO.throw cause
        | none =>
            HEIO.throw <| .die <|
              IO.userError "the layer fiber did not return a result"

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
    HEIO.bind (self.build input) fun environment =>
      HEIO.liftIO.{uout} Cause.die <|
        Z.unsafeRunSync
          (program.provideEnvironment environment)
          fiberId
          useDiagram
  match <- HEIO.toIOResult builtAndRun with
  | .ok result => pure result
  | .error cause => pure (some (.failure cause))

end Layer
