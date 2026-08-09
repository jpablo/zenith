import Z.Cause

/-!
`HEIO` has the real-world sequencing model of Lean's `IO`, but its error and
success values can live in independent universes.

Standard `IO` actions can enter `HEIO` through `liftIO`. A computation can
return to standard `IO` only after its final value is low-universe.
-/

inductive HEIO.Out.{ue, ua}
    (E : Type ue)
    (A : Type ua) : Type (max ue ua) where
  | ok : A -> Void IO.RealWorld -> HEIO.Out E A
  | error : E -> Void IO.RealWorld -> HEIO.Out E A

def HEIO.{ue, ua}
    (E : Type ue)
    (A : Type ua) : Type (max ue ua) :=
  Void IO.RealWorld -> HEIO.Out E A

namespace HEIO

def pure (value : A) : HEIO E A :=
  fun world => .ok value world

def bind
    (self : HEIO E A)
    (next : A -> HEIO E B) : HEIO E B :=
  fun world =>
    match self world with
    | .ok value world => next value world
    | .error error world => .error error world

def throw (error : E) : HEIO E A :=
  fun world => .error error world

def tryCatch
    (self : HEIO E A)
    (handler : E -> HEIO E A) : HEIO E A :=
  fun world =>
    match self world with
    | .ok value world => .ok value world
    | .error error world => handler error world

def mapError
    (f : E -> E₁)
    (self : HEIO E A) : HEIO E₁ A :=
  fun world =>
    match self world with
    | .ok value world => .ok value world
    | .error error world => .error (f error) world

instance : Monad (HEIO E) where
  pure := pure
  bind := bind

instance : MonadExceptOf E (HEIO E) where
  throw := throw
  tryCatch := tryCatch

/-- Run a standard `IO` action inside a selected `HEIO` universe. -/
def liftIO.{u}
    (mapError : IO.Error -> E)
    (action : IO A) : HEIO E (ULift.{u} A) :=
  fun world =>
    match action world with
    | .ok value world => .ok (ULift.up value) world
    | .error error world => .error (mapError error) world

/--
Return to standard `IO` after the final result has returned to `Type`.
-/
def toIO.{u}
    (mapError : E -> IO.Error)
    (action : HEIO E (ULift.{u} A)) : IO A :=
  fun world =>
    match action world with
    | .ok value world => .ok value.down world
    | .error error world => .error (mapError error) world

/--
Return both success and failure as a standard low-universe `IO` value.
-/
def toIOResult.{u}
    (action : HEIO E (ULift.{u} A)) : IO (Except E A) :=
  fun world =>
    match action world with
    | .ok value world => .ok (.ok value.down) world
    | .error error world => .ok (.error error) world

end HEIO
