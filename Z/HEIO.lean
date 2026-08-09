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
  fun world =>
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
    (action : HEIO E A) : HST (Except E A) :=
  fun world =>
    match action world with
    | .ok value world => .mk (.ok value) world
    | .error error world => .mk (.error error) world

/-- Start an `HEIO` action in a runtime task. -/
def fork
    (action : HEIO E A)
    (priority := Task.Priority.default) :
    HEIO E (Task (Except E A)) :=
  liftHST (Prim.asTask (toHSTResult action) priority)

/-- Wait for a task that was created by `HEIO.fork`. -/
def wait (task : Task A) : HEIO E A :=
  liftHST (Prim.wait task)

def pure (value : A) : HEIO E A :=
  fun world => .ok value world

def bind
    (self : HEIO E A)
    (next : A -> HEIO E B) : HEIO E B :=
  fun world =>
    match self world with
    | .ok value world => next value world
    | .error error world => .error error world

def map
    (f : A -> B)
    (self : HEIO E A) : HEIO E B :=
  bind self fun value => pure (f value)

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

/-- Handle both outcomes and permit the result universe and error type to change. -/
def fold
    (self : HEIO E A)
    (failure : E -> HEIO E₁ B)
    (success : A -> HEIO E₁ B) : HEIO E₁ B :=
  fun world =>
    match self world with
    | .ok value world => success value world
    | .error error world => failure error world

/--
Run `finalizer` after either outcome. If both actions fail, the finalizer
failure is returned. This is the same policy that `Z.ensuring` uses.
-/
def ensuring
    (self : HEIO E A)
    (finalizer : HEIO E Unit) : HEIO E A :=
  fun world =>
    match self world with
    | .ok value world =>
        match finalizer world with
        | .ok _ world => .ok value world
        | .error error world => .error error world
    | .error originalError world =>
        match finalizer world with
        | .ok _ world => .error originalError world
        | .error finalizerError world => .error finalizerError world

instance : Monad (HEIO E) where
  pure := pure
  bind := bind

instance : MonadExceptOf E (HEIO E) where
  throw := throw
  tryCatch := tryCatch

/-- Run an exception-free standard action inside a selected `HEIO` universe. -/
def liftBaseIO.{u}
    (action : BaseIO A) : HEIO E (ULift.{u} A) :=
  fun world =>
    match action world with
    | .mk value world => .ok (ULift.up value) world

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
