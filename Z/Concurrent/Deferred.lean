import Z.Combinators

/-!
An interruption-aware, one-shot result cell.

`Deferred E A` completes once with an `Exit E A`. Awaiters may be interrupted;
their callback is then removed while the cell is still pending.
-/

private inductive DeferredState (E A : Type)
  | pending (observers : List (Nat × Observer E A))
  | done (exit : Exit E A)

/-- A one-shot result cell that many Zenith fibers may await. -/
structure Deferred (E A : Type) where
  private state : IO.Ref (DeferredState E A)
  private nextObserverId : IO.Ref Nat

namespace Deferred

private def removeObserver (self : Deferred E A) (observerId : Nat) : IO Unit :=
  self.state.modify fun state =>
    match state with
    | .pending observers =>
        .pending <| observers.filter fun (id, _) => id != observerId
    | done => done

private def register
    (self : Deferred E A)
    (observer : Observer E A) : IO (IO Unit) := do
  let observerId ← self.nextObserverId.modifyGet fun next => (next, next + 1)
  let completed? ← self.state.modifyGet fun state =>
    match state with
    | .pending observers =>
        (none, .pending ((observerId, observer) :: observers))
    | .done exit => (some exit, state)
  match completed? with
  | none => pure <| self.removeObserver observerId
  | some exit =>
      try observer exit
      catch _ => pure ()
      pure IO.unit

private def completeExit (self : Deferred E A) (exit : Exit E A) : IO Bool := do
  let observers? ← self.state.modifyGet fun state =>
    match state with
    | .pending observers => (some observers, .done exit)
    | done => (none, done)
  match observers? with
  | none => pure false
  | some observers =>
      for (_, observer) in observers do
        try observer exit
        catch _ => pure ()
      pure true

/-- Allocate an unresolved result cell. -/
def make : UIO (Deferred E A) :=
  Z.fromIO (do
    pure {
      state := ← IO.mkRef (.pending [])
      nextObserverId := ← IO.mkRef 0
    }) |>.withLabel "Deferred.make"

/-- Observe the completed exit, if the cell already has one. -/
def poll (self : Deferred E A) : UIO (Option (Exit E A)) :=
  Z.fromIO (do
    match ← self.state.get with
    | .pending _ => pure none
    | .done exit => pure (some exit)) |>.withLabel "Deferred.poll"

/-- Complete the cell with an explicit exit. Only the first completion wins. -/
def done (self : Deferred E A) (exit : Exit E A) : UIO Bool :=
  Z.fromIO (self.completeExit exit) |>.withLabel "Deferred.done"

/-- Complete the cell successfully. Only the first completion wins. -/
def succeed (self : Deferred E A) (value : A) : UIO Bool :=
  self.done (.success value) |>.withLabel "Deferred.succeed"

/-- Complete the cell with a typed failure. Only the first completion wins. -/
def fail (self : Deferred E A) (error : E) : UIO Bool :=
  self.done (.failure (.fail error)) |>.withLabel "Deferred.fail"

/-- Complete the cell with a structured failure. Only the first completion wins. -/
def failCause (self : Deferred E A) (cause : Cause E) : UIO Bool :=
  self.done (.failure cause) |>.withLabel "Deferred.failCause"

/-- Complete the cell with a defect. Only the first completion wins. -/
def die (self : Deferred E A) (error : IO.Error) : UIO Bool :=
  self.done (.failure (.die error)) |>.withLabel "Deferred.die"

/-- Complete the cell with interruption. Only the first completion wins. -/
def interrupt (self : Deferred E A) : UIO Bool :=
  self.done (.failure .interrupt) |>.withLabel "Deferred.interrupt"

/--
Run `effect`, then use its exit to complete the cell. Only the first completion
wins; the effect's failure becomes the completed exit rather than this
operation's error.
-/
def complete (self : Deferred E A) (effect : Z R E A) : Z R Empty Bool :=
  effect.exit.flatMap fun exit =>
    (self.done exit).contramap fun _ : R => ()

/-- Wait for the cell. Interruption removes the pending awaiter callback. -/
def await (self : Deferred E A) : Z Unit E A :=
  Z.asyncInterrupt self.register |>.withLabel "Deferred.await"

end Deferred
