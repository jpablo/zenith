import Init.Data.Queue
import Z.Combinators

/-!
An unbounded, multi-producer multi-consumer queue for Zenith fibers.

`take` is interruption-aware: cancellation removes a pending taker before a
later `offer` can consume a value for it.
-/

namespace Z

private inductive QueueState (A : Type)
  | open (values : Std.Queue A) (takers : List (Nat × Observer Empty A))
  | shutdown

/-- An unbounded, interruption-aware FIFO queue. -/
structure Queue (A : Type) where
  private state : IO.Ref (QueueState A)
  private nextTakerId : IO.Ref Nat

namespace Queue

private def notify (observer : Observer Empty A) (exit : Exit Empty A) : IO Unit :=
  try observer exit
  catch _ => pure ()

private def removeTaker (self : Queue A) (takerId : Nat) : IO Unit :=
  self.state.modify fun state =>
    match state with
    | .open values takers =>
        .open values <| takers.filter fun (id, _) => id != takerId
    | .shutdown => .shutdown

private def registerTaker
    (self : Queue A)
    (observer : Observer Empty A) : IO (IO Unit) := do
  let takerId ← self.nextTakerId.modifyGet fun next => (next, next + 1)
  let exit? ← self.state.modifyGet fun state =>
    match state with
    | .shutdown => (some (.failure .interrupt), .shutdown)
    | .open values takers =>
        match values.dequeue? with
        | some (value, remaining) =>
            (some (.success value), .open remaining takers)
        | none => (none, .open values (takers ++ [(takerId, observer)]))
  match exit? with
  | none => pure <| self.removeTaker takerId
  | some exit =>
      notify observer exit
      pure IO.unit

private def offerValue (self : Queue A) (value : A) : IO Bool := do
  let (accepted, taker?) ← self.state.modifyGet fun state =>
    match state with
    | .shutdown => ((false, none), .shutdown)
    | .open values [] => ((true, none), .open (values.enqueue value) [])
    | .open values ((_, taker) :: remaining) =>
        ((true, some taker), .open values remaining)
  match taker? with
  | none => pure accepted
  | some taker =>
      notify taker (.success value)
      pure accepted

private def pollValue (self : Queue A) : IO (Option A) :=
  self.state.modifyGet fun state =>
    match state with
    | .shutdown => (none, .shutdown)
    | .open values takers =>
        match values.dequeue? with
        | none => (none, state)
        | some (value, remaining) => (some value, .open remaining takers)

private def shutdownQueue (self : Queue A) : IO (List (Observer Empty A)) :=
  self.state.modifyGet fun state =>
    match state with
    | .shutdown => ([], .shutdown)
    | .open _ takers => (takers.map Prod.snd, .shutdown)

/-- Allocate an unbounded FIFO queue. -/
def unbounded : UIO (Queue A) :=
  Z.succeed (do
    pure {
      state := ← IO.mkRef (.open Std.Queue.empty [])
      nextTakerId := ← IO.mkRef 0
    }) |>.withLabel "Queue.unbounded"

/-- Offer a value. Returns `false` only after queue shutdown. -/
def offer (self : Queue A) (value : A) : UIO Bool :=
  Z.succeed (self.offerValue value) |>.withLabel "Queue.offer"

/-- Take the next value, waiting interruptibly while the queue is empty. -/
def take (self : Queue A) : UIO A :=
  Z.asyncInterrupt self.registerTaker |>.withLabel "Queue.take"

/-- Take a value if one is immediately available. -/
def poll (self : Queue A) : UIO (Option A) :=
  Z.succeed (self.pollValue) |>.withLabel "Queue.poll"

/-- Shut down the queue and interrupt all pending takers. -/
def shutdown (self : Queue A) : UIO Unit :=
  Z.succeed (do
    let takers ← self.shutdownQueue
    for taker in takers do
      notify taker (.failure .interrupt)) |>.withLabel "Queue.shutdown"

/-- Report whether the queue has been shut down. -/
def isShutdown (self : Queue A) : UIO Bool :=
  Z.succeed (do
    match ← self.state.get with
    | .shutdown => pure true
    | .open .. => pure false) |>.withLabel "Queue.isShutdown"

end Queue
end Z
