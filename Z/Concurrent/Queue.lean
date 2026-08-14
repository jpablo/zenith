import Init.Data.Queue
import Z.Combinators

/-!
FIFO, multi-producer multi-consumer queues for Zenith fibers.

Both `take` and a blocked bounded `offer` are interruption-aware. Cancellation
removes the pending waiter before a later queue operation can consume it.
-/

namespace Z

private structure Taker (A : Type) where
  id : Nat
  observer : Observer Empty A

private structure Offerer (A : Type) where
  id : Nat
  value : A
  observer : Observer Empty Bool

private inductive QueueState (A : Type)
  | open
      (capacity : Option Nat)
      (values : Std.Queue A)
      (valueCount : Nat)
      (takers : List (Taker A))
      (offerers : List (Offerer A))
  | shutdown

/-- A FIFO queue with interruption-aware waiting producers and consumers. -/
structure Queue (A : Type) where
  private state : IO.Ref (QueueState A)
  private nextWaiterId : IO.Ref Nat

namespace Queue

private def notify
    (observer : Observer Empty B)
    (exit : Exit Empty B) : IO Unit :=
  try observer exit
  catch _ => pure ()

private def hasSpace (capacity : Option Nat) (valueCount : Nat) : Bool :=
  match capacity with
  | none => true
  | some limit => valueCount < limit

private def removeTaker (self : Queue A) (takerId : Nat) : IO Unit :=
  self.state.modify fun state =>
    match state with
    | .open capacity values valueCount takers offerers =>
        .open capacity values valueCount
          (takers.filter fun taker => taker.id != takerId)
          offerers
    | .shutdown => .shutdown

private def removeOfferer (self : Queue A) (offererId : Nat) : IO Unit :=
  self.state.modify fun state =>
    match state with
    | .open capacity values valueCount takers offerers =>
        .open capacity values valueCount takers
          (offerers.filter fun offerer => offerer.id != offererId)
    | .shutdown => .shutdown

private def registerTaker
    (self : Queue A)
    (observer : Observer Empty A) : IO (IO Unit) := do
  let takerId ← self.nextWaiterId.modifyGet fun next => (next, next + 1)
  let (exit?, acceptedOfferer?) ← self.state.modifyGet fun state =>
    match state with
    | .shutdown => ((some (.failure .interrupt), none), .shutdown)
    | .open capacity values valueCount takers offerers =>
        match values.dequeue? with
        | some (value, remainingValues) =>
            match offerers with
            | [] =>
                ((some (.success value), none),
                  .open capacity remainingValues (valueCount - 1) takers [])
            | offerer :: remainingOfferers =>
                ((some (.success value), some offerer),
                  .open capacity (remainingValues.enqueue offerer.value) valueCount
                    takers remainingOfferers)
        | none =>
            match offerers with
            | offerer :: remainingOfferers =>
                ((some (.success offerer.value), some offerer),
                  .open capacity values valueCount takers remainingOfferers)
            | [] =>
                ((none, none),
                  .open capacity values valueCount
                    (takers ++ [{ id := takerId, observer }]) [])
  match exit? with
  | none => pure <| self.removeTaker takerId
  | some exit =>
      notify observer exit
      match acceptedOfferer? with
      | none => pure ()
      | some offerer => notify offerer.observer (.success true)
      pure IO.unit

private def registerOffer
    (self : Queue A)
    (value : A)
    (observer : Observer Empty Bool) : IO (IO Unit) := do
  let offererId ← self.nextWaiterId.modifyGet fun next => (next, next + 1)
  let (accepted?, taker?) ← self.state.modifyGet fun state =>
    match state with
    | .shutdown => ((some false, none), .shutdown)
    | .open capacity values valueCount [] offerers =>
        if hasSpace capacity valueCount then
          ((some true, none),
            .open capacity (values.enqueue value) (valueCount + 1) [] offerers)
        else
          ((none, none),
            .open capacity values valueCount []
              (offerers ++ [{ id := offererId, value, observer }]))
    | .open capacity values valueCount (taker :: remainingTakers) offerers =>
        ((some true, some taker),
          .open capacity values valueCount remainingTakers offerers)
  match accepted? with
  | none => pure <| self.removeOfferer offererId
  | some accepted =>
      notify observer (.success accepted)
      match taker? with
      | none => pure ()
      | some taker => notify taker.observer (.success value)
      pure IO.unit

private def pollValue (self : Queue A) : IO (Option A) := do
  let (value?, acceptedOfferer?) ← self.state.modifyGet fun state =>
    match state with
    | .shutdown => ((none, none), .shutdown)
    | .open capacity values valueCount takers offerers =>
        match values.dequeue? with
        | some (value, remainingValues) =>
            match offerers with
            | [] =>
                ((some value, none),
                  .open capacity remainingValues (valueCount - 1) takers [])
            | offerer :: remainingOfferers =>
                ((some value, some offerer),
                  .open capacity (remainingValues.enqueue offerer.value) valueCount
                    takers remainingOfferers)
        | none =>
            match offerers with
            | offerer :: remainingOfferers =>
                ((some offerer.value, some offerer),
                  .open capacity values valueCount takers remainingOfferers)
            | [] => ((none, none), state)
  match acceptedOfferer? with
  | none => pure ()
  | some offerer => notify offerer.observer (.success true)
  pure value?

private def shutdownQueue
    (self : Queue A) : IO (List (Taker A) × List (Offerer A)) :=
  self.state.modifyGet fun state =>
    match state with
    | .shutdown => (([], []), .shutdown)
    | .open _ _ _ takers offerers => ((takers, offerers), .shutdown)

private def make (capacity : Option Nat) : UIO (Queue A) :=
  Z.fromIO (do
    pure {
      state := ← IO.mkRef (.open capacity Std.Queue.empty 0 [] [])
      nextWaiterId := ← IO.mkRef 0
    })

/-- Allocate an unbounded FIFO queue. -/
def unbounded : UIO (Queue A) :=
  make none |>.withLabel "Queue.unbounded"

/--
Allocate a FIFO queue that holds at most `capacity` queued values.

`capacity = 0` creates a rendezvous queue: an offer waits for a taker.
-/
def bounded (capacity : Nat) : UIO (Queue A) :=
  make (some capacity) |>.withLabel "Queue.bounded"

/--
Offer a value. An unbounded queue accepts it immediately. A bounded queue
waits until it has capacity or a taker is ready. Returns `false` after queue
shutdown.
-/
def offer (self : Queue A) (value : A) : UIO Bool :=
  Z.asyncInterrupt (self.registerOffer value) |>.withLabel "Queue.offer"

/-- Take the next value, waiting interruptibly while the queue is empty. -/
def take (self : Queue A) : UIO A :=
  Z.asyncInterrupt self.registerTaker |>.withLabel "Queue.take"

/-- Take a value if one is immediately available. -/
def poll (self : Queue A) : UIO (Option A) :=
  Z.fromIO (self.pollValue) |>.withLabel "Queue.poll"

/--
Shut down the queue. This discards queued values, interrupts pending takers,
and makes pending and future offers return `false`.
-/
def shutdown (self : Queue A) : UIO Unit :=
  Z.fromIO (do
    let (takers, offerers) ← self.shutdownQueue
    for taker in takers do
      notify taker.observer (.failure .interrupt)
    for offerer in offerers do
      notify offerer.observer (.success false)) |>.withLabel "Queue.shutdown"

/-- Report whether the queue has been shut down. -/
def isShutdown (self : Queue A) : UIO Bool :=
  Z.fromIO (do
    match ← self.state.get with
    | .shutdown => pure true
    | .open .. => pure false) |>.withLabel "Queue.isShutdown"

end Queue
end Z
