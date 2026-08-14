import Z.Concurrent.Queue
import Z.Syntax.Do

/-!
A small continuation-based stream API for Zenith.

Each `runForeach` invocation owns its mutable state. Buffered producers and
parallel workers are finalizers of that invocation, so they stop when the
downstream consumer completes, fails, or is interrupted.
-/

namespace Z

/-- A repeatable source of typed effectful values. -/
structure Stream (R E A : Type) where
  private run : (A -> Z R E Unit) -> Z R E Unit

namespace Stream

private abbrev FailureRef (E : Type) := IO.Ref (Option (Cause E))

private def queueOffer
    (queue : Queue A)
    (value : A) : Z R E Bool :=
  (queue.offer value).contramap (fun _ : R => ()) |>.mapFailure Empty.elim

private def queueTake (queue : Queue A) : Z R E A :=
  (queue.take).contramap (fun _ : R => ()) |>.mapFailure Empty.elim

private def queueShutdown (queue : Queue A) : Z R E Unit :=
  queue.shutdown.contramap (fun _ : R => ()) |>.mapFailure Empty.elim

private def boundedQueue (capacity : Nat) : Z R E (Queue A) :=
  (Queue.bounded capacity).contramap (fun _ : R => ()) |>.mapFailure Empty.elim

private def recordFailure
    (failure : FailureRef E)
    (cause : Cause E) : Z R E Unit :=
  Z.internal.succeed <| failure.modify fun current =>
    match current with
    | none => some cause
    | some recorded => some recorded

private def rethrowFailure
    (failure : FailureRef E)
    (fallback : Cause E) : Z R E A :=
  Z.flatMapIO failure.get fun
    | some recorded => (Z.failCause (R := R) recorded).map impossible
    | none => (Z.failCause (R := R) fallback).map impossible

private def joinFiber (fiber : Fiber E A) : Z R E A :=
  (Z.async fun observer => fiber.awaitAsync observer)
    |>.contramap (fun _ : R => ())

private def stopFiber (fiber : Fiber E A) : IO Unit := do
  fiber.requestInterrupt
  try
    let _ ← fiber.await
    pure ()
  catch _ => pure ()

private def stopFibers (fibers : List (Fiber E A)) : IO Unit := do
  for fiber in fibers do
    fiber.requestInterrupt
  for fiber in fibers do
    try
      let _ ← fiber.await
      pure ()
    catch _ => pure ()

private partial def unfoldLoop
    (state : IO.Ref (Option S))
    (step : S -> Z R E (Option (A × S)))
    (consume : A -> Z R E Unit) : Z R E Unit :=
  Z.flatMapIO state.get fun
    | none => pure ()
    | some current =>
        step current |>.flatMap fun
          | none => Z.internal.succeed (state.set none)
          | some (value, next) =>
              Z.internal.succeed (state.set (some next)) *>
                consume value *>
                unfoldLoop state step consume

/-- Create a stream from an effectful state transition. -/
def unfold
    (initial : S)
    (step : S -> Z R E (Option (A × S))) : Stream R E A := {
  run := fun consume =>
    Z.flatMapIO (IO.mkRef (some initial)) fun state =>
      unfoldLoop state step consume
}

/-- Create a stream from a finite list. -/
def fromList (values : List A) : Stream Unit Empty A :=
  unfold values fun remaining =>
    match remaining with
    | [] => pure none
    | value :: tail => pure (some (value, tail))

/-- Transform each stream value with an effect. -/
def map
    (self : Stream R E A)
    (transform : A -> Z R E B) : Stream R E B := {
  run := fun consume =>
    self.run fun value =>
      (transform value).flatMap consume
}

/-- Keep only values that satisfy `predicate`. -/
def filter
    (self : Stream R E A)
    (predicate : A -> Bool) : Stream R E A := {
  run := fun consume =>
    self.run fun value =>
      if predicate value then consume value else pure ()
}

/-- Run `consume` for every stream value. -/
def runForeach
    (self : Stream R E A)
    (consume : A -> Z R E Unit) : Z R E Unit :=
  self.run consume

/-- Collect all stream values in source order. -/
def runCollect (self : Stream R E A) : Z R E (List A) :=
  Z.flatMapIO (IO.mkRef ([] : List A)) fun values =>
    (self.runForeach fun value =>
      Z.internal.succeed <| values.modify (value :: ·))
      |>.flatMap fun _ =>
        Z.flatMapIO values.get fun collected => pure collected.reverse

private partial def bufferSource
    (source : Stream R E A)
    (queue : Queue (Option A)) : Z R E Unit :=
  source.runForeach fun value =>
    queueOffer queue (some value) |>.flatMap fun accepted =>
      if accepted then pure ()
      else (Z.failCause (R := R) (.interrupt)).map impossible

private def bufferProducer
    (source : Stream R E A)
    (queue : Queue (Option A))
    (failure : FailureRef E) : Z R E Unit :=
  ((bufferSource source queue).flatMap fun _ =>
    (queueOffer queue none).map fun _ => ())
    |>.foldCauseM
      (fun cause =>
        recordFailure failure cause *>
          queueShutdown queue *>
          (Z.failCause (R := R) cause).map impossible)
      pure

private partial def drainQueue
    (queue : Queue (Option A))
    (failure : FailureRef E)
    (consume : A -> Z R E Unit) : Z R E Unit :=
  (queueTake queue).foldCauseM
    (rethrowFailure failure)
    (fun
      | none => pure ()
      | some value => consume value *> drainQueue queue failure consume)

/--
Run the upstream stream in a background fiber and keep at most `capacity`
values ahead of its consumer.
-/
def buffer (self : Stream R E A) (capacity : Nat) : Stream R E A := {
  run := fun consume =>
    (boundedQueue (R := R) (E := E) (A := Option A) capacity).flatMap fun queue =>
      Z.flatMapIO (IO.mkRef (none : Option (Cause E))) fun failure =>
        ((bufferProducer self queue failure).fork "stream-buffer-producer")
          |>.mapFailure Empty.elim
          |>.flatMap fun producer =>
            (drainQueue queue failure consume).ensuring
              (Z.internal.succeed (stopFiber producer))
}

private def appendMapFiber
    (pending : IO.Ref (List (Fiber E B)))
    (fiber : Fiber E B)
    (parallelism : Nat) : Z R E Bool :=
  Z.internal.succeed <| pending.modifyGet fun current =>
    let next := current ++ [fiber]
    (next.length >= parallelism, next)

private partial def joinMapFibers
    (fibers : List (Fiber E B))
    (consume : B -> Z R E Unit) : Z R E Unit :=
  match fibers with
  | [] => pure ()
  | fiber :: remaining =>
      (joinFiber fiber).flatMap fun value =>
        consume value *> joinMapFibers remaining consume

private def drainMapFibers
    (pending : IO.Ref (List (Fiber E B)))
    (consume : B -> Z R E Unit) : Z R E Unit :=
  Z.flatMapIO pending.get fun fibers =>
    (joinMapFibers fibers consume).flatMap fun _ =>
      Z.internal.succeed (pending.set [])

/--
Transform values in source order with batches of at most `parallelism` active
effects. A zero parallelism uses one worker.
-/
def mapPar
    (self : Stream R E A)
    (parallelism : Nat)
    (transform : A -> Z R E B) : Stream R E B := {
  run := fun consume =>
    let parallelism := max parallelism 1
    Z.flatMapIO (IO.mkRef ([] : List (Fiber E B))) fun pending =>
      let schedule : A -> Z R E Unit := fun value =>
        ((transform value).fork "stream-mapPar")
          |>.mapFailure Empty.elim
          |>.flatMap fun fiber =>
            (appendMapFiber pending fiber parallelism).flatMap fun full =>
              if full then drainMapFibers pending consume else pure ()
      let cleanup : Z R Empty Unit :=
        Z.flatMapIO pending.get fun fibers =>
          Z.internal.succeed (stopFibers fibers)
      ((self.runForeach schedule) *> drainMapFibers pending consume)
        |>.ensuring cleanup
}

end Stream
end Z
