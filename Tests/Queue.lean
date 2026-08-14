import Z
import Tests.Support

/-!
Regression tests for the unbounded, interruption-aware Zenith queue.
-/

private def makeQueue : IO (Z.Queue Nat) := do
  match ← Z.unsafeRunSync Z.Queue.unbounded "queue-make" with
  | .success queue => pure queue
  | .failure cause => failTest s!"Queue.unbounded failed: {cause}"

private def makeBoundedQueue (capacity : Nat) : IO (Z.Queue Nat) := do
  match ← Z.unsafeRunSync (Z.Queue.bounded capacity) "queue-make-bounded" with
  | .success queue => pure queue
  | .failure cause => failTest s!"Queue.bounded failed: {cause}"

def testQueuePreservesFIFOOrder : IO Unit := do
  let queue ← makeQueue
  match ← Z.unsafeRunSync (queue.offer 1) "queue-offer-first" with
  | .success true => pure ()
  | _ => failTest "Queue.offer rejected its first value"
  match ← Z.unsafeRunSync (queue.offer 2) "queue-offer-second" with
  | .success true => pure ()
  | _ => failTest "Queue.offer rejected its second value"
  match ← Z.unsafeRunSync queue.take "queue-take-first" with
  | .success 1 => pure ()
  | _ => failTest "Queue.take did not return the first value"
  match ← Z.unsafeRunSync queue.take "queue-take-second" with
  | .success 2 => pure ()
  | _ => failTest "Queue.take did not preserve FIFO order"
  match ← Z.unsafeRunSync queue.poll "queue-poll-empty" with
  | .success none => pure ()
  | _ => failTest "Queue.poll did not report an empty queue"

/-- Adapted from ZIO `QueueSpec`: a pending take receives later offers. -/
def testQueueTransfersToWaitingTaker : IO Unit := do
  let queue ← makeQueue
  let started ← IO.mkRef false
  let taker ← Z.unsafeFork
    ((Z.fromIO (started.set true)) *> queue.take) "queue-waiting-taker"
  waitForFlag "waiting queue taker" started
  IO.sleep 10
  match ← Z.unsafeRunSync (queue.offer 42) "queue-offer-waiting-taker" with
  | .success true => pure ()
  | _ => failTest "Queue.offer did not accept a waiting taker"
  match ← fiberExitWithin taker with
  | some (.success 42) => pure ()
  | some exit => failTest s!"waiting Queue.take returned {exit}"
  | none => failTest "Queue.offer did not resume its waiting taker"
  match ← Z.unsafeRunSync queue.poll "queue-poll-after-handoff" with
  | .success none => pure ()
  | _ => failTest "Queue handoff left an unexpected queued value"

/-- Adapted from ZIO `QueueSpec`: blocked offers resume in FIFO order. -/
def testBoundedQueuePreservesWaitingOfferOrder : IO Unit := do
  let queue ← makeBoundedQueue 1
  let _ ← Z.unsafeRunSync (queue.offer 0) "waiting-offers-initial"
  let firstStarted ← IO.mkRef false
  let first ← Z.unsafeFork
    ((Z.fromIO (firstStarted.set true)) *> queue.offer 1)
    "queue-first-waiting-offer"
  waitForFlag "first waiting queue offer" firstStarted
  IO.sleep 10
  let secondStarted ← IO.mkRef false
  let second ← Z.unsafeFork
    ((Z.fromIO (secondStarted.set true)) *> queue.offer 2)
    "queue-second-waiting-offer"
  waitForFlag "second waiting queue offer" secondStarted
  IO.sleep 10

  match ← Z.unsafeRunSync queue.take "waiting-offers-take-initial" with
  | .success 0 => pure ()
  | _ => failTest "Queue.take did not return the initial bounded value"
  match ← fiberExitWithin first with
  | some (.success true) => pure ()
  | some exit => failTest s!"first waiting Queue.offer returned {exit}"
  | none => failTest "the first waiting Queue.offer did not resume"
  match ← Z.unsafeRunSync queue.take "waiting-offers-take-first" with
  | .success 1 => pure ()
  | _ => failTest "Queue did not preserve the first blocked offer"
  match ← fiberExitWithin second with
  | some (.success true) => pure ()
  | some exit => failTest s!"second waiting Queue.offer returned {exit}"
  | none => failTest "the second waiting Queue.offer did not resume"
  match ← Z.unsafeRunSync queue.take "waiting-offers-take-second" with
  | .success 2 => pure ()
  | _ => failTest "Queue did not preserve the second blocked offer"

/-- Adapted from ZIO `QueueSpec`: pending takers receive offers in FIFO order. -/
def testQueuePreservesWaitingTakerOrder : IO Unit := do
  let queue ← makeQueue
  let firstStarted ← IO.mkRef false
  let first ← Z.unsafeFork
    ((Z.fromIO (firstStarted.set true)) *> queue.take)
    "queue-first-waiting-taker"
  waitForFlag "first waiting queue taker" firstStarted
  IO.sleep 10
  let secondStarted ← IO.mkRef false
  let second ← Z.unsafeFork
    ((Z.fromIO (secondStarted.set true)) *> queue.take)
    "queue-second-waiting-taker"
  waitForFlag "second waiting queue taker" secondStarted
  IO.sleep 10
  let _ ← Z.unsafeRunSync (queue.offer 10) "waiting-takers-offer-first"
  let _ ← Z.unsafeRunSync (queue.offer 20) "waiting-takers-offer-second"
  match ← fiberExitWithin first with
  | some (.success 10) => pure ()
  | some exit => failTest s!"first waiting Queue.take returned {exit}"
  | none => failTest "the first waiting Queue.take did not resume"
  match ← fiberExitWithin second with
  | some (.success 20) => pure ()
  | some exit => failTest s!"second waiting Queue.take returned {exit}"
  | none => failTest "the second waiting Queue.take did not resume"

def testQueueRemovesInterruptedTaker : IO Unit := do
  let queue ← makeQueue
  let firstStarted ← IO.mkRef false
  let firstResumed ← IO.mkRef false
  let first ← Z.unsafeFork (do
    Z.fromIO (firstStarted.set true)
    let _ ← queue.take
    Z.fromIO (firstResumed.set true)) "queue-first-taker"
  waitForFlag "first queue taker" firstStarted
  IO.sleep 10
  first.requestInterrupt
  match ← fiberExitWithin first with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"interrupted queue taker returned {exit}"
  | none => failTest "interrupted queue taker did not finish"

  let secondStarted ← IO.mkRef false
  let second ← Z.unsafeFork
    ((Z.fromIO (secondStarted.set true)) *> queue.take) "queue-second-taker"
  waitForFlag "second queue taker" secondStarted
  IO.sleep 10
  match ← Z.unsafeRunSync (queue.offer 42) "queue-offer-after-interrupt" with
  | .success true => pure ()
  | _ => failTest "Queue.offer failed after an interrupted taker"
  match ← fiberExitWithin second with
  | some (.success 42) => pure ()
  | some exit => failTest s!"second queue taker returned {exit}"
  | none => failTest "second queue taker did not receive the offered value"
  assertTrue "interrupted queue taker resumed after cancellation"
    (!(← firstResumed.get))

def testQueueShutdownInterruptsTakers : IO Unit := do
  let queue ← makeQueue
  let started ← IO.mkRef false
  let taker ← Z.unsafeFork
    ((Z.fromIO (started.set true)) *> queue.take) "queue-shutdown-taker"
  waitForFlag "queue shutdown taker" started
  IO.sleep 10
  let _ ← Z.unsafeRunSync queue.shutdown "queue-shutdown"
  match ← fiberExitWithin taker with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"shutdown queue taker returned {exit}"
  | none => failTest "queue shutdown did not wake its taker"
  match ← Z.unsafeRunSync (queue.offer 7) "queue-offer-shutdown" with
  | .success false => pure ()
  | _ => failTest "Queue.offer accepted a value after shutdown"
  match ← Z.unsafeRunSync queue.isShutdown "queue-is-shutdown" with
  | .success true => pure ()
  | _ => failTest "Queue.isShutdown did not report shutdown"

def testBoundedQueueBackpressuresOffers : IO Unit := do
  let queue ← makeBoundedQueue 1
  match ← Z.unsafeRunSync (queue.offer 1) "bounded-queue-offer-first" with
  | .success true => pure ()
  | _ => failTest "bounded Queue.offer rejected its first value"
  let started ← IO.mkRef false
  let blocked ← Z.unsafeFork
    ((Z.fromIO (started.set true)) *> queue.offer 2) "bounded-queue-offer"
  waitForFlag "bounded queue offer" started
  IO.sleep 10
  match ← blocked.state.get with
  | .done exit => failTest s!"bounded Queue.offer did not block: {exit}"
  | _ => pure ()
  match ← Z.unsafeRunSync queue.take "bounded-queue-take-first" with
  | .success 1 => pure ()
  | _ => failTest "bounded Queue.take did not return the first value"
  match ← fiberExitWithin blocked with
  | some (.success true) => pure ()
  | some exit => failTest s!"blocked Queue.offer returned {exit}"
  | none => failTest "blocked Queue.offer did not resume after a take"
  match ← Z.unsafeRunSync queue.take "bounded-queue-take-second" with
  | .success 2 => pure ()
  | _ => failTest "bounded Queue.take did not return the accepted offer"

def testZeroCapacityQueueRendezvous : IO Unit := do
  let queue ← makeBoundedQueue 0
  let started ← IO.mkRef false
  let offerer ← Z.unsafeFork
    ((Z.fromIO (started.set true)) *> queue.offer 42) "rendezvous-offer"
  waitForFlag "rendezvous offer" started
  IO.sleep 10
  match ← offerer.state.get with
  | .done exit => failTest s!"rendezvous Queue.offer did not block: {exit}"
  | _ => pure ()
  match ← Z.unsafeRunSync queue.take "rendezvous-take" with
  | .success 42 => pure ()
  | _ => failTest "rendezvous Queue.take did not receive the offered value"
  match ← fiberExitWithin offerer with
  | some (.success true) => pure ()
  | some exit => failTest s!"rendezvous Queue.offer returned {exit}"
  | none => failTest "rendezvous Queue.offer did not resume"

def testBoundedQueueRemovesInterruptedOffer : IO Unit := do
  let queue ← makeBoundedQueue 1
  let _ ← Z.unsafeRunSync (queue.offer 1) "bounded-interrupt-offer-first"
  let firstStarted ← IO.mkRef false
  let first ← Z.unsafeFork
    ((Z.fromIO (firstStarted.set true)) *> queue.offer 2)
    "bounded-interrupt-first-offer"
  waitForFlag "first bounded queue offer" firstStarted
  IO.sleep 10
  first.requestInterrupt
  match ← fiberExitWithin first with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"interrupted Queue.offer returned {exit}"
  | none => failTest "interrupted Queue.offer did not finish"

  let secondStarted ← IO.mkRef false
  let second ← Z.unsafeFork
    ((Z.fromIO (secondStarted.set true)) *> queue.offer 3)
    "bounded-interrupt-second-offer"
  waitForFlag "second bounded queue offer" secondStarted
  IO.sleep 10
  match ← Z.unsafeRunSync queue.take "bounded-interrupt-take-first" with
  | .success 1 => pure ()
  | _ => failTest "bounded Queue.take did not free a producer slot"
  match ← fiberExitWithin second with
  | some (.success true) => pure ()
  | some exit => failTest s!"replacement Queue.offer returned {exit}"
  | none => failTest "replacement Queue.offer did not resume"
  match ← Z.unsafeRunSync queue.take "bounded-interrupt-take-second" with
  | .success 3 => pure ()
  | _ => failTest "interrupted Queue.offer consumed a later producer slot"

def testBoundedQueueShutdownRejectsBlockedOffer : IO Unit := do
  let queue ← makeBoundedQueue 1
  let _ ← Z.unsafeRunSync (queue.offer 1) "bounded-shutdown-offer-first"
  let started ← IO.mkRef false
  let blocked ← Z.unsafeFork
    ((Z.fromIO (started.set true)) *> queue.offer 2)
    "bounded-shutdown-offer"
  waitForFlag "bounded queue shutdown offer" started
  IO.sleep 10
  let _ ← Z.unsafeRunSync queue.shutdown "bounded-queue-shutdown"
  match ← fiberExitWithin blocked with
  | some (.success false) => pure ()
  | some exit => failTest s!"shutdown Queue.offer returned {exit}"
  | none => failTest "shutdown did not wake a blocked Queue.offer"

def queueTests : List (String × IO Unit) := [
  ("testQueuePreservesFIFOOrder", testQueuePreservesFIFOOrder),
  ("testQueueTransfersToWaitingTaker", testQueueTransfersToWaitingTaker),
  ("testBoundedQueuePreservesWaitingOfferOrder",
    testBoundedQueuePreservesWaitingOfferOrder),
  ("testQueuePreservesWaitingTakerOrder", testQueuePreservesWaitingTakerOrder),
  ("testQueueRemovesInterruptedTaker", testQueueRemovesInterruptedTaker),
  ("testQueueShutdownInterruptsTakers", testQueueShutdownInterruptsTakers),
  ("testBoundedQueueBackpressuresOffers", testBoundedQueueBackpressuresOffers),
  ("testZeroCapacityQueueRendezvous", testZeroCapacityQueueRendezvous),
  ("testBoundedQueueRemovesInterruptedOffer", testBoundedQueueRemovesInterruptedOffer),
  ("testBoundedQueueShutdownRejectsBlockedOffer",
    testBoundedQueueShutdownRejectsBlockedOffer)
]
