import Z
import Tests.Support

/-!
Regression tests for the unbounded, interruption-aware Zenith queue.
-/

private def makeQueue : IO (Z.Queue Nat) := do
  match ← Z.unsafeRunSync Z.Queue.unbounded "queue-make" with
  | .success queue => pure queue
  | .failure cause => failTest s!"Queue.unbounded failed: {cause}"

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

def testQueueRemovesInterruptedTaker : IO Unit := do
  let queue ← makeQueue
  let firstStarted ← IO.mkRef false
  let firstResumed ← IO.mkRef false
  let first ← Z.unsafeFork (do
    Z.succeed (firstStarted.set true)
    let _ ← queue.take
    Z.succeed (firstResumed.set true)) "queue-first-taker"
  waitForFlag "first queue taker" firstStarted
  IO.sleep 10
  first.requestInterrupt
  match ← fiberExitWithin first with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"interrupted queue taker returned {exit}"
  | none => failTest "interrupted queue taker did not finish"

  let secondStarted ← IO.mkRef false
  let second ← Z.unsafeFork
    ((Z.succeed (secondStarted.set true)) *> queue.take) "queue-second-taker"
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
    ((Z.succeed (started.set true)) *> queue.take) "queue-shutdown-taker"
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

def queueTests : List (String × IO Unit) := [
  ("testQueuePreservesFIFOOrder", testQueuePreservesFIFOOrder),
  ("testQueueRemovesInterruptedTaker", testQueueRemovesInterruptedTaker),
  ("testQueueShutdownInterruptsTakers", testQueueShutdownInterruptsTakers)
]
