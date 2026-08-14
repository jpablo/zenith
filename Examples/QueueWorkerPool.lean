import Z

/-!
A small bounded-concurrency worker pool built with `Z.Queue` and fibers.

The queue carries `Option Nat`: `some job` is work and `none` is one graceful
stop signal. `Queue.shutdown` is not used for normal completion because it
interrupts pending takers.
-/

namespace QueueWorkerPool

private partial def worker
    (queue : Z.Queue (Option Nat))
    (results : IO.Ref (List Nat)) : Z Unit Empty Unit := do
  match ← queue.take with
  | none => pure ()
  | some job =>
      Z.sleep 5
      Z.succeed <| results.modify ((job * job) :: ·)
      worker queue results

/-- Run at most `workerCount` jobs at the same time and return sorted results. -/
def run (jobs : List Nat) (workerCount : Nat) : Z Unit Empty (List Nat) := do
  if workerCount == 0 then
    Z.die (R := Unit) <|
      IO.userError "QueueWorkerPool requires at least one worker"
  let queue ← Z.Queue.unbounded
  let results ← Z.succeed <| IO.mkRef ([] : List Nat)
  let workers ← (List.range workerCount).mapM fun index =>
    (worker queue results).fork s!"queue-worker-{index}"
  for job in jobs do
    let accepted ← queue.offer (some job)
    unless accepted do
      Z.die (R := Unit) <|
        IO.userError "QueueWorkerPool queue shut down while producing"
  for _ in List.range workerCount do
    let accepted ← queue.offer none
    unless accepted do
      Z.die (R := Unit) <|
        IO.userError "QueueWorkerPool queue shut down while stopping workers"
  for worker in workers do
    let _ ← worker.join
    pure ()
  let output ← Z.succeed results.get
  pure <| output.mergeSort (· < ·)

def demo : Z Unit Empty (List Nat) :=
  run [1, 2, 3, 4, 5, 6] 3

def main : IO Unit := do
  match ← Z.unsafeRunSync demo "queue-worker-pool" with
  | .success results => IO.println s!"squared jobs: {results}"
  | .failure cause =>
      throw <| IO.userError s!"Queue worker-pool demo failed: {cause}"

end QueueWorkerPool
