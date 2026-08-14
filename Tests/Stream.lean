import Z
import Tests.Support

/-!
Regression tests for the minimal Zenith stream API.
-/

def testStreamTransformsAndCollects : IO Unit := do
  let program : Z Unit Empty (List Nat) :=
    Z.Stream.runCollect <|
      Z.Stream.filter
        (Z.Stream.map (Z.Stream.fromList [1, 2, 3, 4]) fun value =>
          Z.succeed (value * 2))
        fun value => decide (value > 4)
  match ← Z.unsafeRunSync program "stream-transforms" with
  | .success [6, 8] => pure ()
  | exit => failTest s!"stream transformations returned {exit}"

def testStreamBufferPreservesValues : IO Unit := do
  let program : Z Unit Empty (List Nat) :=
    (Z.Stream.fromList [1, 2, 3, 4]).buffer 1 |>.runCollect
  match ← Z.unsafeRunSync program "stream-buffer" with
  | .success [1, 2, 3, 4] => pure ()
  | exit => failTest s!"buffered stream returned {exit}"

def testStoppingBufferedStreamStopsProducer : IO Unit := do
  let started ← IO.mkRef false
  let finalized ← IO.mkRef false
  let source : Z.Stream Unit Empty Nat :=
    Z.Stream.unfold () fun _ =>
      ((Z.fromIO (started.set true)) *> Z.sleep 2000 *> pure (some (1, ())))
        |>.ensuring (Z.fromIO (finalized.set true))
  let fiber ← Z.unsafeFork
    ((source.buffer 1).runForeach fun _ => pure ()) "stream-buffer-cancel"
  waitForFlag "buffered stream producer" started
  fiber.requestInterrupt
  match ← fiberExitWithin fiber with
  | some (.failure .interrupt) => pure ()
  | some exit => failTest s!"interrupted buffered stream returned {exit}"
  | none => failTest "interrupted buffered stream did not finish"
  assertTrue "buffered stream producer finalizer did not run"
    (← finalized.get)

def testStreamMapParBoundsParallelism : IO Unit := do
  let active ← IO.mkRef 0
  let maximum ← IO.mkRef 0
  let transform (value : Nat) : Z Unit Empty Nat :=
    ((Z.fromIO do
      let current ← active.modifyGet fun previous =>
        let next := previous + 1
        (next, next)
      maximum.modify fun previous => max previous current) *>
      Z.sleep 30 *>
      pure (value * value))
      |>.ensuring (Z.fromIO <| active.modify fun current => current - 1)
  let program : Z Unit Empty (List Nat) :=
    (Z.Stream.fromList [1, 2, 3, 4]).mapPar 2 transform |>.runCollect
  match ← Z.unsafeRunSync program "stream-map-par" with
  | .success [1, 4, 9, 16] => pure ()
  | exit => failTest s!"parallel stream mapping returned {exit}"
  assertTrue "parallel stream mapping did not run two transformations together"
    ((← maximum.get) == 2)

def streamTests : List (String × IO Unit) := [
  ("testStreamTransformsAndCollects", testStreamTransformsAndCollects),
  ("testStreamBufferPreservesValues", testStreamBufferPreservesValues),
  ("testStoppingBufferedStreamStopsProducer", testStoppingBufferedStreamStopsProducer),
  ("testStreamMapParBoundsParallelism", testStreamMapParBoundsParallelism)
]
