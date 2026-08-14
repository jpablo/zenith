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

/-- Adapted from ZIO `ZStreamSpec`: a stream may run more than once. -/
def testStreamIsRepeatable : IO Unit := do
  let stream := Z.Stream.fromList [1, 2, 3]
  match ← Z.unsafeRunSync stream.runCollect "stream-repeat-first" with
  | .success [1, 2, 3] => pure ()
  | exit => failTest s!"the first stream run returned {exit}"
  match ← Z.unsafeRunSync stream.runCollect "stream-repeat-second" with
  | .success [1, 2, 3] => pure ()
  | exit => failTest s!"the second stream run returned {exit}"

/-- Adapted from ZIO `StreamLazinessSpec`: steps run only during consumption. -/
def testStreamDefersSourceSteps : IO Unit := do
  let steps ← IO.mkRef 0
  let source : Z.Stream Unit Empty Nat :=
    Z.Stream.unfold 0 fun state =>
      Z.fromIO (steps.modify (· + 1)) *>
        if state < 2 then pure (some (state, state + 1)) else pure none
  let stream := source.map fun value => pure (value + 10)
  assertTrue "constructing a stream ran a source step" ((← steps.get) == 0)
  match ← Z.unsafeRunSync stream.runCollect "stream-lazy-first" with
  | .success [10, 11] => pure ()
  | exit => failTest s!"the lazy stream run returned {exit}"
  assertTrue "one stream run evaluated the wrong number of source steps"
    ((← steps.get) == 3)
  match ← Z.unsafeRunSync stream.runCollect "stream-lazy-second" with
  | .success [10, 11] => pure ()
  | exit => failTest s!"the second lazy stream run returned {exit}"
  assertTrue "the second stream run did not re-evaluate its source"
    ((← steps.get) == 6)

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

/-- Adapted from ZIO `ZStreamSpec`: `mapPar` keeps source output order. -/
def testStreamMapParPreservesSourceOrder : IO Unit := do
  let transform (value : Nat) : Z Unit Empty Nat :=
    let delay : UInt32 :=
      match value with
      | 1 => 40
      | 2 => 20
      | _ => 5
    Z.sleep delay *> pure (value * 10)
  let program : Z Unit Empty (List Nat) :=
    (Z.Stream.fromList [1, 2, 3]).mapPar 3 transform |>.runCollect
  match ← Z.unsafeRunSync program "stream-map-par-order" with
  | .success [10, 20, 30] => pure ()
  | exit => failTest s!"parallel stream mapping changed source order: {exit}"

/-- Adapted from ZIO `ZStreamSpec`: buffered streams keep a source failure. -/
def testBufferedStreamPropagatesSourceFailure : IO Unit := do
  let source : Z.Stream Unit String Nat :=
    Z.Stream.unfold 0 fun state =>
      if state == 0 then
        pure (some (1, 1))
      else
        Z.fail "source failure"
  let program : Z Unit String (List Nat) := (source.buffer 1).runCollect
  match ← Z.unsafeRunSync program "stream-buffer-source-failure" with
  | .failure (.fail "source failure") => pure ()
  | exit => failTest s!"buffered stream changed its source failure: {exit}"

def streamTests : List (String × IO Unit) := [
  ("testStreamTransformsAndCollects", testStreamTransformsAndCollects),
  ("testStreamIsRepeatable", testStreamIsRepeatable),
  ("testStreamDefersSourceSteps", testStreamDefersSourceSteps),
  ("testStreamBufferPreservesValues", testStreamBufferPreservesValues),
  ("testStoppingBufferedStreamStopsProducer", testStoppingBufferedStreamStopsProducer),
  ("testStreamMapParBoundsParallelism", testStreamMapParBoundsParallelism),
  ("testStreamMapParPreservesSourceOrder", testStreamMapParPreservesSourceOrder),
  ("testBufferedStreamPropagatesSourceFailure",
    testBufferedStreamPropagatesSourceFailure)
]
