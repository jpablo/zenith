# Streams

`Z.Stream R E A` is a repeatable, effectful source of values. Each call to a
runner starts a new stream run.

```lean
def numbers : Z.Stream Unit Empty Nat :=
  Z.Stream.fromList [1, 2, 3, 4]

def doubled : Z Unit Empty (List Nat) :=
  Z.Stream.runCollect <|
    Z.Stream.map numbers fun value =>
      Z.succeed (value * 2)
```

## Basic operations

* `unfold initial step` creates a stream from an effectful state transition.
* `fromList values` creates a finite stream.
* `map` transforms each value with an effect.
* `filter` removes values with a Boolean predicate.
* `runForeach` consumes values with an effect.
* `runCollect` returns all values in source order.

## Buffering and lifetime

`stream.buffer capacity` runs the upstream stream in a background fiber and
uses `Z.Queue.bounded capacity` between the producer and consumer. A full
buffer pauses the producer.

The runner owns this producer fiber. If its consumer succeeds, fails, or is
interrupted, Zenith interrupts the producer and waits for it to finish. A
source failure shuts down the buffer and reaches the consumer through the same
typed error channel.

## Parallel mapping

`stream.mapPar workers transform` runs up to `workers` transformations at a
time. It preserves source order. The current implementation uses ordered
batches: it starts one batch, emits that batch in source order, then starts the
next batch. A zero worker count uses one worker.

## Use in the TODO report

`Examples/TodoReport.lean` creates a stream of source paths with `unfold`,
buffers discovery with capacity 32, maps file scanning in parallel, and then
collects and sorts the results. Directory traversal therefore pauses when file
reads are busy instead of retaining all source paths first.

## Current boundary

This is a small foundation. It does not yet have flat-map, merge, broadcast,
time-based operators, or continuous ordered parallel output.
