# Queues

`Z.Queue A` is a FIFO, multi-producer, multi-consumer queue for Zenith fibers.

```lean
let queue ← Z.Queue.bounded 32
let accepted ← queue.offer value
let next ← queue.take
```

Use `Z.Queue.unbounded` when producers must never wait. Use
`Z.Queue.bounded capacity` when the queue must limit queued values. `offer`
returns `true` while the queue is open. For a bounded queue, it waits when the
queue is full. It returns `false` after shutdown. `poll` returns `none` when no
value is available. `take` waits when the queue is empty.

`Z.Queue.bounded 0` is a rendezvous queue: each offer waits for a taker.

## Interruption

`take` is interruption-aware. If a waiting fiber is interrupted, Zenith
removes its waiter from the queue. A later `offer` can then go to another
waiting fiber or into the queue. This prevents an interrupted consumer from
silently taking a later value.

A blocked bounded `offer` is also interruption-aware. An interrupted producer
is removed before a later `take` frees capacity. This prevents its value from
being accepted after cancellation.

## Shutdown

`queue.shutdown` is final. It discards queued values, interrupts all waiting
takers, and makes pending and future `offer` calls return `false`.

Use a value in the queue for normal worker completion. For example, the worker
pool in `Examples/QueueWorkerPool.lean` uses `Option Job`: `some job` is work,
and one `none` value stops one worker. Reserve `shutdown` for cancellation or
other abnormal termination.

`Z.Stream.buffer` uses a bounded queue for the common producer-consumer
pattern. `Examples/TodoReport.lean` uses it while it discovers and scans
source files.

## Current boundary

The queue has no bulk operations, priority mode, or broadcast mode yet. Add
these only when a real program needs them.
