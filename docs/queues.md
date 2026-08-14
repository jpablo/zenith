# Queues

`Z.Queue A` is an unbounded, FIFO, multi-producer, multi-consumer queue for
Zenith fibers.

```lean
let queue ← Z.Queue.unbounded
let accepted ← queue.offer value
let next ← queue.take
```

`offer` returns `true` while the queue is open. It returns `false` after
shutdown. `poll` returns `none` when no value is available. `take` waits when
the queue is empty.

## Interruption

`take` is interruption-aware. If a waiting fiber is interrupted, Zenith
removes its waiter from the queue. A later `offer` can then go to another
waiting fiber or into the queue. This prevents an interrupted consumer from
silently taking a later value.

## Shutdown

`queue.shutdown` is final. It discards queued values, interrupts all waiting
takers, and makes future `offer` calls return `false`.

Use a value in the queue for normal worker completion. For example, the worker
pool in `Examples/QueueWorkerPool.lean` uses `Option Job`: `some job` is work,
and one `none` value stops one worker. Reserve `shutdown` for cancellation or
other abnormal termination.

`Examples/TodoReport.lean` uses this pattern for file scanning. It discovers
paths first, sends them to four scoped workers, and sorts findings before it
writes the report. A typed file error ends the scoped scan and interrupts any
remaining workers.

## Current boundary

The queue is intentionally unbounded. It does not apply backpressure to
producers. A bounded queue needs cancellation-aware waiting producers as well
as cancellation-aware waiting consumers. That is a separate feature.
