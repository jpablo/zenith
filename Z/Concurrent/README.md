# Concurrent core features

This folder contains core concurrent abstractions that run on Zenith fibers.

* `Deferred.lean` defines a one-shot result cell with interruptible awaiters.
* `Parallel.lean` defines parallel composition, races, and timeouts.
* `Queue.lean` defines interruption-aware bounded and unbounded FIFO queues.
* `Stream.lean` defines repeatable effectful streams, buffering, and bounded
  parallel mapping.

These modules are part of `import Z`.
