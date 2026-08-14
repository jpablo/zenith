# Deferred results

`Deferred E A` is a one-shot result cell. Many fibers can await it, but only
the first completion wins. It keeps the complete `Exit E A`, so a value, typed
failure, defect, or interruption is delivered unchanged to every awaiter.

```lean
def example : Z Unit String Nat := zdo
  let deferred ← Deferred.make
  let _ ← (deferred.succeed 42).fork "producer"
  deferred.await
```

The completion operations return `Bool`: `true` means that this call completed
the cell, and `false` means it was already complete.

```lean
deferred.succeed value
deferred.fail error
deferred.failCause cause
deferred.die defect
deferred.interrupt
deferred.done exit
```

`Deferred.complete effect` runs an effect and stores its whole exit. It has no
typed failure of its own because the source failure becomes the cell result.

An awaiting Zenith fiber is interruptible. When interruption wins while the
cell is unresolved, Zenith removes its callback from the pending waiter list.
A later completion cannot resume that fiber.
