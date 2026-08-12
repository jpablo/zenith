# Parallel effect composition

`Z.zipPar` runs two effects concurrently and returns both values:

```lean
def loadBoth :=
  loadConfig.zipPar loadUsers
```

`Z.zipWithPar` applies a function when both effects succeed:

```lean
def loadSummary :=
  loadConfig.zipWithPar loadUsers summarize
```

Both operations infer the combined environment and error types. They use the
same `Environment.Meet` and `ErrorChannel.Join` rules as other heterogeneous
Zenith combinators.

Parallel execution has these rules:

- Both effects start in child fibers.
- If both effects succeed, the result keeps left-to-right value order.
- If one effect fails, Zenith interrupts the other effect and waits for it.
- An interruption caused only by sibling cancellation is not added to the
  returned cause.
- If both effects fail, Zenith returns `Cause.parallel left right`.
- If cancelled cleanup also fails, its cause stays in the parallel cause.
- External interruption cancels and waits for both child fibers.

The child fibers use the complete inferred environment. This also works when
an environment service is in a universe above `Type`.
