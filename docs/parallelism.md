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

`Z.race` returns the first successful value:

```lean
def fastestResponse :=
  primaryRequest.race backupRequest
```

The first completion does not always win. If it is a failure, `race` waits for
the other effect. It fails only if both effects fail. In that case, it returns
`Cause.parallel left right`.

Both effects passed to `race` return the same success type. Use `Z.raceEither`
when their success types differ:

```lean
def firstResult :=
  loadConfig.raceEither loadUsers
-- Z R E (Sum Config (List User))
```

The `Sum` tag identifies the successful branch.

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

Racing has these additional rules:

- A successful winner interrupts the loser.
- `race` waits for the loser and its finalizers before it returns.
- A failure does not win while the other effect can still succeed.
- Two failures are combined as `Cause.parallel left right`.

The child fibers use the complete inferred environment. This also works when
an environment service is in a universe above `Type`.
