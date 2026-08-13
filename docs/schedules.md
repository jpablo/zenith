# Retry and repeat schedules

`Schedule R Input Output` is a reusable policy for recurring effects. It has
an internal state. Each step consumes an input, emits an output, and decides
to stop or continue after a delay.

`Z.retry` feeds typed errors into a schedule:

```lean
def loadWithRetries : Z FileSystem FileError Bytes :=
  loadFile.retry (Schedule.recurs 3)
```

The effect runs once before the schedule is consulted. `Schedule.recurs 3`
permits three additional attempts. A success returns the effect value. If all
attempts fail, `retry` returns the last typed failure. Defects and interruption
are not retried.

`Z.repeat` feeds successful values into a schedule:

```lean
def finalCount : Z Console ConsoleError Nat :=
  printHeartbeat.repeat (Schedule.recurs 3)
```

This runs the effect four times: one initial run and three repetitions. It
returns the final schedule output, which is `3`. An effect failure stops the
repetition and stays a failure.

The first built-in policies are:

- `Schedule.stop`: do not run again.
- `Schedule.once`: run one additional time.
- `Schedule.recurs count`: run `count` additional times without a delay.
- `Schedule.forever`: continue without a delay.
- `Schedule.spaced milliseconds`: continue with a fixed delay after each run.

`Schedule.map` transforms schedule outputs. `Schedule.as` replaces them with a
constant value.

## Composition

Intersection continues only while both schedules continue. It pairs their
outputs and selects the longer delay:

```lean
def boundedBackoff : Schedule Unit RequestError (UInt32 × Nat) :=
  (Schedule.exponential 100).zip (Schedule.recurs 5)
```

`Schedule.intersect` and `Schedule.zip` are equivalent. The operator form is
`&&&`:

```lean
def boundedBackoff : Schedule Unit RequestError (UInt32 × Nat) :=
  Schedule.exponential (Input := RequestError) 100 &&&
    Schedule.recurs (Input := RequestError) 5
```

Lean reserves `&&` for boolean expressions. Therefore, Zenith uses Lean's
overloadable `&&&` operator.

Union continues while either schedule continues. It pairs their outputs and
selects the shorter active delay. `Schedule.union` and `Schedule.either` are
equivalent. The operator form is `|||`, because Lean reserves `||` for
booleans.

Sequencing runs the first schedule until it stops and then runs the second:

```lean
def phased : Schedule Unit RequestError Nat :=
  Schedule.recurs (Input := RequestError) 2 ++
    Schedule.recurs (Input := RequestError) 3
```

`Schedule.andThen` is the named form. Both schedules have the same output type.
`Schedule.andThenEither` supports different output types and returns `Sum`.

The named composition methods infer ignored input types from a surrounding
`retry` or `repeat` call. The overloadable operators can need an explicit
`Input := ...` argument because Lean resolves the operator before it sees that
later context.

## Exponential backoff

`Schedule.exponential base` starts with `base` milliseconds and doubles each
later delay. The optional natural-number `factor` changes the multiplier:

```lean
Schedule.exponential 100            -- 100, 200, 400, ...
Schedule.exponential 100 (factor := 3) -- 100, 300, 900, ...
```

The output is the current delay as `UInt32`. Growth saturates at the largest
`UInt32` value instead of wrapping to a small delay.

Schedules can require services. `retry` and `repeat` use
`Environment.Meet` to infer the combined requirements of the effect and its
schedule. `Schedule.make` creates a custom effectful step:

```lean
def policy : Schedule RetryConfig FileError Nat :=
  Schedule.make 0 fun _ count =>
    Z.serviceWith fun config =>
      let decision :=
        if count < config.maximumRetries then
          Schedule.Decision.continue config.delayMilliseconds
        else
          Schedule.Decision.done
      (count + 1, count, decision)
```

Delays use `UInt32` milliseconds, which matches `Z.sleep`. A delay is
interruptible. Interruption stops the active delay and the recurrence loop.

Input filters, output folds, jitter, Fibonacci backoff, and a manual driver are
future additions.
