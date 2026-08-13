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

## Filters

Schedule filters can stop a policy from its current input or output:

```lean
def temporaryErrors : Schedule Unit RequestError Nat :=
  (Schedule.forever).whileInput RequestError.isTemporary

def firstFive : Schedule Unit Input Nat :=
  (Schedule.forever).whileOutput fun count => count < 5
```

The available forms are `whileInput`, `untilInput`, `whileOutput`, and
`untilOutput`. Each predicate is checked only when the underlying schedule
wants to continue. A false predicate changes the decision to stop but keeps
the current schedule output.

The `ZIO` forms use an effectful predicate:

```lean
def configuredRetries : Schedule RetryConfig RequestError Nat :=
  (Schedule.forever).whileOutputZIO fun retries =>
    Z.serviceWith fun config : RetryConfig =>
      retries < config.maximumRetries
```

The available forms are `checkZIO`, `whileInputZIO`, `untilInputZIO`,
`whileOutputZIO`, and `untilOutputZIO`. A predicate has type
`Z R Empty Bool`, so it can use services and run effects but cannot produce a
typed failure. Zenith combines the schedule and predicate environments with
`Environment.Meet`. It does not run the predicate after the underlying
schedule has stopped.

## Retry fallback

`Z.retryOrElse` runs a fallback after the schedule stops. The fallback receives
the last typed error and the final schedule output:

```lean
def resilientLoad : Z (FileSystem × Logger) LogError Bytes :=
  loadFile.retryOrElse (Schedule.recurs 3) fun error retries =>
    logAndLoadDefault error retries
```

The original typed error is handled, so the result has the fallback error
type. Effect, schedule, and fallback requirements are combined with
`Environment.Meet`. Defects and interruptions are not sent to the fallback.
If a cause contains both a typed failure and a defect, Zenith removes the
handled failure and preserves the defect.

Both successful paths of `retryOrElse` have the same type.
`retryOrElseEither` supports a different fallback success type. It returns
`Sum FallbackValue EffectValue`, with fallback success on the left.

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

Output folds, jitter, Fibonacci backoff, and a manual driver are future
additions.
