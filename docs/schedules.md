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

This is the first schedule subset. Schedule intersection, union, sequencing,
input filters, exponential backoff, and a manual driver are future additions.
