# Interpreter benchmarks

This folder contains repeatable performance measurements for the Zenith
interpreter. `Interpreter.lean` defines the benchmark executable. The rest of
this file records how to run it and the measured baselines.

Build the benchmark once, and then run the executable directly:

```sh
lake build interpreterBench
./.lake/build/bin/interpreterBench
```

Use the quick profile while you change code:

```sh
lake exe interpreterBench --quick
```

The default profile runs two warm-up rounds and seven measured rounds. It
reports the minimum, median, and maximum time. `ns/op` uses the number of
logical operations in the named test. Use the median when you compare runs.

Run the benchmark on the same machine with as little other work as possible.
Do not compare results from different machines as if they were equivalent.

## Initial baseline

The initial baseline was measured on 2026-08-11 at commit `c917a3f`.

- Machine: Apple M4 Pro, Mac16,8, 14 logical CPUs
- System: Darwin arm64, kernel 25.5.0
- Lean: 4.32.2, release build
- Warm-ups: 2
- Samples: 7

| Case | Median time per logical operation |
| --- | ---: |
| `baseline/io-bind` | 1 ns/op |
| `baseline/io-task` | 5,648 ns/op |
| `baseline/io-ref` | 7 ns/op |
| `run/succeed` | 8,254 ns/op |
| `run/flatMap` | 866 ns/op |
| `run/sync` | 845 ns/op |
| `run/error-recovery` | 1,929 ns/op |
| `run/immediate-async` | 4,276 ns/op |
| `run/fork-join` | 14,250 ns/op |

The `io-bind` case is a compiler-optimized lower bound. The `io-task` case is
the useful reference for `run/succeed`, because both cases create and wait
for a Lean task.

## Disabled-instrumentation fast path

Measured on 2026-08-11 after the interpreter stopped preparing diagram and
log data when both features were disabled. The table uses the second of two
full benchmark runs. The machine and benchmark configuration are the same as
the initial baseline.

| Case | Initial | Optimized | Change |
| --- | ---: | ---: | ---: |
| `baseline/io-bind` | 1 ns/op | 1 ns/op | reference |
| `baseline/io-task` | 5,648 ns/op | 5,582 ns/op | reference |
| `baseline/io-ref` | 7 ns/op | 8 ns/op | reference |
| `run/succeed` | 8,254 ns/op | 6,936 ns/op | 1.19× faster |
| `run/flatMap` | 866 ns/op | 211 ns/op | 4.10× faster |
| `run/sync` | 845 ns/op | 246 ns/op | 3.43× faster |
| `run/error-recovery` | 1,929 ns/op | 452 ns/op | 4.27× faster |
| `run/immediate-async` | 4,276 ns/op | 3,587 ns/op | 1.19× faster |
| `run/fork-join` | 14,250 ns/op | 13,671 ns/op | 1.04× faster |

The first run gave 207 ns/op for `flatMap`, 251 ns/op for `sync`, and
451 ns/op for error recovery. This shows that the large changes are stable
across both runs.

## Current optimized result

The next changes run `unsafeRunSync` on its caller's task with only the state
that a synchronous runner needs, continue inline when an async callback
completes during registration, avoid unused fiber log text, and read the
logging setting once per fiber. Interruption checks also skip the
interruptibility reference when no interruption was requested.

The table uses the second of two full runs on the same machine and with the
same configuration as the initial baseline.

| Case | Initial | Current | Improvement |
| --- | ---: | ---: | ---: |
| `run/succeed` | 8,254 ns/op | 317 ns/op | 26.04× |
| `run/flatMap` | 866 ns/op | 178 ns/op | 4.87× |
| `run/sync` | 845 ns/op | 199 ns/op | 4.25× |
| `run/error-recovery` | 1,929 ns/op | 393 ns/op | 4.91× |
| `run/immediate-async` | 4,276 ns/op | 299 ns/op | 14.30× |
| `run/fork-join` | 14,250 ns/op | 12,619 ns/op | 1.13× |

The first current-result run measured 325, 180, 200, 401, 305, and 12,641
ns/op in the same row order. The two runs give the same overall result.

A later style refactor extracted inline diagram helpers and a typed async
resume gate. Its verification run measured 316, 171, 186, 388, 301, and
12,293 ns/op in the same row order. The refactor caused no performance loss.

## Sequential dispatcher extraction

Measured on 2026-08-15 on the same machine and with the default benchmark
profile. The pre-dispatcher run was taken immediately before extracting
`Z.Runtime.Sequential`. The post-dispatcher run uses the executable routing
functions from `runLoop`, `continueOrComplete`, and `runWithErrorHandler`.

| Case | Pre-dispatcher | Post-dispatcher | Change |
| --- | ---: | ---: | ---: |
| `baseline/io-bind` | 1 ns/op | <1 ns/op | reference |
| `baseline/io-task` | 5,623 ns/op | 5,575 ns/op | reference |
| `baseline/io-ref` | 7 ns/op | 7 ns/op | reference |
| `run/succeed` | 358 ns/op | 316 ns/op | 1.13× faster |
| `run/flatMap` | 253 ns/op | 152 ns/op | 1.66× faster |
| `run/sync` | 266 ns/op | 173 ns/op | 1.54× faster |
| `run/contramap` | 106 ns/op | 64 ns/op | 1.66× faster |
| `run/provide-environment` | 106 ns/op | 64 ns/op | 1.66× faster |
| `run/error-recovery` | 519 ns/op | 362 ns/op | 1.43× faster |
| `run/immediate-async` | 375 ns/op | 303 ns/op | 1.24× faster |
| `run/immediate-async-interrupt` | 508 ns/op | 429 ns/op | 1.18× faster |
| `run/uninterruptible` | 675 ns/op | 429 ns/op | 1.57× faster |
| `run/fork-join` | 12,272 ns/op | 12,350 ns/op | 1% slower |

The task baseline is unchanged. The small fork/join difference is within one
benchmark run's normal machine variation. The sequential paths show no
regression, and the same-task asynchronous-resume path remains below the
task baseline.
