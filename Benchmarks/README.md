# Interpreter benchmarks

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
| `run/succeedNow` | 8,254 ns/op |
| `run/flatMap` | 866 ns/op |
| `run/sync` | 845 ns/op |
| `run/error-recovery` | 1,929 ns/op |
| `run/immediate-async` | 4,276 ns/op |
| `run/fork-join` | 14,250 ns/op |

The `io-bind` case is a compiler-optimized lower bound. The `io-task` case is
the useful reference for `run/succeedNow`, because both cases create and wait
for a Lean task.
