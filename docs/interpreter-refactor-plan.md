# Interpreter Refactor Plan

This plan describes a possible runtime refactor that makes the interpreter
transition logic easier to verify. It is separate from the semantic proof
plan in [core-formalization-plan.md](core-formalization-plan.md). The proof
plan specifies behavior. This plan protects runtime behavior and performance.

## Goal

Extract a pure, total one-step transition from the interpreter where possible.
Keep a small `IO` driver that executes external work and repeats transitions.

The complete runtime loop may still need `partial`. An effect can wait for an
asynchronous callback or run forever, so there is no global decreasing
measure. The goal is to remove `partial` from the transition decision, not to
claim that the complete runtime always terminates.

## Lean runtime facts

`partial` does not add a runtime termination check. Removing the keyword by
itself does not make an interpreter faster.

Lean 4.32 implements `whileM` with an erased `partial` runtime function. Its
recursive call is in tail position. Therefore, a `whileM` driver is a viable
shape for the runtime loop, but it does not make the generated runtime code
total or guarantee a performance improvement.

Proof terms and type parameters are erased. Runtime cost can instead come
from state objects, transition results, tag tests, closures, queues, and task
creation.

## Performance contract

The current figures are same-machine medians from
[Benchmarks/README.md](../Benchmarks/README.md). They are reference values,
not portable targets for other machines.

| Case | Current reference |
|---|---:|
| `run/flatMap` | 178 ns/op |
| `run/sync` | 199 ns/op |
| `run/error-recovery` | 393 ns/op |
| `run/immediate-async` | 299 ns/op |
| `run/fork-join` | 12,619 ns/op |
| `baseline/io-task` | 5,648 ns/op |

The `run/immediate-async` result improved from 3,587 ns/op to 299 ns/op in a
change set that included several optimizations. One of them was the required
inline callback path. Do not attribute the whole improvement to that path
alone.

The current `AsyncResumeGate` stores completion during registration and runs
it in `finishRegistration` on the same task. A refactor must keep this case.
Completion after registration may need a task hop. Completion during
registration must not add one.

## Design constraints

| Choice | Expected effect |
|---|---|
| One `whileM` driver on the current task | Usually near-neutral; measure it. |
| Proof and type evidence | Erased in generated code. |
| Allocate a state and transition value for every instruction | Can regress `flatMap` and `sync`. |
| Reuse mutable state when safe | Can avoid transition allocations. |
| Queue every transition | Likely adds measurable overhead. |
| Create a task for every callback completion | Likely loses the immediate-async fast path. |
| Continue synchronous registration completion inline | Required. |

The exact state and command representation is an implementation choice. It
must be benchmarked. Do not assume that a compiler will remove allocations or
closures without measurement.

## Required behavior

The refactor must preserve these properties:

1. Synchronous execution continues on the caller task.
2. A callback that completes during registration resumes inline.
3. Ordinary sequential transitions do not go through a queue.
4. The number of task creations does not increase on the existing synchronous
   and immediate-async benchmark paths.
5. The existing error recovery, fork/join, and interruption behavior remains
   unchanged.

## Refactor sequence

1. Record a benchmark baseline and add cases for `asyncInterrupt`,
   `contramap`, `provideEnvironment`, and interruption status.
2. Extract a pure one-step transition for the sequential instruction subset.
   Keep the current runtime loop unchanged at this point.
3. Prove the pure transition conforms to
   `SequentialMachine.Step` in
   [`Zenith/Formalization/SequentialMachine.lean`](../Zenith/Formalization/SequentialMachine.lean).
   Use `SequentialRuntimeStack.lean` to preserve the production continuation
   stack shape while this bridge is introduced.
4. Replace the sequential recursive calls with one driver loop only if the
   benchmark comparison shows no material regression.
5. Add asynchronous commands while preserving the current resume-gate
   protocol and task-hop behavior.
6. Add fibers, interruption, diagrams, logging, and defect handling in
   separate changes.

## Formalization status

The semantic preparation for steps 2 and 3 is complete:

* `SequentialCore.lean` defines the pure sequential instruction subset.
* `SequentialMachine.lean` proves its typed stack-machine behavior.
* `SequentialRuntimeStack.lean` relates model frames to production `Stack`
  frames, including their saved environment evidence.
* `SequentialRuntime.lean` defines a production-shaped pure transition
  relation and proves that every finite model execution has a matching
  transition sequence.
* `Z.Runtime.Sequential` is the extracted executable dispatcher. It owns the
  six instruction-routing transitions and the two continuation-routing
  transitions. The interpreter now calls it for `runLoop`,
  `continueOrComplete`, and `runWithErrorHandler`.
* `SequentialDispatcher.lean` proves constructor reduction laws that connect
  the dispatcher to the lowered model and production stack shapes.

This is a specification, refinement result, and executable dispatcher link.
`SequentialDispatcher.run_models_step` proves the general instruction-routing
conformance theorem. `success_models_step` and `failure_models_step` prove
the analogous general result for continuation delivery through lowered stack
frames. The next runtime change, if needed, is a driver-loop refactor. It must
first preserve the current task and same-task async-resume behavior, then pass
the benchmark comparison. Another valid next proof boundary is one
asynchronous registration and resume-gate path.

Every step must run:

```sh
lake build
lake test
lake exe interpreterBench
```

Compare medians from the same machine and benchmark configuration. Reject a
change with a material regression until its cost is understood and accepted.
