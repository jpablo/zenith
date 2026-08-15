# Zenith

Zenith is an experimental effect library for Lean 4, inspired by
[ZIO](https://zio.dev/). It is a small and readable implementation for
learning, experimentation, and analysis of typed effects in Lean.

The central type is:

```lean
Z R E A
```

It describes an effect that:

| Parameter | Meaning |
| --- | --- |
| `R` | The environment or services the effect needs. |
| `E` | The typed failures the effect can report. |
| `A` | The successful result value. |

Zenith is not a drop-in replacement for Scala ZIO. It adapts useful ZIO ideas
to Lean's type system, runtime, and ordinary `IO` boundary.

## What it includes

The core `import Z` API provides:

* Typed effects, typed failures, defects, and structured failure causes.
* Product-based environment access and provision.
* Sequential composition, error recovery, and `zdo` inference for combined
  environment and error requirements.
* Fibers, interruption, parallel composition, races, and timeouts.
* Scopes, finalizers, resource acquisition, and layers.
* Retry and repeat schedules.
* Deferred values, bounded and unbounded queues, and streams.

Optional modules add:

* Stable service keys, automatic layer composition, and constructor-derived
  layers through `import Zenith.Services`.
* A thin `Std.Http.Server` adapter through `import Zenith.Http`.
* Graphviz execution diagrams through `import Zenith.Debug`.
* Kernel-checked laws and interpreter refinement work in
  `Zenith.Formalization`.

## Why use Zenith instead of plain Lean effects?

Lean already provides `IO`, typed `EIO`, `Task`, channels, mutable references,
and ordinary function parameters or type classes for dependencies. Zenith does
not replace those facilities. It builds a single higher-level effect model on
top of them, so the same program can compose dependencies, typed failures,
resources, and concurrency in one type.

| Plain Lean facility | Zenith addition | Practical result |
| --- | --- | --- |
| `IO A` and `EIO E A` | `Z R E A` tracks requirements, typed failures, and success values together. | Composition can calculate the combined environment and error channel. |
| Explicit parameters, `ReaderT`, or type classes | `Environment`, `provideEnvironment`, and optional keyed services. | A program states the services it needs; callers provide them at the execution boundary. |
| `EIO` exceptions and `IO.Error` | Typed failures, defects, interruption, and structured `Cause` values. | Recovery can handle expected failures without silently treating defects or interruption as ordinary errors. |
| `Task`, `IO.asTask`, and cooperative cancellation | `Fiber`, `join`, `interrupt`, `zipPar`, `race`, and `timeout`. | Concurrent effects use the same environment and failure model as sequential effects. |
| Manual `try`/`finally` and lifetime conventions | `Z.acquireRelease`, `Z.scoped`, scopes, and resource layers. | Resource release belongs to a scope and remains connected to the effect that acquired it. |
| Manual retry loops and shared mutable coordination | `Schedule`, `Deferred`, `Queue`, and `Stream`. | Common retry, repeat, buffering, and bounded-concurrency policies are reusable values. |
| Ad hoc logs or debugger output | Optional Graphviz execution traces and a formalization project. | Interpreter behavior can be inspected and selected core properties can be checked in Lean. |

The gain is mainly **compositional structure**. For example, a `flatMap` can
combine the requirements of two effects and join their typed errors, while
`zdo` helps infer that result. In plain Lean, these capabilities are available
as lower-level parts, but application code must choose and connect them.

Zenith also has costs. It adds an abstraction, runtime allocations, and an
interpreter boundary. Use plain `IO`, `EIO`, or direct tasks for a small
program or when Zenith's effect composition does not give a clear benefit.

## Quick start

Install [Elan](https://github.com/leanprover/elan), then clone and build the
project. Elan installs the Lean and Lake versions pinned in `lean-toolchain`.

```sh
git clone https://github.com/jpablo/zenith.git
cd zenith
lake build
lake test
```

`lake test` includes deliberate compile-time failures. Lean prints their
diagnostics, but the command succeeds when the complete regression suite
passes.

### Your first Zenith program

Create a Lean executable with this program:

```lean
import Z

def hello : Z Unit Empty Unit :=
  Z.fromIO <| IO.println "Hello from Zenith"

def main : IO Unit := do
  match ← Z.unsafeRunSync hello "hello" with
  | .success () => pure ()
  | .failure cause =>
      throw (IO.userError s!"Zenith program failed: {cause}")
```

`Z.fromIO` lifts an ordinary Lean `IO` action. `Z.unsafeRunSync` is the
explicit boundary that runs a closed `Z Unit E A` effect and returns an
`Exit E A`.

For environment access, typed error handling, `zdo`, asynchronous effects,
fibers, scopes, schedules, and streams, see
[Examples/Basic.lean](Examples/Basic.lean) and the other
[runnable examples](Examples/README.md).

## Run the included programs

| Command | What it does |
| --- | --- |
| `lake exe z` | Runs the core API examples and writes Graphviz DOT traces. |
| `lake exe githubIssueSync` | Runs the self-contained GitHub issue-sync demo. |
| `lake exe scopedResource` | Demonstrates acquisition, use, and release of a scoped resource. |
| `lake exe queueWorkerPool` | Demonstrates bounded concurrent worker-pool processing. |
| `lake exe stableServiceKeys` | Demonstrates keyed services and automatic layer composition. |
| `lake exe todoReport [ROOT] [OUTPUT]` | Generates a Markdown TODO report for a workspace. |
| `lake exe httpServer` | Starts the HTTP demo on `127.0.0.1:8080`; request `/health`, then press Enter to stop it. |

Graphviz is optional. Install it only when you want to render generated DOT
files as SVG:

```sh
dot -Tsvg diagrams/example.dot -o diagrams/example.svg
```

## Development commands

```sh
# Build all targets.
lake build

# Run runtime and compile-time regression checks.
lake test

# Build only the optional formalization library.
lake build ZenithFormalization

# Run the fast interpreter benchmark profile.
lake exe interpreterBench --quick
```

The benchmark records reference results and comparison rules in
[Benchmarks/README.md](Benchmarks/README.md). Compare runs on the same
machine and benchmark configuration.

## Project layout

| Location | Contents |
| --- | --- |
| [`Z/`](Z/README.md) | Core effect type, runtime, concurrency, resources, schedules, and syntax. |
| [`Zenith/`](Zenith/README.md) | Optional HTTP, debugging, keyed-service, and formalization modules. |
| [`Examples/`](Examples/README.md) | Small and runnable programs. |
| [`Tests/`](Tests/README.md) | Runtime and compile-time regression checks. |
| [`Benchmarks/`](Benchmarks/README.md) | Interpreter benchmark executable and recorded results. |
| [`docs/`](docs/README.md) | Design notes, API explanations, and formalization guides. |
| [`blueprint/`](blueprint/README.md) | Whole-project formalization status map and dependency-graph source. |
| [`diagrams/`](diagrams/README.md) | Generated and hand-written Graphviz artifacts. |

## Formalization

Zenith uses incremental refinement. It first proves a small semantic model,
then relates it to production representations and executable routing. The
current checked work includes the requirement/error type algebra, service-row
connection, variance signatures, and a pure sequential interpreter boundary.

Start with the [formalization study guide](docs/formalization-study-guide.md).
Use the [formalization blueprint](blueprint/README.md) for the current proof
status, dependencies, and next boundaries.

To generate the local blueprint site:

```sh
leanblueprint web
leanblueprint serve
```

See [blueprint/README.md](blueprint/README.md) for setup, declaration checks,
and the distinction between kernel-proved, production-connected, and
fixture-checked claims.

## Design notes

Useful starting points are:

* [Interpreter and runtime loop](docs/run-loop.md)
* [Interpreter refactor and performance constraints](docs/interpreter-refactor-plan.md)
* [Requirements, errors, and variance](docs/core-type-algebra.md)
* [Resource scopes](docs/scopes.md)
* [Parallel effects and causes](docs/parallelism.md)
* [Schedules](docs/schedules.md)
* [Queues and streams](docs/queues.md)
* [Optional HTTP integration](docs/http.md)
