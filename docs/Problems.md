# Universe design

## Standard `IO` is not universe-polymorphic

Lean defines:

```lean
IO : Type → Type
```

Therefore, standard `IO` cannot return a Zenith effect:

```lean
import Z

#check_failure IO (Z Unit Empty Unit)
```

Zenith uses `HEIO` when a layer must produce a value from a higher universe.
`HEIO` threads the same `IO.RealWorld` token as standard `IO`. A standard `IO`
action enters it through `HEIO.liftIO`.

## High-universe services

A service whose fields contain `Z` values lives in `Type 1`:

```lean
structure Issue where

structure Github : Type 1 where
  getIssues : String → Z Unit IO.Error (List Issue)
```

The public environment parameter of `Z` is universe-polymorphic, so this is
valid:

```lean
#check Z Github IO.Error Unit
```

Business logic selects an effectful method with `serviceWithM`:

```lean
def program : Z Github IO.Error (List Issue) :=
  Z.serviceWithM fun github =>
    github.getIssues "lean"
```

The service does not become a fiber result. `serviceWithM` closes the
environment and produces the existing deep `ZCore Unit` instruction tree. The
current interpreter then runs that tree without a new service-call fiber.

## High-universe layers

`Layer` can produce a service from any universe:

```lean
def githubLayer : Layer Unit IO.Error Github :=
  Layer.fromBuild fun _ =>
    pure {
      getIssues := fun _ => Z.succeed ([] : List Issue)
    }
```

`Layer.run` builds the service, supplies it to the program, and runs the closed
instruction tree:

```lean
def runProgram : IO (Exit IO.Error (List Issue)) :=
  githubLayer.run () program
```

Layers can own resources. `acquireRelease` accepts high-universe `HEIO`
actions. `acquireReleaseEffect` is the simpler constructor for low-universe `Z`
values. Release actions run in reverse acquisition order, including after a
program failure or a later acquisition failure.

`zipWith` builds in sequence. `zipWithPar` builds independent layers in
parallel. `memoize` and `share` give explicit scoped sharing. All other layer
builds are fresh.

The complete checked example is in [`Problems.lean`](Problems.lean). Run it
from the project root:

```sh
lake env lean docs/Problems.lean
```
