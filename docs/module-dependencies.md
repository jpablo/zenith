# Module dependencies

Lean has a strict linear dependency order, which includes definitions within the same file and imports between files.

The module `Z` is declared in `lakefile.lean` as 

```
lean_lib Z
```

The entry point for the module is the file `zenith/Z.lean` (bottom box in the diagram).

This file will import files in the directory `zenith/Z/` , and create other definitions as well.

Imports are transitive, so that doing a simple `import Z` in a file will bring all the (top level) definitions and namespaces in the tree into scope.

## Source layout

The core source tree has these responsibility groups:

| Directory | Responsibility |
|---|---|
| `Z/Runtime` | Fibers, interruption, the interpreter, trace hooks, and runtime metadata |
| `Z/Concurrent` | Queues, deferred values, parallel effects, and streams |
| `Z/Resource` | Scopes, layers, and the internal high-universe layer runtime |
| `Z/Syntax` | Zenith syntax elaborators |
| `Zenith` | Optional libraries that build on `Z` |

The public effect declarations remain in the `Z` namespace. The reorganization
changes direct module-import paths only. Use these replacement imports:

| Previous path | Current path |
|---|---|
| `Z.Debug` | `Zenith.Debug` |
| `Z.ServiceKeys` or `Z.KeyedLayerMake` | `Zenith.Services` |
| `Z.ServiceKeyLaws` | `Zenith.Formalization.ServiceKeyLaws` |
| `Z.Interpreter` | `Z.Runtime.Interpreter` |
| `Z.Scope` | `Z.Resource.Scope` |
| `Z.Layer` | `Z.Resource.Layer` |
| `Z.Queue` | `Z.Concurrent.Queue` |
| `Z.Deferred` | `Z.Concurrent.Deferred` |
| `Z.Parallel` | `Z.Concurrent.Parallel` |
| `Z.Stream` | `Z.Concurrent.Stream` |
| `Z.Do` | `Z.Syntax.Do` |

## Import layers

`import Z` loads the effect runtime, standard concurrency and resource APIs,
and the generic `ExecutionDiagram` observer interface. It does not load
visualization, keyed-service, or service-proof code.

`import Zenith.Debug` loads `Z` and the Graphviz writer. The writer depends on
`Z.Runtime.Trace` and `Zenith.Debug.Colors`, so only programs that request DOT
output compile and load that implementation.

## Optional integration libraries

The HTTP server adapter is a separate library:

```
lean_lib ZenithHttp
```

It exports `Zenith.Http`. Applications that need an HTTP server write:

```
import Z
import Zenith.Http
```

`import Z` does not import `Zenith.Http`. This keeps the core effect library
independent of the Zenith HTTP adapter and its HTTP transport API.

Keyed services and automatic layer construction are also optional:

```
lean_lib ZenithServices
```

They export `Zenith.Services`, while their declarations remain in the `Z`
namespace. Applications that use `Services[...]`, `KeyedLayer`, or
`Z.provide` write:

```
import Z
import Zenith.Services
```

The checked service-row proofs are in a separate `ZenithFormalization` library
and export `Zenith.Formalization.ServiceKeyLaws`.
