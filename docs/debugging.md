# Execution tracing

`import Z` contains the interpreter and the generic `ExecutionDiagram`
observer interface. It does not import Graphviz output support.

To write a Graphviz DOT execution diagram, import `Z.Debug`:

```lean
import Z.Debug

def run : IO (Exit Empty Unit) :=
  Z.Debug.runWithGraphviz (Z.succeedNow ()) "diagrams/example.dot" "example"
```

`Z.Debug.runWithGraphviz` opens the DOT file, installs the Graphviz observer,
and runs the effect. The result is the normal `Exit E A` value.

Advanced integrations can define an `ExecutionDiagram (IO Unit)` and pass it
to `Z.unsafeRunSyncWithDiagram` or `Layer.runWithDiagram`. This lets an
application send the same events to a different output system without adding
Graphviz to its dependencies.
