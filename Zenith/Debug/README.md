# Debug and visualization support

This folder implements optional execution-diagram output.

* `Graphviz.lean` writes an `ExecutionDiagram` as Graphviz DOT text.
* `Colors.lean` defines the DOT color scheme.
* `../Debug.lean` exposes `Zenith.Debug.runWithGraphviz` as the public entry
  point.

Import `Zenith.Debug` only when a program must write a diagram. Normal core
programs should import `Z` only.
