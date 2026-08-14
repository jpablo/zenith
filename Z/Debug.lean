import Z
import Z.Debug.Graphviz

open System
open IO

/-!
Optional debugging and visualization support for Zenith.

Importing `Z` does not load Graphviz output support. Import `Z.Debug` when a
program must write an execution diagram.
-/

namespace Z.Debug

/-- Run an effect and write its execution diagram in Graphviz DOT format. -/
def runWithGraphviz
    (self : Z Unit E A)
    (file : String)
    (fiberId : FiberId := "main") : IO (Exit E A) := do
  let handle ← FS.Handle.mk file FS.Mode.write
  let exit ← Z.unsafeRunSyncWithDiagram self (GraphViz.graphvizIO handle) fiberId
  handle.flush
  pure exit

end Z.Debug
