import Tests.Support

/-!
Regression tests for keyed-layer graph planning.

The planner binds a candidate input to the external environment when the key
exists there, while `andThenInto` gives dependency outputs priority, so the
plan and the running graph can disagree. Add the reproducing test here.
-/

def keyedRegressionTests : List (String × IO Unit) := []
