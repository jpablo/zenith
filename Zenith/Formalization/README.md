# Zenith formalization

This folder contains optional machine-checked statements about Zenith.

* `CoreLaws.lean` proves pure laws for causes, exits, interruption status, and
  ordinary environment projection.
* `TypeAlgebra.lean` formalizes the abstract requirement-intersection and
  error-union algebra, including generic canonical normal forms.
* `ServiceKeyLaws.lean` formalizes service-key rows and their laws. It supports
  the optional `Zenith.Services` layer API and documents the properties that
  the runtime code must preserve.
* `ServiceRowConnection.lean` connects stable keyed rows and typed projections
  to the abstract requirement algebra.
* `ErrorShape.lean` models the public nested-`Sum` error representation and
  maps it to the abstract error algebra.
* `VarianceLaws.lean` checks the production `Z` variance, coercion, and
  heterogeneous composition signatures.
* `SequentialCore.lean` defines the pure sequential `ZCore` subset, its
  terminating evaluation relation, and its lowering to production `ZCore`
  nodes. It is the first interpreter-correctness boundary.
* `SequentialMachine.lean` defines the corresponding typed stack machine and
  proves that each direct model evaluation reaches the same final machine exit.
* `SequentialRuntimeStack.lean` connects verified pure continuation stacks to
  the production `Stack` representation and proves exact frame-count
  preservation.
* `SequentialRuntime.lean` defines a pure transition relation for the
  sequential production interpreter branches. It records the available
  environment and `CanProvide` evidence that `runLoop` saves in each frame,
  and proves one-step and finite-sequence refinement from the pure machine.
* `SequentialDispatcher.lean` checks reduction laws for the executable
  sequential dispatcher. Its general instruction and continuation theorems
  show that lowered model instructions and lowered continuation frames make
  the same routing choices as the pure production transition relation.

This is a separate optional library. Application programs do not need to
import it.
