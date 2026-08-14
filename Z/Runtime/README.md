# Zenith runtime

This folder contains the runtime that executes Zenith effects.

* `Interpreter.lean` evaluates the deep `Z` instruction representation.
* `Fiber.lean` defines fiber handles, state, completion, and interruption.
* `Interruption.lean` and `InterruptStatus.lean` track cancellation state.
* `Metadata.lean`, `Models.lean`, and `Trace.lean` define runtime metadata and
  execution-observation interfaces.

Most applications use this runtime through `Z.unsafeRunSync`, `fork`, and the
standard effect combinators rather than importing these modules directly.
