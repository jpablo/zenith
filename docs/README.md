# Design documentation

This folder contains design notes, implementation explanations, and type-level
experiments for Zenith.

The Markdown files explain the current implementation:

* `run-loop.md` describes the interpreter.
* `module-dependencies.md` describes the source and import layout.
* `causes.md`, `parallelism.md`, `scopes.md`, and `schedules.md` describe core
  effect semantics.
* `deferred.md`, `queues.md`, and `streams.md` describe concurrent APIs.
* `http.md` and `debugging.md` describe optional integrations.
* `Problems.md`, `variance.md`, and `intersection-types.md` record design
  questions and Lean encodings.

The `.lean` files are checked formalization and elaboration experiments. Verify
them with `lake env lean docs/FILE.lean`.
