# Design documentation

This folder contains design notes and implementation explanations for Zenith.

The Markdown files explain the current implementation:

* `run-loop.md` describes the interpreter.
* `interpreter-refactor-plan.md` defines the interpreter refactor and
  performance constraints that support formal verification.
* `module-dependencies.md` describes the source and import layout.
* `causes.md`, `parallelism.md`, `scopes.md`, and `schedules.md` describe core
  effect semantics.
* `deferred.md`, `queues.md`, and `streams.md` describe concurrent APIs.
* `http.md` and `debugging.md` describe optional integrations.
* `Problems.md`, `variance.md`, and `core-type-algebra.md` record design
  questions and link to their checked Lean artifacts.
* `formalization-study-guide.md` gives a guided reading order for the type
  algebra and interpreter formalization.

The [formalization blueprint](../blueprint/README.md) is the project-wide
status map. It shows the formalization dependencies, the current proof
boundary, and the next planned boundaries.

Checked formalizations are in `Zenith/Formalization`; compile-time regression
cases are in `Tests`; and executable examples are in `Examples`.
