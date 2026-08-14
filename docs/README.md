# Design documentation

This folder contains design notes and implementation explanations for Zenith.

The Markdown files explain the current implementation:

* `run-loop.md` describes the interpreter.
* `module-dependencies.md` describes the source and import layout.
* `causes.md`, `parallelism.md`, `scopes.md`, and `schedules.md` describe core
  effect semantics.
* `deferred.md`, `queues.md`, and `streams.md` describe concurrent APIs.
* `http.md` and `debugging.md` describe optional integrations.
* `Problems.md`, `variance.md`, and `intersection-types.md` record design
  questions and link to their checked Lean artifacts.

Checked formalizations are in `Zenith/Formalization`; compile-time regression
cases are in `Tests`; and executable examples are in `Examples`.
