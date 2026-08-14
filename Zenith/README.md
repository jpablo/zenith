# Optional Zenith libraries

This folder contains libraries built on the core `Z` effect system. They are
not imported by `import Z`.

* [`Http.lean`](Http.lean) defines the optional `Std.Http.Server` adapter.
* [`Debug/`](Debug/README.md) writes execution diagrams in Graphviz DOT form.
* [`Services/`](Services/README.md) adds keyed services and automatic layer
  composition.
* [`Formalization/`](Formalization/README.md) contains checked laws for the
  keyed-service model.

Import only the optional module that an application uses. For example, use
`import Zenith.Http` for an HTTP application or `import Zenith.Services` for
keyed service layers.
