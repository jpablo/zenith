# Resource management

This folder contains resource-lifetime features for the core library.

* `Scope.lean` defines dynamic scopes, finalizers, scoped fibers, and
  `Z.acquireRelease`.
* `Layer.lean` defines service layers, composition, sharing, and parallel
  acquisition.
* [`Internal/`](Internal/README.md) contains the high-universe runtime used by
  layers. It is an implementation detail.

These modules are part of `import Z`.
