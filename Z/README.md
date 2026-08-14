# Z core library

This folder contains the core Zenith effect library. `Z.lean` imports the
public modules in this folder and its subfolders.

The top-level files define the effect representation, failures, exits,
environment access, common combinators, schedules, and default services.

Subfolders divide the runtime by responsibility:

* [`Concurrent/`](Concurrent/README.md) contains concurrent data types and
  stream operations.
* [`Resource/`](Resource/README.md) contains scopes and resource layers.
* [`Runtime/`](Runtime/README.md) contains fibers and the interpreter.
* [`Syntax/`](Syntax/README.md) contains Zenith syntax extensions.

Use `import Z` in normal applications. It loads this core library but does not
load the optional libraries in `Zenith/`.
