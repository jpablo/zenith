# Zenith Formalization Blueprint

This folder is the status map for the whole Zenith formalization. It makes
three things visible in one place:

1. What Lean's kernel has checked.
2. What production code is connected to those proofs.
3. What remains outside the current proof boundary.

The source is in `src/`. The generated web site is intentionally not
committed. It contains the dependency graph and the Lean declaration names
for each completed claim. Its declaration buttons resolve to public API pages
after the optional `doc-gen4` documentation deployment is added.

## Read the status first

The main status page is `src/content.tex`. Its current position is:

* The requirement/error algebra is kernel-proved.
* Stable service rows, nested error shapes, and variance are connected to the
  production representation at the stated evidence level.
* The pure sequential interpreter model, its typed stack machine, and the
  extracted sequential dispatcher are kernel-proved and connected.
* The next proof boundary is the interpreter driver and one asynchronous
  registration/resume-gate path. Fibers, interruption, scheduling, logging,
  and other runtime behavior are not yet covered by this correctness proof.

Read this file with [the formalization study guide](../docs/formalization-study-guide.md)
for the method and a suggested learning order.

## Build and check

From the repository root, first check the Lean formalization:

```sh
lake build ZenithFormalization
lake env lean blueprint/Declarations.lean
```

The second command verifies the Lean names that the blueprint currently
links to. It is a local replacement for the optional `checkdecls` integration.

To generate the browser version, install the official
[`leanblueprint`](https://github.com/PatrickMassot/leanblueprint) tool and run:

```sh
leanblueprint web
```

This writes the site to `blueprint/web/`. The PDF command requires a TeX
engine and `latexmk`:

```sh
leanblueprint pdf
```

The project does not yet add the optional `checkdecls`, `doc-gen4`, GitHub
Pages, or continuous-integration setup. Add those only when the project is
ready to publish the generated documentation.

## Update procedure

When a proof boundary changes:

1. Change the matching status entry in `src/content.tex`.
2. Add or update its `\\lean{...}` declaration link.
3. Add the declaration to `Declarations.lean`.
4. Run the two Lean commands above.
5. For executable-interpreter changes, also run `lake test` and the relevant
   interpreter benchmark as described in
   [the interpreter refactor plan](../docs/interpreter-refactor-plan.md).

Do not mark a claim as kernel-proved when it is only a compile-time fixture or
a production-shaped specification. The status legend in the blueprint makes
this distinction explicit.
