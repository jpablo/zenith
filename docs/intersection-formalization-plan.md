# Zenith Core Type Algebra Formalization Plan

This document defines the next formalization steps for the Zenith core type
algebra. The source specification is in
[`intersection-types.md`](intersection-types.md).

## Goal

Create a checked Lean model for this effect type:

```text
ZIO[R, E, A]
```

The model has two separate type algebras:

```text
Requirement R ::= Any | Service | And(R, R)
Error       E ::= Nothing | Failure | Or(E, E)
```

The first milestone proves the abstract algebra. A later milestone connects
that algebra to the production service rows, error sums, and `Z` conversions.

## Scope

The formalization must prove:

1. Requirement and error subtyping are preorders.
2. Mutual subtyping is an equivalence relation.
3. Requirement intersection is a greatest lower bound.
4. Error union is a least upper bound.
5. Normalization preserves meaning and is idempotent.
6. Equivalent expressions have exactly equal normal forms.
7. Production environment projection and error injection agree with
   subtyping.
8. `Z` adaptation has the required environment, error, and success variance.

Items 1 through 6 are the abstract milestone. Items 7 and 8 are the
production-connection milestone.

## Non-goals

This work does not model all Scala types. It does not include member merging,
shared object identity, mixed intersection and union expressions, general
nominal inheritance, Scala inference widening, or JVM erasure.

The formal model must not depend on runtime allocation details. For example,
it can prove that an error union is a least upper bound without claiming that
Scala and Lean use the same runtime representation.

## Planned Lean artifacts

| File | Purpose |
|---|---|
| `docs/intersection-types.lean` | Abstract syntax, semantics, and proofs |
| `Zenith/Formalization/ServiceKeyLaws.lean` | Existing production service-row laws |
| `Z/ErrorChannelLaws.lean` | New production error-channel laws, if the selected representation permits them |
| `docs/intersection-types.md` | User-facing specification and final status |

Keep the abstract model in the documentation file until its public API is
stable. Do not add it to `Z.lean` during the first milestone.

## Phase 1: Define semantic syntax

Add two independent atom types and two syntax trees:

```lean
inductive Requirement (Service : Type u) where
  | any
  | service (value : Service)
  | and (left right : Requirement Service)

inductive ErrorType (Failure : Type v) where
  | nothing
  | failure (value : Failure)
  | or (left right : ErrorType Failure)
```

Use different atom types because service identity and failure identity can
have different implementations.

Define membership without normalization:

```lean
Requirement.Requires : Service -> Requirement Service -> Prop
ErrorType.Allows : Failure -> ErrorType Failure -> Prop
```

The constructors have these meanings:

```text
Requires service Any             = False
Requires service (Service x)     = service = x
Requires service (And left right)= Requires service left or
                                   Requires service right

Allows failure Nothing           = False
Allows failure (Failure x)       = failure = x
Allows failure (Or left right)   = Allows failure left or
                                   Allows failure right
```

Completion check:

- Examples show the intended membership results.
- The definitions need no ordering or normalization assumptions.

## Phase 2: Define subtyping and prove the algebra

Define requirement subtyping with reverse inclusion:

```lean
def Requirement.Subtype (left right : Requirement Service) : Prop :=
  forall service, Requires service right -> Requires service left
```

Define error subtyping with ordinary inclusion:

```lean
def ErrorType.Subtype (left right : ErrorType Failure) : Prop :=
  forall failure, Allows failure left -> Allows failure right
```

For each relation, define equivalence as subtyping in both directions.

Prove these theorem groups in this order:

1. Reflexivity and transitivity.
2. Equivalence reflexivity, symmetry, and transitivity.
3. Intersection left elimination, right elimination, and introduction.
4. Union left introduction, right introduction, and elimination.
5. Associativity, commutativity, idempotence, and identity up to equivalence.

Use the existing `IsGreatestLowerBound` definition for requirement
intersection. Add the dual `IsLeastUpperBound` definition for error union.

Completion check:

- All core laws follow from `Requires` and `Allows`.
- No proof uses production `Z` types or service rows.

## Phase 3: Add canonical normal forms

The semantic proofs need only atom equality. Exact normal forms also need a
lawful total order for atoms.

Represent each normal form as a sorted list with no duplicate atoms:

```text
RequirementNF Service = sorted unique List Service
ErrorNF Failure        = sorted unique List Failure
```

Add one shared normalization utility if the service and failure proofs are
identical. The utility must require decidable equality and a lawful total
comparator. Do not copy the production `Entry` code without first separating
the generic list laws from service-specific fields.

Prove:

1. An atom is in a normal form exactly when it is in the source syntax.
2. Normalization preserves semantic equivalence.
3. Normalization returns a sorted list with no duplicates.
4. Normalization is exactly idempotent.
5. Semantically equivalent syntax has exactly equal normal forms.
6. Normal-form merge is exactly associative, commutative, and idempotent.
7. The empty normal form is the exact identity.

Completion check:

```lean
example : normalizeRequirement (And a b) =
    normalizeRequirement (And b a) := by
  ...

example : normalizeError (Or a (Or b a)) =
    normalizeError (Or b a) := by
  ...
```

These examples must use theorem proofs. They must not depend on both sides
reducing to the same concrete expression by accident.

## Phase 4: Connect service rows to requirements

Define row membership as the positive form of the existing `Row.Fresh`
predicate. Then define row inclusion:

```text
Row.ContainsKey key entries := not Row.Fresh key entries

Row.Provides available required :=
  every key in required is also in available
```

Prove:

1. `Row.SameKeys` is semantic requirement equivalence.
2. `Row.normalize` implements abstract requirement normalization.
3. `Row.merge` implements abstract intersection.
4. A successful keyed `Environment.CanProvide` projection implies
   `Row.Provides`.
5. A `Row.Provides` proof plus row coherence is sufficient to construct the
   required projection evidence.

The coherence condition is important. Equal keys must identify the same
complete `Entry`, including its service type.

Completion check:

- The existing exact row laws become corollaries or implementation theorems
  of the abstract requirement laws.
- No theorem treats two conflicting entries with the same key as compatible.

## Phase 5: Select and verify the error representation

The current `zdo` elaborator flattens `Sum`, removes `Empty`, sorts Lean type
expressions, and then rebuilds a joined error type. This process is partly in
the elaborator, so kernel proofs cannot inspect all of it directly.

Make one explicit decision before production error proofs:

### Choice A: Keep nested `Sum`

Define a checked `ErrorShape` syntax and an interpretation to nested `Sum`.
Prove the algebra for the syntax and the interpretation functions. Keep
elaborator behavior under compile-time fixture tests.

This choice has a small implementation cost, but exact equality remains an
elaboration property rather than a kernel theorem about arbitrary Lean error
types.

### Choice B: Add stable error keys

Represent errors with a normalized keyed row, similar to service rows. This
can give kernel-checked canonical forms and direct subset proofs.

This choice has a larger implementation and migration cost. It also changes
the public error representation, so it requires a separate design decision.

Do not claim that production errors satisfy obligations 5 through 7 until
one choice has the required evidence.

## Phase 6: Connect the algebra to `Z` variance

State the effect adaptation rule:

```text
R2 <: R1    E1 <: E2    A1 <: A2
---------------------------------
Z R1 E1 A1 <: Z R2 E2 A2
```

Connect each premise to production evidence:

| Premise | Production evidence |
|---|---|
| `R2 <: R1` | `Environment.CanProvide R2 R1` |
| `E1 <: E2` | `ErrorChannel.CanInject E1 E2` or selected error evidence |
| `A1 <: A2` | `CanConvert A1 A2` |

Verify that `Z.adapt`, `Z.widen`, and the focused `CoeTC` instances implement
this direction. Add compile-time examples for each single axis and for all
three axes together.

Also verify the composition rule:

```text
Z R1 E1 A -> (A -> Z R2 E2 B) -> Z (R1 & R2) (E1 | E2) B
```

Use `zdo` examples to check that reordered requirements and errors produce
the same inferred types.

## Phase 7: Update the specification status

After the proofs pass:

1. Update the proof-obligation list in `intersection-types.md`.
2. Mark each obligation as abstract, production-connected, or deferred.
3. State the selected error representation and its limit.
4. Add links from each specification claim to its Lean theorem.
5. Remove old experimental text that conflicts with the proved model.

## Verification commands

Run these commands after each phase:

```sh
lake env lean docs/intersection-types.lean
lake build
git diff --check
```

If Phase 5 adds diagnostic fixtures, also run:

```sh
lake build Examples.KeyedLayerMakeDiagnostics
```

## Recommended commit sequence

Use one reviewable commit for each completed proof boundary:

1. Define the two semantic syntax trees and subtype relations.
2. Prove the abstract GLB, LUB, and equivalence laws.
3. Prove canonical normalization.
4. Connect stable service rows to the requirement model.
5. Select and verify the error representation.
6. Connect the model to `Z` variance and update the documentation.

## Current status

Phases 1 and 2 are complete in `docs/intersection-types.lean`. The file now
defines the two abstract syntax trees and proves their preorder, equivalence,
GLB, and LUB laws. This work is independent of the production runtime.

## Immediate next change

Start Phase 3 in `docs/intersection-types.lean`: add canonical normal forms
for both syntax trees. This phase needs a lawful total order for leaves, but
it does not require a production representation decision.
