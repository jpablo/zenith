# Zenith Formalization Study Guide

This guide gives a reading and practice order for Zenith's formalization. It
explains what each module proves, how the modules connect, and where the
current proof boundary ends.

The formalization is optional. Application programs do not import it. Its
purpose is to state important Zenith properties in Lean's kernel and to make
the remaining unproved behavior explicit.

## What you will learn

After this guide, you should be able to explain:

1. Why requirement intersection and error union use separate algebras.
2. How normalized service rows implement the requirement algebra.
3. Which production properties Lean proves in the kernel and which properties
   compile-time fixtures check.
4. How a direct semantic evaluator becomes a typed continuation-stack machine.
5. How that machine relates to Zenith's production Stack and pure interpreter
   model.

## Before you start

You should know basic Lean definitions, inductive types, propositions,
equality, and structural induction. You should also know these ideas:

* A type class can carry executable evidence, such as Environment.CanProvide.
* An indexed inductive type can constrain the valid next states and stack
  frames.
* A theorem in Prop is erased at runtime. It gives proof confidence but does
  not add a runtime check.

You do not need prior knowledge of Scala, ZIO, category theory, or a full
interpreter-correctness proof.

## Build commands

Run these commands from the project root:

    # Build only the optional formalization library.
    lake build ZenithFormalization

    # Build all project targets.
    lake build

    # Run runtime and compile-time regression checks.
    lake test

    # Use after a runtime-interpreter change, not for proof-only edits.
    lake exe interpreterBench --quick

Some test output reports intentional failed elaboration checks. The final
result must say that all regression tests passed.

## Methodology: formalize by refinement

We do not try to prove the whole existing project correct in one step. That
would combine typed effects, mutable state, Lean IO, callbacks, concurrency,
interruption, logging, and diagrams in one large statement. Such a statement
would be difficult to define, test, and maintain.

Instead, we use **incremental refinement**. We define a small model of one
behavior, prove facts about the model, and connect the model to progressively
more of the production implementation. Each connection reduces the gap
between what is proved and what executes.

The method has these stages:

| Stage | Question | Zenith artifact | Why it exists |
|---|---|---|---|
| 1. Select a boundary | Which behavior is small enough to state exactly? | The type algebra; then the pure sequential interpreter subset. | It prevents unrelated runtime features from hiding the core behavior. |
| 2. Specify behavior | What should that boundary mean without implementation details? | `Requirement`, `ErrorType`, `SequentialCore.Program`, and `Evaluates`. | A specification gives the proof a stable target. |
| 3. Prove the model | Is the specification internally consistent and deterministic? | GLB/LUB, normal-form, and evaluation-determinism theorems. | It finds errors in the design before it is connected to production code. |
| 4. Relate representations | How does a production value represent the model value? | Service-row, error-shape, and runtime-stack correspondence relations. | It prevents a proof about a toy model from being mistaken for a proof about Zenith data. |
| 5. Prove simulation | Does each abstract transition have a matching concrete transition? | `step_refines` and `steps_refine`. | It shows that the concrete shape can preserve the model behavior. |
| 6. Link executable code | Does the function that runs in production use the checked transition? | The planned pure dispatcher extracted from `runLoop`. | This turns the production-shaped relation into a theorem about executable code. |
| 7. Extend the boundary | Which excluded feature should enter next? | Raw IO, callbacks, interruption, fibers, and observability. | Small extensions keep failures and performance changes understandable. |

Stages 1 through 5 are complete for the current pure sequential subset.
Stage 6 is the main open interpreter task. The current
`SequentialRuntime.Step` relation mirrors `runLoop`, but `runLoop` does not
yet call an extracted dispatcher that implements the relation. Therefore, it
is a checked specification and simulation result, not yet a correctness
theorem for the executable interpreter.

### Why this method is useful

It separates three kinds of mistakes that would otherwise be mixed together:

* A **design mistake** means the intended algebra or effect semantics is
  inconsistent. Model proofs expose this early.
* A **representation mistake** means the production rows, errors, stacks, or
  environments do not carry enough information to represent the model.
  Correspondence relations expose this.
* An **implementation mistake** means the runtime driver does not follow the
  checked transition. The dispatcher-to-`runLoop` proof will expose this.

The method also keeps runtime performance under control. The current model
and its proofs are in `Prop`, so they are erased. A runtime refactor can still
be slower if it adds allocations, queues, closures, or task creation. That is
why each change to the executable interpreter must also run the benchmarks.

### What a completed proof will mean

For a selected boundary, the intended final statement has this form:

    If a model program evaluates to an exit,
    and a production program represents that model program,
    then the production interpreter reaches the corresponding exit.

This statement needs qualifications. It only covers the selected instruction
set and stated assumptions. It does not prove the Lean runtime, the operating
system, or an unmodeled callback. The value is precise: it documents exactly
which behavior is checked and exactly which behavior remains outside the
proof.

## Map of the work

Read the modules in this order:

    Core laws and type algebra
      CoreLaws ──────────────┐
      TypeAlgebra            │
      ServiceKeyLaws         │
      ServiceRowConnection ──┼──> Production type-level API
      ErrorShape             │
      VarianceLaws ──────────┘

    Sequential interpreter model
      SequentialCore
           -> SequentialMachine
           -> SequentialRuntimeStack
           -> SequentialRuntime
           -> future extracted dispatcher in runLoop

The first group studies Zenith's types. The second group studies the pure
sequential part of execution. The groups are related by the public Z and
ZCore types, but they prove different properties.

## Evidence levels

Keep these four evidence levels separate when you read a claim.

| Level | Meaning | Example |
|---|---|---|
| Kernel-proved | Lean's kernel checks a theorem for every value in its stated domain. | Requirement intersection is a GLB. |
| Production-connected | A theorem connects an abstract model to a production representation. | A coherent service row provides a required row. |
| Fixture-checked | Representative programs must elaborate, or must fail to elaborate. | Nested error Sum normalization under zdo. |
| Specification/refinement | A pure relation models a production branch, but the executable function does not yet call it. | SequentialRuntime.Step. |

Do not describe a fixture check or a pure specification as a proof of all
runtime behavior.

## Part 1: Start with small pure laws

Read [CoreLaws.lean](../Zenith/Formalization/CoreLaws.lean) first. It is the
best entry point because its definitions are close to ordinary Lean data.

Study these topics:

* Cause composition and mapping.
* Exit success and failure behavior.
* Interruption status.
* Ordinary product-environment projection.

Ask these questions while you read:

1. Which functions compute values, and which declarations only state
   propositions?
2. Which proofs finish with rfl, and which need case analysis?
3. Which environment properties depend on an explicit CanProvide instance?

Practice: select one small theorem and replace its proof with a proof that
uses cases or rfl. Then rebuild ZenithFormalization.

## Part 2: Learn the abstract type algebra

Read [TypeAlgebra.lean](../Zenith/Formalization/TypeAlgebra.lean), then read
[core-type-algebra.md](core-type-algebra.md).

There are two independent syntaxes:

    Requirement: Any | Service | And
    Error:       Nothing | Failure | Or

The direction is important:

* A requirement And(left, right) is a greatest lower bound. It requires both
  capabilities.
* An error Or(left, right) is a least upper bound. It allows either failure.

Study this sequence:

1. Membership predicates: Requirement.Requires and ErrorType.Allows.
2. The subtype preorders.
3. Mutual-subtyping equivalence.
4. GLB and LUB theorems.
5. Canonical sorted, duplicate-free normal forms.

Practice: explain why associativity is usually proved up to semantic
equivalence, while normalized expressions can be proved exactly equal.

## Part 3: Connect the algebra to Zenith values

Read these modules in order:

1. [ServiceKeyLaws.lean](../Zenith/Formalization/ServiceKeyLaws.lean)
2. [ServiceRowConnection.lean](../Zenith/Formalization/ServiceRowConnection.lean)
3. [ErrorShape.lean](../Zenith/Formalization/ErrorShape.lean)
4. [VarianceLaws.lean](../Zenith/Formalization/VarianceLaws.lean)

The key question is: What is the concrete Zenith representation of the
abstract fact?

| Abstract fact | Production representation |
|---|---|
| Requirement intersection | Normalized stable service-key row merge |
| Requirement provision | Environment.CanProvide projection |
| Error union | Nested Sum values |
| Environment contravariance | Z.adapt, Z.widen, and CoeTC |
| Error and success covariance | Z.adapt, coercions, and composition helpers |

Important limitation: arbitrary Lean error types do not have stable public
keys. Therefore, normal ordering of arbitrary error expressions is
fixture-checked, not kernel-proved. This is an intentional boundary.

Practice: inspect [Tests/Variance.lean](../Tests/Variance.lean). Classify each
case as a positive elaboration check or a deliberate failure check.

## Part 4: Read the sequential semantic model

Read [SequentialCore.lean](../Zenith/Formalization/SequentialCore.lean). This
module defines a smaller Program type with only the instructions in the first
proof boundary:

    done
    flatMap
    foldCauseM
    contramap
    environment
    provideEnvironment

Evaluates program environment exit is a big-step relation. It describes the
final result of a terminating program, without deciding how many interpreter
transitions occur.

Focus on three declarations:

* toZCore lowers the model into public production instruction nodes.
* Evaluates gives one semantic rule per model instruction.
* evaluates_deterministic proves that two derivations for the same program
  have the same final exit.

Practice: trace a flatMap that first returns 2 and then returns 3. Write down
the two Evaluates rules that apply.

## Part 5: Move to a typed stack machine

Read [SequentialMachine.lean](../Zenith/Formalization/SequentialMachine.lean).

The machine has three state forms:

    evaluate program environment stack
    resume exit stack
    halt exit

Stack E A EFinal AFinal means that the current computation can produce Exit E
A, and this stack eventually produces Exit EFinal AFinal.

Read Step before reading its proof. Then read evaluation_runs_to_resume and
evaluation_runs_to_halt. These theorems prove that direct evaluation reaches
the same final result by small machine transitions.

Practice: find the two distinct Step rules for a flatMap failure. Explain why
the success continuation does not run.

## Part 6: Connect the model to the production interpreter shape

Read these modules together:

1. [SequentialRuntimeStack.lean](../Zenith/Formalization/SequentialRuntimeStack.lean)
2. [SequentialRuntime.lean](../Zenith/Formalization/SequentialRuntime.lean)

The production Stack type indexes only its immediate continuation result.
RuntimeStack hides that immediate result type, while Corresponds keeps a proof
that every model frame has a matching production frame.

A saved production frame includes:

* A parent diagram identifier.
* An available environment.
* CanProvide evidence.
* A proof that this evidence recovers the model environment.

SequentialRuntime.Production.Step is a pure relation that mirrors the
sequential branches of runLoop, continueOrComplete, and runWithErrorHandler.
It does not run IO.

Read these theorems last:

* step_refines: one typed-machine transition has a matching production
  transition.
* steps_refine: every finite typed-machine execution has a matching production
  transition sequence.

Practice: follow one flatMap success path:

    evaluate (flatMap effect next)
    evaluate effect with a pushed production frame
    resume (success value)
    evaluate (next value) with the saved environment and tail stack

## Part 7: Know the current boundary

The current work does not prove that the executable private ZCore.runLoop
performs SequentialRuntime.Step. The relation is a checked specification that
mirrors its relevant branches.

The next work is:

1. Extract a module-visible pure dispatcher from runLoop.
2. Make that dispatcher implement SequentialRuntime.Step.
3. Connect the interpreter driver to the dispatcher.
4. Measure flatMap, sync, error recovery, immediate async, and fork/join
   before accepting the refactor.
5. Extend the proof boundary one feature at a time: raw IO, callbacks,
   interruption, fibers, logging, diagrams, and mutable state.

Read [interpreter-refactor-plan.md](interpreter-refactor-plan.md) before you
change interpreter control flow. It records the correctness and performance
constraints for this work.

## Suggested study sessions

| Session | Reading | Outcome |
|---|---|---|
| 1 | CoreLaws.lean | Understand small proposition proofs and environment evidence. |
| 2 | TypeAlgebra.lean and core-type-algebra.md | Understand the two abstract algebras. |
| 3 | Service rows, errors, and variance modules | Separate kernel proofs from production checks. |
| 4 | SequentialCore.lean | Understand direct semantics and lowering. |
| 5 | SequentialMachine.lean | Understand typed stacks and small-step execution. |
| 6 | Runtime stack and runtime relation modules | Understand the current interpreter proof boundary. |
| 7 | Both plans | Select one narrow next proof or refactor task. |

## Reference documents

* [core-type-algebra.md](core-type-algebra.md): user-facing type-algebra
  specification and evidence status.
* [core-formalization-plan.md](core-formalization-plan.md): completed phases
  and formalization work still to do.
* [interpreter-refactor-plan.md](interpreter-refactor-plan.md): safe path from
  the current pure model to the executable interpreter.
* [Zenith/Formalization/README.md](../Zenith/Formalization/README.md): short
  index of all formalization modules.
