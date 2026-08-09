# Variance

ZIO uses declaration-site variance to express how environment, error, and
success types can change:

```scala
sealed trait ZIO[-R, +E, +A]
```

- `R` is contravariant.
- `E` is covariant.
- `A` is covariant.

For example, an effect with environment `Any` and error `Nothing` can be used
where an effect with more environment requirements or more error types is
expected.

Lean does not have declaration-site variance or a general subtype relation.
Zenith therefore uses type-class coercions to approximate these rules.

## The conversion relation

Zenith defines a local conversion relation in
[`Z/Coercions.lean`](../Z/Coercions.lean):

```lean
infixl:65 " <: " => CoeTC

def impossible {T : Empty -> Type _} (value : Empty) : T value :=
  Empty.rec T value

instance : A <: A := ⟨id⟩
instance : Empty <: A := ⟨impossible⟩
instance : A <: Unit := ⟨fun _ => ()⟩
```

For this relation, `Empty` acts as the bottom type and `Unit` acts as the top
type. These instances do not add a general subtype system to Lean.

The notation uses `CoeTC`, not `Coe`. Lean defines `CoeTC` as the auxiliary
class that implements the transitive closure of ordinary `Coe` instances.
Zenith deliberately queries it as a relation in constraints such as
`[A <: B]`. It includes identity and ordinary `Coe` conversions. Lean's source
notes that users should generally not implement `CoeTC` directly, so this is a
deliberate implementation choice in Zenith.

## Precise and context-polymorphic constructors

The current API keeps both precise constructors and constructors that adopt
their environment and error types from the expected context:

```lean
def Z.succeedNow' (value : A) : Z R E A
def Z.succeedNow  (value : A) : Z Unit Empty A

def Z.fail' [ToString E] (error : E) : Z R E Empty
def Z.fail  [ToString E] (error : E) : Z Unit E Empty
```

`succeedNow` and `fail` give the most precise types. The apostrophe variants
are useful inside combinators because their unspecified parameters can match
the surrounding context.

For example, the current `sandbox` implementation uses `fail'`:

```lean
def Z.sandbox (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail' cause) pure
```

Here, `fail'` adopts `R` from the expected result. The success type changes
from `Empty` to `A` through the covariant success conversion.

## Simulated `Z` variance

The current `Z` instances are:

```lean
instance [conversion : R₀ <: R₁] : (Z R₁ E A) <: (Z R₀ E A) :=
  ⟨Z.contramap conversion.coe⟩

instance [conversion : E₀ <: E] : (Z R E₀ A) <: (Z R E A) :=
  ⟨Z.mapFailure conversion.coe⟩

instance [conversion : A <: B] : (Z R E A) <: (Z R E B) :=
  ⟨Z.map conversion.coe⟩
```

Each instance changes one parameter. These conversions support cases such as:

```lean
example (effect : Z Unit E A) : Z R E A := effect
example (effect : Z R Empty A) : Z R E A := effect
example (effect : Z R E Empty) : Z R E A := effect
```

Lean does not automatically chain these conversions when more than one `Z`
parameter must change. The built-in `CoeTC` closure chains ordinary `Coe`
instances after a `CoeTC` instance. Zenith's `Z` variance instances are
themselves `CoeTC` instances, so two of them do not form that chain. For
example, `Z.fail cause` has both the wrong environment and the wrong success
type for the handler below:

```lean
-- Does not compile.
example (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail cause) pure
```

An explicit intermediate type makes both conversions work:

```lean
example (cause : Cause E) [ToString E] : Z R (Cause E) A :=
  let environmentWide : Z R (Cause E) Empty := Z.fail cause
  let resultWide : Z R (Cause E) A := environmentWide
  resultWide
```

In normal combinator code, using `fail'` is shorter.

## Runtime representation cost

The public `Z` type is now a shallow environment wrapper. It closes its
environment into a deep `ZCore Unit E A` instruction tree.

The variance operations still add nodes to that instruction tree:

- `map` adds a success continuation.
- `mapFailure` adds a failure continuation.
- `contramap` changes the environment before closure and currently adds a
  `ZCore.contramap id` node.

A conversion from `Empty` is safe because no `Empty` value can reach the
conversion function. The instruction node still has an interpreter cost.

## Combining environment requirements

Environment combination remains a separate problem. In Scala, a
contravariant environment parameter lets the compiler infer an intersection
such as `Int & String`. In Zenith, products represent combined requirements:

```lean
def combinedEnvironment : Z (Nat × String) Empty (Nat × String) := do
  let environment <- Z.environment (Nat × String)
  pure (environment.get Nat, environment.get String)
```

The current monad instance fixes `R` for the complete `do` block. Therefore,
the following form does not compile:

```lean
-- Does not compile.
def combinedEnvironment : Z (Nat × String) Empty (Nat × String) := do
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)
```

The first statement selects `Z Nat Empty` as the monad. The second statement
needs `Z String Empty`, while the declared result needs
`Z (Nat × String) Empty`.

Zenith already has `IsComponent`, written `A ∣ R`, for environment projection.
A helper can use it to widen each environment request explicitly:

```lean
def environmentPart (A : Type) [A ∣ R] : Z R Empty (Environment A) :=
  (Z.environment A).contramap fun environment : Environment R =>
    environment.get A

def combinedEnvironment : Z (Nat × String) Empty (Nat × String) := do
  let nat <- environmentPart Nat
  let string <- environmentPart String
  pure (nat, string)
```

This code works, but requirement inference is manual. Automatic combination
of environment requirements is still an open design problem.

The checked examples are in [`variance.lean`](variance.lean). Run them from the
project root:

```sh
lake env lean docs/variance.lean
```
