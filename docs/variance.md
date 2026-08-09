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
Zenith therefore uses a project-specific conversion relation and focused
type-class coercions to approximate these rules.

## The conversion relation

Zenith defines its own conversion relation in
[`Z/Coercions.lean`](../Z/Coercions.lean):

```lean
class CanConvert (A : Type u) (B : Type v) : Type (max u v) where
  coe : A -> B

infixl:65 " <: " => CanConvert

def impossible {T : Empty -> Type _} (value : Empty) : T value :=
  Empty.rec T value

instance (priority := low) : A <: A := ⟨id⟩
instance : Empty <: A := ⟨impossible⟩
instance : A <: Unit := ⟨fun _ => ()⟩
```

For this relation, `Empty` acts as the bottom type and `Unit` acts as the top
type. The low priority on identity lets the bottom and top rules take
precedence when they overlap.

`CanConvert` is not a Lean coercion. For example, the instance `Nat <: Unit`
does not let Lean silently use a `Nat` as a `Unit`. This keeps the Scala-like
relation inside the APIs that request it. It also avoids using `CoeTC` as the
base relation for all types. Lean uses `CoeTC` as an implementation class for
its coercion system and recommends that users do not implement it directly.
Zenith now limits its direct `CoeTC` instances to the `Z` and `Layer`
boundaries.

## Precise public constructors

The public constructors return the most precise `Z` type:

```lean
def Z.succeedNow (value : A) : Z Unit Empty A
def Z.succeed (action : IO A) : Z Unit Empty A
def Z.done (exit : Exit E A) : Z Unit E A
def Z.fail [ToString E] (error : E) : Z Unit E Empty
def Z.attempt (action : IO A) : Z Unit IO.Error A
```

Zenith no longer exposes apostrophe variants of these functions. The variance
coercions adapt a precise result to its use site. For example:

```lean
example : Z R IO.Error Nat :=
  Z.succeedNow 1
```

Lean elaborates the precise constructor before it inserts the `Z` coercion.
Thus, a value with an unknown element type can need an annotation:

```lean
example : Z R IO.Error (List Issue) :=
  Z.succeedNow ([] : List Issue)
```

Framework code can use `Z.internal.succeedNow`, `Z.internal.succeed`,
`Z.internal.done`, `Z.internal.fail`, and `Z.internal.attempt`. These builders
let the surrounding implementation select `R` and `E`, which can avoid
unnecessary conversion nodes. They are not part of the public API.

The current `sandbox` uses the precise public `fail` constructor:

```lean
def Z.sandbox (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail cause) pure
```

## Simulated `Z` variance

Zenith uses `CoeTC` only at the boundary where a complete `Z` value becomes
another `Z` value. The current instances are equivalent to:

```lean
instance [conversion : R₀ <: R₁] :
    CoeTC (Z R₁ E A) (Z R₀ E A) :=
  ⟨Z.contramap conversion.coe⟩

instance [conversion : E₀ <: E] :
    CoeTC (Z R E₀ A) (Z R E A) :=
  ⟨Z.mapFailure conversion.coe⟩

instance [conversion : A <: B] :
    CoeTC (Z R E A) (Z R E B) :=
  ⟨Z.map conversion.coe⟩

instance (priority := low)
    [environment : R₀ <: R₁]
    [error : E₀ <: E₁]
    [success : A₀ <: A₁] :
    CoeTC (Z R₁ E₀ A₀) (Z R₀ E₁ A₁) :=
  ⟨Z.adapt environment.coe error.coe success.coe⟩
```

The first three instances change one parameter. The low-priority fallback
changes all parameters in one coercion. This fallback is important because
Lean does not reliably chain several user-defined `CoeTC` conversions.

The conversions support one-axis and multi-axis cases:

```lean
example (effect : Z Unit E A) : Z R E A := effect
example (effect : Z R Empty A) : Z R E A := effect
example (effect : Z R E Empty) : Z R E A := effect
example (effect : Z Unit Empty Empty) : Z R E A := effect
```

For example, `Z.fail cause` changes both the environment and success types in
this handler:

```lean
example (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail cause) pure
```

For code that must control the conversion, `Z.adapt` accepts the three
functions directly:

```lean
def Z.adapt
    (environment : R₀ -> R₁)
    (error : E₀ -> E₁)
    (success : A₀ -> A₁)
    (self : Z R₁ E₀ A₀) : Z R₀ E₁ A₁
```

`Layer` has the same three one-axis coercions, the same low-priority combined
coercion, and an explicit `Layer.adapt` operation.

## Runtime representation cost

The public `Z` type is now a shallow environment wrapper. It closes its
environment into a deep `ZCore Unit E A` instruction tree.

The `Z` variance operations still add nodes to that instruction tree:

- `map` adds a success continuation.
- `mapFailure` adds a failure continuation.
- `contramap` changes the environment before closure and currently adds a
  `ZCore.contramap id` node.

A one-axis coercion uses its specialized instance, so it adds only the node
for that axis. A multi-axis coercion uses `adapt` and adds all three nodes,
including identity conversions on unchanged axes. An explicit `adapt` has the
same cost. A conversion from `Empty` is safe because no `Empty` value can
reach the conversion function.

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
