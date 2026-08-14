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

Environment conversion also uses `Environment.CanProvide`. The relation
supplies a required environment from the complete available environment. It
first tries `IsComponent`, then uses `CanConvert` as a low-priority fallback:

```lean
class Environment.CanProvide
    (Available : Type u) (Required : Type v) where
  provide : Available -> Required
```

`CanConvert` is not a Lean coercion. For example, the instance `Nat <: Unit`
does not let Lean silently use a `Nat` as a `Unit`. This keeps the Scala-like
relation inside the APIs that request it. It also avoids using `CoeTC` as the
base relation for all types. Lean uses `CoeTC` as an implementation class for
its coercion system and recommends that users do not implement it directly.
Zenith now limits its direct `CoeTC` instances to the `Z` and `Layer`
boundaries.

## Error-channel joins

`ErrorChannel.Join Left Right Result` contains two injections into a common
error type. It keeps one side when `CanConvert` can contain the other side.
For unrelated errors, it uses `Sum Left Right`:

```lean
class ErrorChannel.Join (Left : Type u) (Right : Type v)
    (Result : outParam (Type w)) where
  left : Left → Result
  right : Right → Result
```

`Z.flatMapJoin` uses this relation for effects with one environment.
`Z.flatMapMeetJoin` also combines different environment requirements. This is
a tagged runtime union. It is not a kernel union type or a proof of Scala's
complete least-upper-bound rules.

`ErrorChannel.CanInject Source Target` converts an action error into a
normalized joined error. Its rules recurse through `Sum` on either side. This
lets the elaborator reorder and reassociate existing error sums.

## Precise public constructors

The public constructors return the most precise `Z` type:

```lean
def Z.succeed (value : A) : Z Unit Empty A
def Z.fromIO (action : IO A) : Z Unit Empty A
def Z.done (exit : Exit E A) : Z Unit E A
def Z.fail [ToString E] (error : E) : Z Unit E Empty
def Z.attempt (action : IO A) : Z Unit IO.Error A
```

Zenith no longer exposes apostrophe variants of these functions. The variance
coercions adapt a precise result to its use site. For example:

```lean
example : Z R IO.Error Nat :=
  Z.succeed 1
```

Lean elaborates the precise constructor before it inserts the `Z` coercion.
Thus, a value with an unknown element type can need an annotation:

```lean
example : Z R IO.Error (List Issue) :=
  Z.succeed ([] : List Issue)
```

Framework code can use `Z.internal.succeedNow`, `Z.internal.succeed`,
`Z.internal.done`, `Z.internal.fail`, and `Z.internal.attempt`. These builders
let the surrounding implementation select `R` and `E`, which can avoid
unnecessary conversion nodes. They are not part of the public API.

The current `sandbox` uses the precise public `fail` constructor:

```lean
def Z.sandbox (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseM (fun cause => Z.fail cause) pure
```

## Simulated `Z` variance

Zenith uses `CoeTC` only at the boundary where a complete `Z` value becomes
another `Z` value. The current instances are equivalent to:

```lean
instance [conversion : Environment.CanProvide R₀ R₁] :
    CoeTC (Z R₁ E A) (Z R₀ E A) :=
  ⟨Z.contramap conversion.provide⟩

instance [conversion : E₀ <: E] :
    CoeTC (Z R E₀ A) (Z R E A) :=
  ⟨Z.mapFailure conversion.coe⟩

instance [conversion : A <: B] :
    CoeTC (Z R E A) (Z R E B) :=
  ⟨Z.map conversion.coe⟩

instance (priority := low)
    [environment : Environment.CanProvide R₀ R₁]
    [error : E₀ <: E₁]
    [success : A₀ <: A₁] :
    CoeTC (Z R₁ E₀ A₀) (Z R₀ E₁ A₁) :=
  ⟨Z.adapt environment.provide error.coe success.coe⟩
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
  self.foldCauseM (fun cause => Z.fail cause) pure
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

`Z.widen` is a shorter explicit operation when only the environment and error
types must change. The result annotation selects both target types:

```lean
example (selectNat : Bool) : Z (Nat × String) Empty String := do
  if selectNat then
    pure "nat"
  else
    Z.widen (R := Nat × String) (E := Empty) (Z.environment String)
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

Environment combination remains a separate problem. The complete property
catalogue and encoding analysis are in
[`core-type-algebra.md`](core-type-algebra.md).

In Scala, a contravariant environment parameter lets the compiler infer an
intersection such as `Int & String`. In Zenith, products represent combined
requirements:

```lean
def combinedEnvironment : Z (Nat × String) Empty (Nat × String) := do
  let environment <- Z.environment (Nat × String)
  pure (environment.get Nat, environment.get String)
```

`Environment` is a reducible abbreviation. This lets Lean see that an
environment action returns the requested service type. With an expected
result type, the standard `do` form now works:

```lean
def combinedEnvironment : Z (Nat × String) Empty (Nat × String) := do
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)
```

The expected `R` still controls the complete block. Code can make this
selection explicit with `Z.flatMapIn`:

```lean
def combinedEnvironment : Z (Nat × String) Empty (Nat × String) :=
  Z.flatMapIn (Z.environment Nat) fun nat =>
    (Z.environment String).map fun string =>
      (nat, string)
```

Some standard `do` control-flow forms still send an incomplete success type
into each branch before a `Z` coercion can run. The expected-environment form
of `zdo` handles these cases:

```lean
def selected (chooseNat : Bool) : Z (Nat × String) Empty String := zdo
  if chooseNat then
    let _ <- Z.environment Nat
    pure "nat"
  else
    Z.environment String
```

With a complete expected type, `zdo` uses `Z R E A` as the complete
environment and error type. It first infers the precise type of each action.
It then uses
`Environment.CanProvide` and the error conversion relation to widen that
action before `bind`. A private action elaborator applies the same operation
to terminal actions before Lean fixes their branch type.

The `zdo[E]` form infers the environment. `E` is the explicit error type:

```lean
def combinedEnvironment := zdo[Empty]
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

-- combinedEnvironment : Z (Nat × String) Empty (Nat × String)
```

The elaborator gives each action a private requirement slot. It elaborates
the complete control-flow block against one temporary environment. It then
flattens nested products, removes `Unit` and `PUnit`, and sorts the service
types with Lean's structural expression order. Finally, it combines the
normalized slots with `Environment.Meet` and resolves the environment
projections. `Environment.Meet` keeps one side when that side already
provides the other side. This removes duplicate and contained requirements.
Otherwise, it uses a product. `Z.flatMapMeet` exposes the binary operation
without notation.

The inferred form works across environment universes. It also removes
non-adjacent duplicate requirements. Reordered and differently associated
requirements now infer the same environment type.

This environment normalization is used by `zdo[E]` and by plain inferred
`zdo`. The structural sort is an implementation order, not a public
service-key order. Thus, it gives stable types for the same elaborated service
types, but it is not yet a general keyed row encoding.

Plain `zdo` without a complete expected type infers both the environment and
the error. It flattens nested `Sum` errors, removes `Empty`, sorts the error
types, and folds them with `ErrorChannel.Join`:

```lean
def inferred := zdo
  let first <- (Z.succeed 1 : Z Unit String Nat)
  let second <- Z.attempt (pure 2)
  pure (first + second)

-- inferred : Z Unit (IO.Error ⊕ String) Nat
```

A bare `throw IO.Error` keeps Zenith's existing meaning: it creates a defect
in `Z R Empty A`. It does not add `IO.Error` to the typed error channel. Use
`Z.fail` for a typed error value or `Z.attempt` for an `IO` failure.

This form supports binds, `if`, `match`, loops, `return`, nested actions, and
native `try/catch/finally`. A catch creates separate inference scopes for the
protected body and the handler. The body error is handled. Thus, only the
handler error contributes to the enclosing block. The two environment
requirements are combined and then included in the enclosing normalized
environment.

The scoped catch forwards early `return`, mutable variables, `break`, and
`continue` through the same control transformers that Lean uses for standard
`do` notation. A protected body with error type `Empty` uses the existing
`IO.Error` defect catch behavior. A body with a nonempty error channel catches
that typed error. Catch patterns and multiple catch clauses are supported.
Clauses use Lean's standard source order. A later clause handles an error from
the protected body or from an earlier handler. A successful handler skips all
later clauses.

A finalizer has its own inferred environment and error. Its requirements are
combined with the protected effect. It runs after success or failure and
before an early `return`, `break`, or `continue` resumes. If both the protected
effect and finalizer fail, a sequential `Cause` keeps both failures. `zdo[E]`
remains available when the complete error type must be explicit.

`Z.catchAllMeet` remains the direct compositional form. It combines the body
and handler environments and exposes only the handler error.

This is a capability meet for Zenith environments. It is not a general Scala
intersection type. Products remain noncommutative outside normalized
inference.

The larger checked example is
[`Examples/GithubIssueSync.lean`](../Examples/GithubIssueSync.lean). It uses
four services, normalized source errors, reordered actions, two catch clauses,
an audit finalizer, and fake services composed by `Z.provide`. Its environment
uses the stable keyed row rather than a product. The runtime cases are in
[`Tests.lean`](../Tests.lean).

The checked examples are in [`Tests/Variance.lean`](../Tests/Variance.lean).
Run them from the project root:

```sh
lake env lean Tests/Variance.lean
```
