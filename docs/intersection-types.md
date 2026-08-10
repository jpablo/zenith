# Scala Intersection Types and Lean

This document catalogues the properties that an encoding of Scala
intersection types can need. It then separates the complete problem from the
smaller environment-combination problem in Zenith.

The checked Lean sketches are in
[`intersection-types.lean`](intersection-types.lean).

## Terminology

Scala types form a preorder under `<:`. Two types are equivalent, written
`=:=`, when each is a subtype of the other.

In this order, `A & B` is the greatest lower bound of `A` and `B`. It is not
their least upper bound:

```text
A & B <: A
A & B <: B

X <: A and X <: B implies X <: A & B
```

Scala defines `A | B` as the least upper bound. The official specifications
give the [intersection subtyping rules][scala-intersection-rules] and the
[general GLB and LUB definitions][scala-types].

The environment parameter of `ZIO[-R, +E, +A]` is contravariant. This reverses
the order:

```text
A & B <: A                    A & B <: B
-------------                 -------------
Z A E X <: Z (A & B) E X     Z B E Y <: Z (A & B) E Y
```

Thus, `Z (A & B)` is a common upper bound of `Z A` and `Z B`. This is why an
environment GLB appears as a `Z` least upper bound.

## Core subtyping properties

An intersection encoding must first state which subtype relation it uses.
Scala's `<:` is a preorder:

- It is reflexive.
- It is transitive.
- Mutual subtyping defines type equivalence.

The intersection type former has three fundamental rules:

- Left elimination: `A & B <: A`.
- Right elimination: `A & B <: B`.
- Introduction: if `X <: A` and `X <: B`, then `X <: A & B`.

The introduction rule is important. Two projection functions alone show that
a candidate type is a lower bound. They do not show that it is the greatest
lower bound.

The checked companion defines `IsGreatestLowerBound` from these three rules.
It also proves that two GLBs are mutually below each other and that exchanging
the two operands preserves a GLB.

## Algebraic properties

The GLB laws give these equivalences:

```text
A & B       =:= B & A                  commutativity
(A & B) & C =:= A & (B & C)            associativity
A & A       =:= A                      idempotence
A & Any     =:= A                      top identity
A & Nothing =:= Nothing                bottom absorption
```

These are equivalences through mutual subtyping. They do not have to be
syntactic or definitional equalities.

Scala also has union types. Intersection distributes over union:

```text
A & (B | C) =:= (A & B) | (A & C)
```

Variance transports these relations through type constructors. For example,
if `F` is covariant, then:

```text
F[A & B] <: F[A] & F[B]
```

The [Scala union specification][scala-union-rules] gives the dual union rules,
the distributive rule, and examples for covariant and contravariant type
constructors.

## Value and member properties

A value of `A & B` is one value that has both types. It is not generally a
pair that contains one `A` value and one unrelated `B` value.

An intersection exposes all members of both operands. If both operands have a
member with the same name, Scala intersects the two member types. Variance can
then simplify the result. The [Scala intersection reference][scala-members]
has a checked member-merging example.

These member rules matter for a general Scala-library translation. They are
not required for the current Zenith environment, because Zenith retrieves a
service through an explicit projection instead of normal member lookup.

## Runtime properties

Scala intersection types do not normally allocate a pair or wrapper. The
intersection restricts the static type of an existing value. The compiler
selects one erased JVM type with the specified erased-GLB rules. See the
[Scala erasure rules][scala-intersection-rules].

A Lean encoding can use products, structures, existential packages, or
dictionaries. These representations can allocate data and can have different
identity semantics. A complete translation must state whether this difference
is acceptable.

## Requirements by use case

| Property | Zenith environments | General Scala translation |
|---|---:|---:|
| Left and right elimination | Required | Required |
| GLB introduction rule | Required | Required |
| Commutativity and associativity | Required | Required |
| Idempotence | Required | Required |
| `Any` and `Nothing` laws | Useful | Required |
| One underlying object | Not required | Required |
| Member merging | Not required | Required |
| Variance propagation | Environment only | Required |
| Union dual | Error-channel work | Required |
| Scala-compatible erasure | Not required | Only for JVM compatibility |

Zenith can therefore use a smaller capability-intersection model. That model
must not be presented as a complete encoding of Scala intersection types.

## Candidate Lean encodings

### Products

The current Zenith environment uses products:

```lean
Environment (A × B)
```

Products have direct projections and work across universes. They also match
the runtime meaning of a service collection, where `A` and `B` can be separate
objects.

Products do not give native intersection behavior:

- `A × B` and `B × A` are not definitionally equal.
- `(A × B) × C` and `A × (B × C)` are not definitionally equal.
- `A × A` contains two potentially different values, so it is not the same
  value set as `A`.
- Combining requirements can create nested products and duplicates.

`IsComponent` hides some order differences during projection. It does not
give a canonical result type for inference.

### Predicates on one carrier

If interfaces are predicates over a shared carrier `α`, intersection is
ordinary logical conjunction:

```lean
def Meets (P Q : α → Prop) (value : α) : Prop :=
  P value ∧ Q value
```

This encoding uses one value and satisfies the introduction, elimination,
commutativity, associativity, idempotence, top, bottom, and distributive laws.
The checked companion proves these properties.

The limitation is structural: arbitrary Lean types `A` and `B` are not
predicates over a common carrier. A Scala translator could define an explicit
object carrier and encode interfaces as predicates or dictionaries over it.

### Existential object packages

An intersection can package one hidden carrier, one value, and two views:

```lean
structure PackedAnd (A B : Type) where
  Carrier : Type
  value : Carrier
  left : Carrier → A
  right : Carrier → B
```

This preserves one underlying value and supports both eliminations. It adds a
wrapper and two dictionaries. The type also does not automatically satisfy
the algebraic laws as definitional equalities. Proofs or normalization are
still necessary.

### Capability rows

An environment can be represented as a type-level row or set of service
codes. Intersection then becomes row union with duplicate removal. A
normalized row can make associativity, commutativity, and idempotence stable
for inference.

This is a good target for Zenith, but it requires design work:

- Lean cannot sort and remove duplicates from arbitrary native `Type` values.
- A row therefore needs stable service keys or a reified type language.
- The representation must support services from different universes.
- Projection evidence must connect the normalized row to its runtime value.

### A reified Scala type language

For translation of multiple Scala libraries, the most complete direction is
to represent the required Scala type fragment as Lean data:

```lean
inductive SType where
  | interface (key : InterfaceKey)
  | intersection (left right : SType)
  | union (left right : SType)
  | top
  | bottom
```

The encoder can normalize this syntax and define subtyping over it. An
interpretation function can then map a normalized `SType` to its Lean runtime
representation.

This approach does not add intersection types to Lean's kernel. It implements
the required Scala type algebra inside Lean. It gives one direct place to
control normalization, member merging, unions, and Scala-specific rules.

## Environment meet in Zenith

Zenith now contains this production relation:

```lean
class Environment.Meet (Left : Type u) (Right : Type v)
    (Result : outParam (Type w)) where
  left : Result → Left
  right : Result → Right
```

The first rule keeps `Right` when it provides `Left`. The second rule keeps
`Left` when it provides `Right`. A low-priority fallback returns
`Left × Right`. These rules remove equal and contained requirements. They
also use `Unit` as the identity. `Z.flatMapMeet` uses the two projections to
compose heterogeneous actions.

This relation does not sort unrelated types. Thus, `Meet Nat String` returns
`Nat × String`, while `Meet String Nat` returns `String × Nat`. These types
provide the same services through `Environment.CanProvide`, but they are not
definitionally equal. Direct uses of `Environment.Meet` and `Z.flatMapMeet`
therefore remain order-dependent.

## Error join in Zenith

Zenith now has the dual production relation for error channels:

```lean
class ErrorChannel.Join (Left : Type u) (Right : Type v)
    (Result : outParam (Type w)) where
  left : Left → Result
  right : Right → Result
```

The first rule keeps `Right` when `Left <: Right`. The second rule keeps
`Left` when `Right <: Left`. A low-priority fallback returns `Sum Left Right`.
Thus, `Empty` is the identity, equal errors collapse, and unrelated errors
get separate tagged cases.

`Z.flatMapJoin` combines different error channels for one environment.
`Z.flatMapMeetJoin` combines both environment requirements and error
channels. Runtime tests check both `Sum.inl` and `Sum.inr` injection.

This relation is a capability join, not a complete encoding of Scala union
types. `Sum` allocates a tag, direct joins remain order-dependent, and the
class does not prove the universal least-upper-bound property.

Plain inferred `zdo` normalizes a collection of these joins. It flattens
nested sums, removes `Empty`, sorts the remaining types, and uses
`ErrorChannel.CanInject` to adapt each source error to the result. Thus,
reordered and differently associated errors infer the same error type.
Bare `throw IO.Error` remains a defect with error type `Empty`; typed error
inference uses `Z.fail` or `Z.attempt`.

## `Monad` and `do` notation

Lean's standard `Bind.bind` is homogeneous:

```lean
m A → (A → m B) → m B
```

The same type constructor `m` occurs in all positions. `MonadLift` can move an
action into a known target monad, but it does not compute a target from two
different environment requirements.

The appropriate abstraction for `Z` is a graded monad. The environment is the
grade, `Any` or Zenith's `Unit` is the identity grade, and intersection
combines grades:

```text
Z R₁ E A
A → Z R₂ E B
-----------------
Z (R₁ & R₂) E B
```

Lean does not provide a standard graded-monad class. However, Lean 4.32 has a
new extensible `do` elaborator. `Lean.Elab.Do.DoOps` supplies custom builders
for `pure` and `bind`, plus operations that recognize and construct monadic
types. `Lean.Elab.Do.elabDoWith` runs the standard `do` elaborator with those
operations. The [`DoOps` source][lean-do-ops] exposes this interface. The
[Lean 4.31 release notes][lean-do-release] describe the new elaborator as
extensible.

Zenith uses this integration point in `Z/Do.lean`. The `zdo` elaborator reuses
Lean's `do` parser and control-flow elaboration. It infers a fresh environment
and error type for each action, then widens the action to the expected block
type before `bind`.

The expected-type form requires an expected `Z R E A` type. It verifies that
`R` contains all requirements. Ordinary `do` remains unchanged. A private
action elaborator also widens bare terminal actions in control-flow branches
before Lean fixes the branch type.

The `zdo[E]` form gives each action a requirement slot, elaborates the block
against one temporary environment, and normalizes the requirements before it
combines them with `Environment.Meet`. Normalization flattens products,
removes `Unit` and `PUnit`, and uses Lean's structural expression order.
Thus, action order and product association do not change the inferred
environment type. This also combines requirements across `if`, `match`,
`try`, loops, `return`, and nested actions. The error type stays explicit
in this `zdo[E]` form.

Without a complete expected type, plain `zdo` applies the same method to both
the environment and the error channel. It supports binds, `if`, `match`,
loops, `return`, nested actions, and native `catch` and `finally`. The scoped `doTry`
elaborator gives the protected body and handler different monads. It removes
the body error before it joins the handler and continuation errors. It also
forwards mutable state and nonlocal control effects through Lean's control
transformers.

`Z.catchAllMeet` provides the direct compositional catch form. It combines the
body and handler environments, removes the handled error, and exposes only the
handler error. `Z.ensuringMeetJoin` provides the corresponding finalizer form.
It combines environment and error requirements and gives a finalizer failure
precedence. Plain inferred native syntax supports multiple catch clauses in
source order. A later clause can handle an error from an earlier handler.

## GitHub issue-sync integration case

[`Examples/GithubIssueSync.lean`](../Examples/GithubIssueSync.lean) tests the
encoding in one representative program. It uses configuration, GitHub, issue
store, and audit services. The checked types show these results:

```lean
syncRaw : Z RawServices SourceErrors Nat
requirementsForward : Z Services AllErrors Unit
requirementsReverse : Z Services AllErrors Unit
sync : Z Services Empty Nat
```

`requirementsForward` and `requirementsReverse` request the same four
services and four errors in opposite orders. They infer the same normalized
types. `sync` uses two ordered handlers. The first handler can fail with
`AuditError`, and the second handler catches that error. An audit finalizer
runs on every path.

The tests use local fake layers. They check normal sync, dry-run branching,
source-error recovery, handler-error recovery, finalizer execution, and an
uncaught store error from `syncRaw`.

Run the standalone in-memory demo with:

```sh
lake exe githubIssueSync
```

This is elaborator-level normalization, not a reified row type. The structural
order is not a public service-key order. Explicit service keys are still
necessary if the API needs an order that is independent of Lean's expression
representation or compiler version.

## Remaining research sequence

1. Define the Scala fragment that Zenith and other target libraries need.
2. Formalize its subtype preorder, intersection rules, and equivalence laws.
3. Decide whether cross-version API stability requires explicit service keys.
4. If explicit keys are necessary, define them and prove the row meet laws.

The issue-sync case gives the first combined test of normalized environments,
normalized errors, and ordered catch chains in a larger program.

## Stable service-key prototype

The reusable experiment is in
[`Z/Experimental/StableServiceKeys.lean`](../Z/Experimental/StableServiceKeys.lean).
The native checked example is
[`Examples/StableServiceKeysDemo.lean`](../Examples/StableServiceKeysDemo.lean).
[`stable-service-keys.lean`](stable-service-keys.lean) is its documentation
import.
The experiment does not change the production `Z` environment. Each service
entry contains an owner, a local name, and its service type. A lexical
insertion sort gives service rows a stable order and removes duplicate keys.

The prototype provides these checked properties:

- Opposite insertion orders produce the same row type.
- A repeated service key produces one row entry.
- `Contains` returns only the service type assigned to the selected entry.
- `Builder.addFresh` lets layer code add services without tuple-order
  knowledge and requires proof that the qualified key is new.
- `Builder.addExisting` handles an exact duplicate without adding a value.
- A `Type 1` GitHub service runs through the existing `Z` and `Layer` types.
- `service_key configEntry : Config` resolves `Config` and generates the key
  `StableServiceKeys.Config` from its full Lean declaration name.
- `KeyedLayer.singleton` converts one ordinary layer to a one-service keyed
  layer.
- `KeyedLayer.zipFresh` combines disjoint service rows in canonical key order.
- `Environment.CanProvide` projects one required keyed row from a larger row.
- `KeyedLayer.zipFreshMeetJoin` combines different input rows and infers their
  common error channel.
- `KeyedLayer.zipFreshInto` injects both errors into one selected stable error
  channel.
- `Row.missing` computes the inputs that an upstream layer does not produce.
- `KeyedLayer.andThenMeetJoin` and `andThenInto` provide vertical composition
  with inferred or selected error channels.
- `KeyedLayer.andThenKeepFreshMeetJoin` and `andThenKeepFreshInto` keep the
  upstream outputs in the final row.
- `KeyedLayer.widenInput` lets one layer read its row from a larger graph input.
- `KeyedLayer.shareInto` gives repeated branches one explicit memoization
  scope.

The checked examples show that two libraries can use the same local service
name when their declaration namespaces differ. They also show that
`Builder.addFresh` rejects a key that is already present. The `Entry`
constructor is private. A client module can create entries through
`service_key`, but it cannot create a conflicting raw entry.
The command generates a reducible abbreviation. Row normalization and
projection must inspect the key during elaboration.

The entry stores a service type as data. Thus, a row uses one universe level
above its service ceiling. Low-universe services must also be placed at that
common ceiling. A closed key type with a separate service interpretation can
avoid this cost, but a closed key type is not extensible across libraries.

Run the checked runtime example with:

```sh
lake exe stableServiceKeys
```

The runtime checks compose effectful keyed layers through the production
`Layer.zipWith` operation. The checks confirm acquisition order, reverse
release order, cleanup after a later acquisition fails, and cleanup after the
program fails. The final environment row has canonical key order even when the
layers are supplied in the opposite order.

The heterogeneous checks use one layer that requires `Config` and another
layer that requires `Store`. Their result requires the canonical union of both
input rows. Each layer receives only its own projected row. The two layers also
have different error types. One check infers their `Sum` with
`ErrorChannel.Join`. Another check reverses layer order and uses
`ErrorChannel.CanInject` to keep the same selected error sum. A failing second
acquisition keeps its error side and releases the first resource.

The vertical checks first build `Github` from `Config`. A later layer requires
both `Github` and `Store` and builds `Reporter`. Only `Config` and `Store`
remain in the external input row because the first layer supplies `Github`.
The successful run releases `Reporter` before `Github`. If `Reporter`
acquisition fails, the error stays on the right side of the selected sum and
the acquired `Github` resource is released.

The pass-through checks keep both `Github` and `Reporter` in the final output
row. The program uses both services. Pass-through requires `Row.Disjoint`
evidence for the two output rows. Thus, two acquired values cannot silently
claim the same service key. Successful and failed downstream acquisition keep
the same release behavior as ordinary vertical composition.

The shared graph checks use one `Github` layer in both the `Reporter` and
`Metrics` branches. `shareInto` acquires and releases `Github` once. If the
second branch fails during acquisition, the first branch and the shared
upstream resource are both released. This sharing scope must be explicit. A
shallow layer is a function value and does not expose a node identity that the
runtime can use for automatic graph memoization.

The declaration command currently accepts only one named service type. It does
not yet define identities for parameterized service types. Horizontal and
vertical layer composition now handle different keyed inputs and errors, and
pass-through uses a strict duplicate-key policy. Shared graphs work with an
explicit scope. The next design decision is whether explicit sharing is
acceptable. Automatic sharing would require a small reified graph layer or a
macro that assigns stable node identities before it builds shallow layers.

Run the checked sketches from the project root:

```sh
lake env lean docs/intersection-types.lean
```

[scala-intersection-rules]: https://docs.scala-lang.org/scala3/reference/new-types/intersection-types-spec.html
[scala-types]: https://www.scala-lang.org/files/archive/spec/3.4/03-types.html
[scala-union-rules]: https://docs.scala-lang.org/scala3/reference/new-types/union-types-spec.html
[scala-members]: https://docs.scala-lang.org/scala3/reference/new-types/intersection-types.html
[lean-do-release]: https://lean-lang.org/doc/reference/latest/releases/v4.31.0/
[lean-do-ops]: https://github.com/leanprover/lean4/blob/v4.32.2/src/Lean/Elab/Do/Basic.lean
