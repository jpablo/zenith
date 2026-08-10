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
class does not prove the universal least-upper-bound property. It is the
runtime and type-inference foundation for a later normalized error union.

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
because Zenith does not yet infer error joins.

This is elaborator-level normalization, not a reified row type. The structural
order is not a public service-key order. Explicit service keys are still
necessary if the API needs an order that is independent of Lean's expression
representation or compiler version.

## Recommended research sequence

1. Define the Scala fragment that Zenith and other target libraries need.
2. Formalize its subtype preorder, intersection rules, and equivalence laws.
3. Test normalized `zdo[E]` inference on larger real programs.
4. Decide whether cross-version API stability requires explicit service keys.
5. If explicit keys are necessary, define them and prove the row meet laws.
6. Normalize `ErrorChannel.Join` results during `zdo` inference.
7. Define how error inference interacts with `catch` before it becomes the
   default notation behavior.

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
