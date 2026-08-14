import Z.Coercions
import Z.Util

/-- An environment has the runtime representation of its service type. -/
abbrev Environment (R : Type u) := R

/--
`IsComponent A B` means that a permutation of `A` is part of `B`.

Symbolic form: A ∣ B.

For example if `A = A₁ × A₂` then 
```
A ∣ (B₁ × A₁ × B₂ × A₂)
A ∣ (A₁ × B₁ × B₂ × A₂)
A ∣ (A₁ × A₂ × B₁ × B₂)
A ∣ (A₂ × A₁ × B₁ × B₂)
... 
```
all are valid.

`Unit` is assumed to be part of any product.

- `get: B -> A` is the projection. 
-/
class IsComponent (A : Type u) (B: Type v) where
  get: B -> A

 
infixl:65 " ∣ " => IsComponent


namespace IsComponent

  /-- Derive a component projection by mapping an existing component. -/
  def contramap [component: A ∣ B] (f: A -> C): C ∣ B :=
    ⟨get ∘> f⟩


  /--
  `Extract A B R` finds one occurrence of `A` inside `B` and reports the
  components `R` that are still unclaimed once that occurrence is consumed.

  The first occurrence wins, so `Extract Char (Char × String × Char)` leaves
  `String × Char`.
  -/
  class Extract (A : Type u) (B : Type v) (R : outParam (Type w)) where
    extract : B -> A × R

  namespace Extract

    /-- `A` is the head, so the whole tail stays unclaimed. -/
    instance (priority := high) here : Extract A (A × T) T := ⟨id⟩

    /-- `A` sits further down, so the head stays unclaimed. -/
    instance skip [tail : Extract A T R] : Extract A (H × T) (H × R) where
      extract | (h, t) => match tail.extract t with
        | (a, r) => (a, (h, r))

    /-- `A` is the last component, so nothing is left. -/
    instance (priority := low) last : Extract A A Unit := ⟨(·, ())⟩

    /-- `Unit` occupies no position, so everything stays unclaimed. -/
    instance (priority := low) unit : Extract Unit B B := ⟨((), ·)⟩

  end Extract

  /--
  This will detect permutations of `A × B` in `H × T`.

  If we reach this case then we know that `A ≠ H` (as this is covered by `rule4`).

  So we need to things:
  - `A` is at one position of `H × T`
  - `B` is in whatever that position leaves behind

  Resolving `B` against the remainder is what stops both halves of the
  requirement from landing on the same element.
  -/
  instance rule5 [extraction : Extract A (H × T) R] [B ∣ R] : (A × B) ∣ (H × T) where
    get e := match extraction.extract e with
      | (a, r) => (a, get r)

  /-- Same heads and different tails but one tail is a component of the other -/
  instance rule4 [B ∣ T] : (A × B) ∣ (A × T) where 
    get | (a, t) => (a, get t)

  /- Either A is in the head or the tail -/
  instance rule3 [A ∣ T] : A ∣ (H × T) := ⟨fun (_, t) => get t⟩
  instance rule2         : A ∣ (A × T) := ⟨fun (a, _) => a⟩

  /-- A few base cases  -/
  instance rule1 :    L ∣ L := ⟨id⟩
  instance rule0 : Unit ∣ L := ⟨fun _ => ()⟩

end IsComponent

namespace Environment 

  /--
  `CanProvide Available Required` supplies a required environment from the
  complete environment that is available to an effect.
  -/
  class CanProvide (Available : Type u) (Required : Type v) where
    provide : Available -> Required

  namespace CanProvide

    instance (priority := high) [component : Required ∣ Available] :
        CanProvide Available Required :=
      ⟨component.get⟩

    instance (priority := low) [conversion : Available <: Required] :
        CanProvide Available Required :=
      ⟨conversion.coe⟩

  end CanProvide

  /--
  `Meet Left Right Result` combines two environment requirements.

  If one side already provides the other, `Result` is that side. Otherwise,
  `Result` is their product.
  -/
  class Meet.{u, v, w}
      (Left : Type u)
      (Right : Type v)
      (Result : outParam (Type w)) : Type (max u v w) where
    left : Result -> Left
    right : Result -> Right

  namespace Meet

    /-- Build a selected meet from projections of one known result type. -/
    @[reducible] def ofCanProvide
        {Left : Type u}
        {Right : Type v}
        {Result : Type w}
        [leftProvider : CanProvide Result Left]
        [rightProvider : CanProvide Result Right] :
        Meet Left Right Result where
      left := leftProvider.provide
      right := rightProvider.provide

    instance (priority := high)
        [provider : CanProvide Right Left] : Meet Left Right Right where
      left := provider.provide
      right := id

    instance
        [provider : CanProvide Left Right] : Meet Left Right Left where
      left := id
      right := provider.provide

    instance (priority := low) : Meet Left Right (Left × Right) where
      left := Prod.fst
      right := Prod.snd

  end Meet

  /-- The empty ordinary environment. -/
  def EmptyEnv: Type := Environment Unit
  
  /-- Construct the empty ordinary environment. -/
  def empty: EmptyEnv := 
    ()

  /-- Add the first service to an empty environment. -/
  def EmptyEnv.add (self: EmptyEnv) (a: A) : Environment A := a

  /-- Prepend one service to an ordinary product environment. -/
  def add (self: Environment T) (a: A) : Environment (A × T) := 
    ⟨a, self⟩ 

  /-- Prefix `self` with the services in `ea`. -/
  def concat (self: Environment T) (ea: Environment A) : Environment (A × T) := 
    ⟨ea, self⟩ 

  /-- Project one required component from an ordinary environment. -/
  def get (self: Environment T) (A) [component: A ∣ T] : A :=
    component.get self

  /-- Construct an ordinary environment with one service. -/
  def of (a: A) : Environment A := 
    empty.add a

  /-- Transform the runtime representation of an environment. -/
  def map (f: A -> B): Environment A -> Environment B := f

  /--
  `Environment T` is definitionally `T`, so this competes with `HAppend` for
  every value. It stays scoped to keep `++` unambiguous for importers.
  -/
  scoped infixr:67 " ++ " => concat

end Environment 

namespace EnvExamples
  open Environment


  example : get 'c'               Char = 'c' := rfl
  example : get ('c', "a", 1) Char = 'c' := rfl
  example : get ('c', "a", 1)  Nat = 1   := rfl
  example : get ('c', "a", 1) Unit = ()  := rfl

  example : get ('c', "a", 1, "b") String = "a" := rfl

  #check_failure get ('c', "a", 1) Int

  -- Order does not matter
  example : get ('c', "a", 1, "b") (String × Nat) = ("a", 1) := rfl
  example : get ('c', "a", 1, "b") (Nat × String) = (1, "a") := rfl

  -- Note that only the first String "a" is picked up.
  example : get ('c', "a", 1, "b") (Char × String) = ('c', "a") := rfl

  -- Two requirements of the same type come from two different positions.
  example : get ('c', "a", 1, "b") (String × String) = ("a", "b") := rfl
  example : get ('c', "a", "b", 1) (String × String) = ("a", "b") := rfl

  -- So each of them needs a position of its own.
  #check_failure get ('c', "a", 1) (String × String)

  -- Make it a Type 1 on purpose to verify that an Environment can hold types on different universes

  structure Point: Type 1 := (x y: Nat) deriving Repr, BEq

  def p := Point.mk 1 2


  def e0: EmptyEnv                     := empty
  def e1: Environment String           := e0.add "<secret>"
  def e2: Environment (Point × String) := e1.add p


  example : e1.get String = "<secret>" := rfl
  example : e2.get String = "<secret>" := rfl
  example : e2.get Point  = p          := rfl
  
  #check_failure e2.get Int

end EnvExamples
