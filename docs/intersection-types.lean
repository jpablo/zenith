import Z
import Lean.Elab.Do.Basic

/-!
Checked sketches for `intersection-types.md`.

These definitions are experiments. They are not part of Zenith's public API.
-/

namespace IntersectionTypes

universe uOrder

variable {A B C α : Type}
variable {P Q S : α → Prop} {value : α}

/-! ## The order-theoretic specification -/

def OrderEquivalent {T : Type uOrder} [LE T] (left right : T) : Prop :=
  left ≤ right ∧ right ≤ left

structure IsGreatestLowerBound
    {T : Type uOrder}
    [LE T]
    (left right result : T) : Prop where
  belowLeft : result ≤ left
  belowRight : result ≤ right
  greatest : ∀ candidate, candidate ≤ left → candidate ≤ right →
    candidate ≤ result

theorem glbUniqueUpToEquivalence
    {T : Type uOrder}
    [LE T]
    {left right first second : T}
    (firstIsGlb : IsGreatestLowerBound left right first)
    (secondIsGlb : IsGreatestLowerBound left right second) :
    OrderEquivalent first second :=
  ⟨secondIsGlb.greatest first firstIsGlb.belowLeft firstIsGlb.belowRight,
    firstIsGlb.greatest second secondIsGlb.belowLeft secondIsGlb.belowRight⟩

theorem glbCommutative
    {T : Type uOrder}
    [LE T]
    {left right result : T}
    (isGlb : IsGreatestLowerBound left right result) :
    IsGreatestLowerBound right left result where
  belowLeft := isGlb.belowRight
  belowRight := isGlb.belowLeft
  greatest candidate belowRight belowLeft :=
    isGlb.greatest candidate belowLeft belowRight

/-! ## Products provide projections, but not native intersection types -/

def productLeft (value : A × B) : A := value.1
def productRight (value : A × B) : B := value.2

def productCommute (value : A × B) : B × A :=
  (value.2, value.1)

def productAssociate (value : (A × B) × C) : A × (B × C) :=
  (value.1.1, value.1.2, value.2)

def productDeduplicate (value : A × A) : A := value.1
def productDuplicate (value : A) : A × A := (value, value)

-- Commutativity and idempotence are not definitional equalities for products.
#check_failure fun (value : A × B) => show B × A from value
#check_failure fun (value : A) => show A × A from value

/-! ## Predicate intersection uses one value and satisfies the logical laws -/

def Meets (P Q : α → Prop) (value : α) : Prop :=
  P value ∧ Q value

theorem meetsLeft (value : α) (proof : Meets P Q value) : P value :=
  proof.1

theorem meetsRight (value : α) (proof : Meets P Q value) : Q value :=
  proof.2

theorem meetsIntro
    (value : α)
    (left : P value)
    (right : Q value) : Meets P Q value :=
  ⟨left, right⟩

theorem meetsCommutative : Meets P Q value ↔ Meets Q P value := by
  simp [Meets, and_comm]

theorem meetsAssociative :
    Meets (Meets P Q) S value ↔ Meets P (Meets Q S) value := by
  simp [Meets, and_assoc]

theorem meetsIdempotent : Meets P P value ↔ P value := by
  simp [Meets]

theorem meetsTop : Meets P (fun _ => True) value ↔ P value := by
  simp [Meets]

theorem meetsBottom : Meets P (fun _ => False) value ↔ False := by
  simp [Meets]

theorem meetsDistributive :
    Meets P (fun x => Q x ∨ S x) value ↔
      Meets P Q value ∨ Meets P S value := by
  simp only [Meets]
  exact and_or_left

/-! ## Production environment meet -/

def inferredProduct :=
  Z.flatMapMeet (Z.environment Nat) fun nat =>
    (Z.environment String).map fun string =>
      (nat, string)

#check inferredProduct
example : Z (Nat × String) Empty (Nat × String) := inferredProduct

def inferredDuplicate :=
  Z.flatMapMeet (Z.environment Nat) fun first =>
    (Z.environment Nat).map fun second =>
      (first, second)

#check inferredDuplicate
example : Z Nat Empty (Nat × Nat) := inferredDuplicate

def inferredContained :=
  Z.flatMapMeet (Z.environment Nat) fun nat =>
    (Z.environment (String × Nat)).map fun environment =>
      (nat, environment)

#check inferredContained
example : Z (String × Nat) Empty (Nat × (String × Nat)) :=
  inferredContained

example : Environment.Meet Nat String (Nat × String) := inferInstance
example : Environment.Meet Nat Nat Nat := inferInstance
example : Environment.Meet Unit Nat Nat := inferInstance
example : Environment.Meet Nat (String × Nat) (String × Nat) := inferInstance

example : ErrorChannel.Join Empty Nat Nat := inferInstance
example : ErrorChannel.Join Nat Empty Nat := inferInstance
example : ErrorChannel.Join Nat Nat Nat := inferInstance
example : ErrorChannel.Join String Nat (String ⊕ Nat) := inferInstance
example : ErrorChannel.CanInject (String ⊕ IO.Error) (IO.Error ⊕ String) :=
  inferInstance

def joinedErrors : Z Unit (String ⊕ IO.Error) Nat :=
  let first : Z Unit String Unit := Z.succeedNow ()
  Z.flatMapMeetJoin first fun _ => Z.attempt (pure 42)

abbrev InferredEnvironment
    {R : Type u} {E A : Type} (_ : Z R E A) : Type u := R

def normalizedForward := zdo[Empty]
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

def normalizedReverse := zdo[Empty]
  let string <- Z.environment String
  let nat <- Z.environment Nat
  pure (string, nat)

def normalizedGrouped := zdo[Empty]
  Z.environment (String × Nat)

example : InferredEnvironment normalizedForward =
    InferredEnvironment normalizedReverse := rfl

example : InferredEnvironment normalizedForward =
    InferredEnvironment normalizedGrouped := rfl

/-! ## Current `do` extension points -/

#check Bind.bind
#check Lean.Elab.Do.DoOps
#check Lean.Elab.Do.elabDoWith

end IntersectionTypes
