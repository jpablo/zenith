import Z

/-!
Compile-time regression cases for the product-environment and error-channel
encodings described in `docs/intersection-types.md`.
-/

namespace Tests.IntersectionTypes

variable {A B C α : Type}
variable {P Q S : α → Prop} {value : α}

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

example : Z (Nat × String) Empty (Nat × String) := inferredProduct

def inferredDuplicate :=
  Z.flatMapMeet (Z.environment Nat) fun first =>
    (Z.environment Nat).map fun second =>
      (first, second)

example : Z Nat Empty (Nat × Nat) := inferredDuplicate

def inferredContained :=
  Z.flatMapMeet (Z.environment Nat) fun nat =>
    (Z.environment (String × Nat)).map fun environment =>
      (nat, environment)

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
  let first : Z Unit String Unit := Z.succeed ()
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

end Tests.IntersectionTypes
