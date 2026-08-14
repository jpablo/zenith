import Z
import Lean.Elab.Do.Basic

/-!
Checked formalization and elaboration experiments for `intersection-types.md`.

The abstract `Requirement` and `ErrorType` sections prove the core type
algebra. They are not part of Zenith's public API.
-/

namespace IntersectionTypes

universe uOrder uService uFailure

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

/-- A least upper bound in an arbitrary preorder. -/
structure IsLeastUpperBound
    {T : Type uOrder}
    [LE T]
    (left right result : T) : Prop where
  aboveLeft : left ≤ result
  aboveRight : right ≤ result
  least : ∀ candidate, left ≤ candidate → right ≤ candidate →
    result ≤ candidate

/-- Two least upper bounds are equivalent in the preorder. -/
theorem lubUniqueUpToEquivalence
    {T : Type uOrder}
    [LE T]
    {left right first second : T}
    (firstIsLub : IsLeastUpperBound left right first)
    (secondIsLub : IsLeastUpperBound left right second) :
    OrderEquivalent first second :=
  ⟨firstIsLub.least second secondIsLub.aboveLeft secondIsLub.aboveRight,
    secondIsLub.least first firstIsLub.aboveLeft firstIsLub.aboveRight⟩

/-- Exchanging the operands preserves a least upper bound. -/
theorem lubCommutative
    {T : Type uOrder}
    [LE T]
    {left right result : T}
    (isLub : IsLeastUpperBound left right result) :
    IsLeastUpperBound right left result where
  aboveLeft := isLub.aboveRight
  aboveRight := isLub.aboveLeft
  least candidate aboveRight aboveLeft :=
    isLub.least candidate aboveLeft aboveRight

/-! ## Abstract Zenith core-profile algebra -/

/--
An abstract environment requirement. `and` models the small intersection
fragment that Zenith needs for service requirements.
-/
inductive Requirement (Service : Type uService) : Type uService where
  /-- No service is required. -/
  | any
  /-- One service is required. -/
  | service (value : Service)
  /-- Both component requirements are required. -/
  | and (left right : Requirement Service)

namespace Requirement

/-- A service occurs in an abstract requirement. -/
def Requires (service : Service) : Requirement Service → Prop
  | .any => False
  | .service required => service = required
  | .and left right => Requires service left ∨ Requires service right

/--
Requirement subtyping uses reverse inclusion: an environment with more
available services is a subtype of one with fewer required services.
-/
def Subtype (left right : Requirement Service) : Prop :=
  ∀ service, Requires service right → Requires service left

instance : LE (Requirement Service) where
  le := Subtype

/-- Requirement subtyping is reflexive. -/
theorem subtype_refl (value : Requirement Service) : value ≤ value :=
  fun _ membership => membership

/-- Requirement subtyping is transitive. -/
theorem subtype_trans
    {left middle right : Requirement Service} :
    left ≤ middle → middle ≤ right → left ≤ right :=
  fun leftToMiddle middleToRight service requiredByRight =>
    leftToMiddle service (middleToRight service requiredByRight)

/-- Mutual requirement subtyping is reflexive. -/
theorem equivalent_refl (value : Requirement Service) :
    OrderEquivalent value value :=
  ⟨subtype_refl value, subtype_refl value⟩

/-- Mutual requirement subtyping is symmetric. -/
theorem equivalent_symm
    {left right : Requirement Service} :
    OrderEquivalent left right → OrderEquivalent right left
  | ⟨leftToRight, rightToLeft⟩ => ⟨rightToLeft, leftToRight⟩

/-- Mutual requirement subtyping is transitive. -/
theorem equivalent_trans
    {left middle right : Requirement Service} :
    OrderEquivalent left middle → OrderEquivalent middle right →
      OrderEquivalent left right
  | ⟨leftToMiddle, middleToLeft⟩, ⟨middleToRight, rightToMiddle⟩ =>
    ⟨subtype_trans leftToMiddle middleToRight,
      subtype_trans rightToMiddle middleToLeft⟩

/-- Requirement intersection is a greatest lower bound. -/
theorem and_isGreatestLowerBound
    (left right : Requirement Service) :
    IsGreatestLowerBound left right (.and left right) where
  belowLeft _ requiredByLeft := Or.inl requiredByLeft
  belowRight _ requiredByRight := Or.inr requiredByRight
  greatest _ candidateBelowLeft candidateBelowRight service
      requiredByBoth :=
    match requiredByBoth with
    | .inl requiredByLeft => candidateBelowLeft service requiredByLeft
    | .inr requiredByRight => candidateBelowRight service requiredByRight

/-- Intersection eliminates its left requirement. -/
theorem and_left (left right : Requirement Service) :
    Requirement.and left right ≤ left :=
  (and_isGreatestLowerBound left right).belowLeft

/-- Intersection eliminates its right requirement. -/
theorem and_right (left right : Requirement Service) :
    Requirement.and left right ≤ right :=
  (and_isGreatestLowerBound left right).belowRight

/-- A requirement below both operands is below their intersection. -/
theorem and_intro
    {candidate left right : Requirement Service}
    (candidateBelowLeft : candidate ≤ left)
    (candidateBelowRight : candidate ≤ right) :
    candidate ≤ Requirement.and left right :=
  (and_isGreatestLowerBound left right).greatest candidate
    candidateBelowLeft candidateBelowRight

/-- Requirement intersection is commutative up to mutual subtyping. -/
theorem and_comm (left right : Requirement Service) :
    OrderEquivalent (Requirement.and left right) (Requirement.and right left) :=
  glbUniqueUpToEquivalence
    (and_isGreatestLowerBound left right)
    (glbCommutative (and_isGreatestLowerBound right left))

/-- Requirement intersection is associative up to mutual subtyping. -/
theorem and_assoc
    (left middle right : Requirement Service) :
    OrderEquivalent (Requirement.and (Requirement.and left middle) right)
      (Requirement.and left (Requirement.and middle right)) :=
  ⟨fun _ requiredByRight =>
      match requiredByRight with
      | .inl requiredByLeft => Or.inl (Or.inl requiredByLeft)
      | .inr (.inl requiredByMiddle) => Or.inl (Or.inr requiredByMiddle)
      | .inr (.inr requiredByRight) => Or.inr requiredByRight,
    fun _ requiredByLeft =>
      match requiredByLeft with
      | .inl (.inl requiredByLeft) => Or.inl requiredByLeft
      | .inl (.inr requiredByMiddle) => Or.inr (Or.inl requiredByMiddle)
      | .inr requiredByRight => Or.inr (Or.inr requiredByRight)⟩

/-- Requirement intersection is idempotent up to mutual subtyping. -/
theorem and_idempotent (value : Requirement Service) :
    OrderEquivalent (Requirement.and value value) value :=
  ⟨fun _ requiredByValue => Or.inl requiredByValue,
    fun _ requiredByBoth =>
      match requiredByBoth with
      | .inl requiredByValue => requiredByValue
      | .inr requiredByValue => requiredByValue⟩

/-- `any` is a right identity for requirement intersection. -/
theorem and_any (value : Requirement Service) :
    OrderEquivalent (Requirement.and value .any) value :=
  ⟨fun _ requiredByValue => Or.inl requiredByValue,
    fun _ requiredByBoth =>
      match requiredByBoth with
      | .inl requiredByValue => requiredByValue
      | .inr requiredByAny => False.elim requiredByAny⟩

/-- `any` is a left identity for requirement intersection. -/
theorem any_and (value : Requirement Service) :
    OrderEquivalent (Requirement.and .any value) value :=
  ⟨fun _ requiredByValue => Or.inr requiredByValue,
    fun _ requiredByBoth =>
      match requiredByBoth with
      | .inl requiredByAny => False.elim requiredByAny
      | .inr requiredByValue => requiredByValue⟩

end Requirement

/--
An abstract typed-error channel. `or` models the union fragment that Zenith
uses when it combines independently typed failures.
-/
inductive ErrorType (Failure : Type uFailure) : Type uFailure where
  /-- No typed failure is allowed. -/
  | nothing
  /-- One typed failure is allowed. -/
  | failure (value : Failure)
  /-- Either component failure is allowed. -/
  | or (left right : ErrorType Failure)

namespace ErrorType

/-- A typed failure occurs in an abstract error channel. -/
def Allows (failure : Failure) : ErrorType Failure → Prop
  | .nothing => False
  | .failure allowed => failure = allowed
  | .or left right => Allows failure left ∨ Allows failure right

/--
Error subtyping uses ordinary inclusion: an error channel is a subtype of
another channel when every one of its failures is allowed by the other.
-/
def Subtype (left right : ErrorType Failure) : Prop :=
  ∀ failure, Allows failure left → Allows failure right

instance : LE (ErrorType Failure) where
  le := Subtype

/-- Error subtyping is reflexive. -/
theorem subtype_refl (value : ErrorType Failure) : value ≤ value :=
  fun _ membership => membership

/-- Error subtyping is transitive. -/
theorem subtype_trans
    {left middle right : ErrorType Failure} :
    left ≤ middle → middle ≤ right → left ≤ right :=
  fun leftToMiddle middleToRight failure allowedByLeft =>
    middleToRight failure (leftToMiddle failure allowedByLeft)

/-- Mutual error subtyping is reflexive. -/
theorem equivalent_refl (value : ErrorType Failure) :
    OrderEquivalent value value :=
  ⟨subtype_refl value, subtype_refl value⟩

/-- Mutual error subtyping is symmetric. -/
theorem equivalent_symm
    {left right : ErrorType Failure} :
    OrderEquivalent left right → OrderEquivalent right left
  | ⟨leftToRight, rightToLeft⟩ => ⟨rightToLeft, leftToRight⟩

/-- Mutual error subtyping is transitive. -/
theorem equivalent_trans
    {left middle right : ErrorType Failure} :
    OrderEquivalent left middle → OrderEquivalent middle right →
      OrderEquivalent left right
  | ⟨leftToMiddle, middleToLeft⟩, ⟨middleToRight, rightToMiddle⟩ =>
    ⟨subtype_trans leftToMiddle middleToRight,
      subtype_trans rightToMiddle middleToLeft⟩

/-- Error union is a least upper bound. -/
theorem or_isLeastUpperBound
    (left right : ErrorType Failure) :
    IsLeastUpperBound left right (.or left right) where
  aboveLeft _ allowedByLeft := Or.inl allowedByLeft
  aboveRight _ allowedByRight := Or.inr allowedByRight
  least _ leftBelowCandidate rightBelowCandidate failure
      allowedByEither :=
    match allowedByEither with
    | .inl allowedByLeft => leftBelowCandidate failure allowedByLeft
    | .inr allowedByRight => rightBelowCandidate failure allowedByRight

/-- Error union includes its left error channel. -/
theorem or_left (left right : ErrorType Failure) :
    left ≤ ErrorType.or left right :=
  (or_isLeastUpperBound left right).aboveLeft

/-- Error union includes its right error channel. -/
theorem or_right (left right : ErrorType Failure) :
    right ≤ ErrorType.or left right :=
  (or_isLeastUpperBound left right).aboveRight

/-- A common error supertype is above the union of both error channels. -/
theorem or_elim
    {left right candidate : ErrorType Failure}
    (leftBelowCandidate : left ≤ candidate)
    (rightBelowCandidate : right ≤ candidate) :
    ErrorType.or left right ≤ candidate :=
  (or_isLeastUpperBound left right).least candidate
    leftBelowCandidate rightBelowCandidate

/-- Error union is commutative up to mutual subtyping. -/
theorem or_comm (left right : ErrorType Failure) :
    OrderEquivalent (ErrorType.or left right) (ErrorType.or right left) :=
  lubUniqueUpToEquivalence
    (or_isLeastUpperBound left right)
    (lubCommutative (or_isLeastUpperBound right left))

/-- Error union is associative up to mutual subtyping. -/
theorem or_assoc
    (left middle right : ErrorType Failure) :
    OrderEquivalent (ErrorType.or (ErrorType.or left middle) right)
      (ErrorType.or left (ErrorType.or middle right)) :=
  ⟨fun _ allowedByLeft =>
      match allowedByLeft with
      | .inl (.inl allowedByLeft) => Or.inl allowedByLeft
      | .inl (.inr allowedByMiddle) => Or.inr (Or.inl allowedByMiddle)
      | .inr allowedByRight => Or.inr (Or.inr allowedByRight),
    fun _ allowedByRight =>
      match allowedByRight with
      | .inl allowedByLeft => Or.inl (Or.inl allowedByLeft)
      | .inr (.inl allowedByMiddle) => Or.inl (Or.inr allowedByMiddle)
      | .inr (.inr allowedByRight) => Or.inr allowedByRight⟩

/-- Error union is idempotent up to mutual subtyping. -/
theorem or_idempotent (value : ErrorType Failure) :
    OrderEquivalent (ErrorType.or value value) value :=
  ⟨fun _ allowedByBoth =>
      match allowedByBoth with
      | .inl allowedByValue => allowedByValue
      | .inr allowedByValue => allowedByValue,
    fun _ allowedByValue => Or.inl allowedByValue⟩

/-- `nothing` is a right identity for error union. -/
theorem or_nothing (value : ErrorType Failure) :
    OrderEquivalent (ErrorType.or value .nothing) value :=
  ⟨fun _ allowedByEither =>
      match allowedByEither with
      | .inl allowedByValue => allowedByValue
      | .inr allowedByNothing => False.elim allowedByNothing,
    fun _ allowedByValue => Or.inl allowedByValue⟩

/-- `nothing` is a left identity for error union. -/
theorem nothing_or (value : ErrorType Failure) :
    OrderEquivalent (ErrorType.or .nothing value) value :=
  ⟨fun _ allowedByEither =>
      match allowedByEither with
      | .inl allowedByNothing => False.elim allowedByNothing
      | .inr allowedByValue => allowedByValue,
    fun _ allowedByValue => Or.inr allowedByValue⟩

end ErrorType

/-- A service leaf belongs to an intersection that contains that leaf. -/
example (service : Service) :
    Requirement.Requires service
      (Requirement.and (Requirement.service service) .any) :=
  Or.inl rfl

/-- A failure leaf belongs to a union that contains that leaf. -/
example (failure : Failure) :
    ErrorType.Allows failure
      (ErrorType.or (ErrorType.failure failure) .nothing) :=
  Or.inl rfl

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

/-! ## Current `do` extension points -/

#check Bind.bind
#check Lean.Elab.Do.DoOps
#check Lean.Elab.Do.elabDoWith

end IntersectionTypes
