import Init.Data.List.Sort.Lemmas

/-!
Abstract requirement and error-channel algebra for `intersection-types.md`.

This optional module proves the core profile without depending on Zenith's
runtime representation or public API.
-/

namespace Zenith.Formalization.TypeAlgebra

universe uOrder uService uFailure

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

/-! ## Canonical finite normal forms -/

namespace Canonical

variable {Atom : Type u}

/-- The non-strict comparison used to sort normal-form atoms. -/
def le [Ord Atom] (left right : Atom) : Bool :=
  (compare left right).isLE

/-- A canonical normal form stores its atoms as an ordered unique list. -/
abbrev NormalForm (Atom : Type u) := List Atom

/-- Remove duplicate atoms while preserving the remaining list order. -/
def deduplicate [DecidableEq Atom] : List Atom -> List Atom
  | [] => []
  | head :: tail =>
      let normalizedTail := deduplicate tail
      if head ∈ normalizedTail then normalizedTail else head :: normalizedTail

/-- Sort and deduplicate a finite collection of atoms. -/
def normalize [DecidableEq Atom] [Ord Atom] (atoms : List Atom) :
    NormalForm Atom :=
  List.mergeSort (deduplicate atoms) le

/-- Combine two normal forms and normalize the result. -/
def merge [DecidableEq Atom] [Ord Atom]
    (left right : NormalForm Atom) : NormalForm Atom :=
  normalize (left ++ right)

/-- The order invariant of a normal form. -/
def Ordered [Ord Atom] (atoms : NormalForm Atom) : Prop :=
  atoms.Pairwise fun left right => le left right = true

/-- The uniqueness invariant of a normal form. -/
def Unique (atoms : NormalForm Atom) : Prop :=
  atoms.Nodup

private theorem mem_deduplicate
    [DecidableEq Atom]
    {atom : Atom}
    (atoms : List Atom) :
    atom ∈ deduplicate atoms ↔ atom ∈ atoms := by
  induction atoms with
  | nil => simp [deduplicate]
  | cons head tail inductionHypothesis =>
      simp only [deduplicate]
      split
      · next present =>
          constructor
          · intro membership
            exact List.mem_cons.mpr (.inr (inductionHypothesis.mp membership))
          · intro membership
            rcases List.mem_cons.mp membership with equality | membership
            · subst atom
              exact present
            · exact inductionHypothesis.mpr membership
      · next _ =>
          constructor
          · intro membership
            rcases List.mem_cons.mp membership with equality | membership
            · exact List.mem_cons.mpr (Or.inl equality)
            · exact List.mem_cons.mpr
                (Or.inr (inductionHypothesis.mp membership))
          · intro membership
            rcases List.mem_cons.mp membership with equality | membership
            · exact List.mem_cons.mpr (Or.inl equality)
            · exact List.mem_cons.mpr
                (Or.inr (inductionHypothesis.mpr membership))

private theorem nodup_deduplicate
    [DecidableEq Atom]
    (atoms : List Atom) :
    (deduplicate atoms).Nodup := by
  induction atoms with
  | nil => simp [deduplicate]
  | cons head tail inductionHypothesis =>
      simp only [deduplicate]
      split
      · exact inductionHypothesis
      · next absent =>
          exact List.nodup_cons.mpr ⟨absent, inductionHypothesis⟩

/-- Normalization preserves atom membership. -/
@[simp]
theorem mem_normalize
    [DecidableEq Atom]
    [Ord Atom]
    {atom : Atom}
    {atoms : List Atom} :
    atom ∈ normalize atoms ↔ atom ∈ atoms := by
  simp [normalize, mem_deduplicate]

/-- Normalization returns a duplicate-free list. -/
theorem unique_normalize
    [DecidableEq Atom]
    [Ord Atom]
    (atoms : List Atom) :
    Unique (normalize atoms) :=
  (List.mergeSort_perm (deduplicate atoms) le).symm.nodup
    (nodup_deduplicate atoms)

private theorem le_transitive
    [Ord Atom]
    [Std.TransOrd Atom]
    (left middle right : Atom) :
    le left middle = true → le middle right = true → le left right = true :=
  Std.TransOrd.isLE_trans

private theorem le_total
    [Ord Atom]
    [Std.TransOrd Atom]
    (left right : Atom) :
    le left right || le right left := by
  cases comparison : compare left right with
  | lt => simp [le, comparison]
  | eq => simp [le, comparison]
  | gt =>
      have reverse : compare right left = .lt :=
        Std.OrientedCmp.lt_of_gt comparison
      simp [le, comparison, reverse]

/-- Normalization returns a list ordered by the selected atom comparison. -/
theorem ordered_normalize
    [DecidableEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    (atoms : List Atom) :
    Ordered (normalize atoms) :=
  List.pairwise_mergeSort le_transitive le_total (deduplicate atoms)

private theorem perm_of_unique_same_members
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    {left right : List Atom}
    (leftUnique : left.Nodup)
    (rightUnique : right.Nodup)
    (sameMembers : ∀ atom, atom ∈ left ↔ atom ∈ right) :
    List.Perm left right := by
  rw [List.perm_iff_count]
  intro atom
  rw [leftUnique.count, rightUnique.count]
  by_cases leftMembership : atom ∈ left
  · have rightMembership := (sameMembers atom).mp leftMembership
    simp [leftMembership, rightMembership]
  · have rightNonMembership : atom ∉ right :=
      fun rightMembership => leftMembership ((sameMembers atom).mpr rightMembership)
    simp [leftMembership, rightNonMembership]

private theorem le_antisymmetric
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    {left right : Atom} :
    le left right = true → le right left = true → left = right := by
  intro leftBelowRight rightBelowLeft
  apply Std.LawfulEqOrd.eq_of_compare
  exact Std.OrientedCmp.isLE_antisymm leftBelowRight rightBelowLeft

/-- Ordered unique lists with the same atoms are exactly equal. -/
theorem eq_of_ordered_unique_same_members
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    {left right : NormalForm Atom}
    (leftOrdered : Ordered left)
    (rightOrdered : Ordered right)
    (leftUnique : Unique left)
    (rightUnique : Unique right)
    (sameMembers : ∀ atom, atom ∈ left ↔ atom ∈ right) :
    left = right :=
  List.Perm.eq_of_pairwise
    (fun _ _ _ _ => le_antisymmetric)
    leftOrdered rightOrdered
    (perm_of_unique_same_members leftUnique rightUnique sameMembers)

/-- Normalization is exactly idempotent. -/
theorem normalize_idempotent
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (atoms : List Atom) :
    normalize (normalize atoms) = normalize atoms :=
  eq_of_ordered_unique_same_members
    (ordered_normalize _)
    (ordered_normalize _)
    (unique_normalize _)
    (unique_normalize _)
    (fun _ => by simp)

/-- Equal atom membership gives exactly equal canonical normal forms. -/
theorem normalize_eq_of_same_members
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    {left right : List Atom}
    (sameMembers : ∀ atom, atom ∈ left ↔ atom ∈ right) :
    normalize left = normalize right :=
  eq_of_ordered_unique_same_members
    (ordered_normalize _)
    (ordered_normalize _)
    (unique_normalize _)
    (unique_normalize _)
    (fun atom => (mem_normalize.trans <| (sameMembers atom).trans
      mem_normalize.symm))

/-- Normal-form merge is exactly associative. -/
theorem merge_assoc
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (first second third : NormalForm Atom) :
    merge (merge first second) third = merge first (merge second third) :=
  normalize_eq_of_same_members fun atom => by
    simp [merge, List.mem_append, or_assoc]

/-- Normal-form merge is exactly commutative. -/
theorem merge_comm
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (left right : NormalForm Atom) :
    merge left right = merge right left :=
  normalize_eq_of_same_members fun atom => by
    simp [List.mem_append, or_comm]

/-- Normal-form merge is exactly idempotent. -/
theorem merge_idempotent
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (atoms : NormalForm Atom) :
    merge atoms atoms = normalize atoms :=
  normalize_eq_of_same_members fun atom => by
    simp [List.mem_append]

/-- The empty normal form is the exact merge identity. -/
theorem merge_empty_right
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (atoms : NormalForm Atom) :
    merge atoms [] = normalize atoms := by
  simp [merge]

/-- The empty normal form is the exact merge identity. -/
theorem merge_empty_left
    [DecidableEq Atom]
    [BEq Atom]
    [LawfulBEq Atom]
    [Ord Atom]
    [Std.TransOrd Atom]
    [Std.LawfulEqOrd Atom]
    (atoms : NormalForm Atom) :
    merge [] atoms = normalize atoms := by
  simp [merge]

end Canonical

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

/-- The service leaves of an abstract requirement. -/
def atoms : Requirement Service -> List Service
  | .any => []
  | .service value => [value]
  | .and left right => atoms left ++ atoms right

/-- Rebuild a requirement from a finite list of service leaves. -/
def ofAtoms : List Service -> Requirement Service
  | [] => .any
  | head :: tail => Requirement.and (.service head) (ofAtoms tail)

/-- A canonical finite representation of a requirement. -/
abbrev NormalForm (Service : Type u) := Canonical.NormalForm Service

/-- Normalize a requirement into its sorted, duplicate-free service list. -/
def normalForm [DecidableEq Service] [Ord Service]
    (requirement : Requirement Service) : NormalForm Service :=
  Canonical.normalize requirement.atoms

/-- Rebuild a semantically equivalent requirement from its canonical form. -/
def normalize [DecidableEq Service] [Ord Service]
    (requirement : Requirement Service) : Requirement Service :=
  ofAtoms requirement.normalForm

/-- Requirement membership is exactly list membership in its leaves. -/
@[simp]
theorem mem_atoms
    {service : Service}
    {requirement : Requirement Service} :
    service ∈ requirement.atoms ↔ Requires service requirement := by
  induction requirement with
  | any => simp [atoms, Requires]
  | service value => simp [atoms, Requires]
  | and left right leftHypothesis rightHypothesis =>
      simp [atoms, Requires, leftHypothesis, rightHypothesis]

/-- Rebuilding a requirement from atoms preserves membership. -/
@[simp]
theorem requires_ofAtoms
    {service : Service}
    {atoms : List Service} :
    Requires service (ofAtoms atoms) ↔ service ∈ atoms := by
  induction atoms with
  | nil => simp [ofAtoms, Requires]
  | cons head tail inductionHypothesis =>
      simp [ofAtoms, Requires, inductionHypothesis]

/-- Normalization preserves the semantic requirement relation. -/
@[simp]
theorem requires_normalize
    [DecidableEq Service]
    [Ord Service]
    {service : Service}
    {requirement : Requirement Service} :
    Requires service (normalize requirement) ↔ Requires service requirement := by
  simp [normalize, normalForm]

/-- Normalization preserves requirement equivalence. -/
theorem normalize_equivalent
    [DecidableEq Service]
    [Ord Service]
    (requirement : Requirement Service) :
    OrderEquivalent (normalize requirement) requirement :=
  ⟨fun _ required => (requires_normalize.mpr required),
    fun _ required => (requires_normalize.mp required)⟩

/-- Equivalent requirements have exactly equal canonical normal forms. -/
theorem normalForm_eq_of_equivalent
    [DecidableEq Service]
    [BEq Service]
    [LawfulBEq Service]
    [Ord Service]
    [Std.TransOrd Service]
    [Std.LawfulEqOrd Service]
    {left right : Requirement Service}
    (equivalent : OrderEquivalent left right) :
    normalForm left = normalForm right := by
  apply Canonical.normalize_eq_of_same_members
  intro service
  simp only [mem_atoms]
  constructor
  · exact equivalent.2 service
  · exact equivalent.1 service

/-- Requirement normalization is exactly idempotent at the normal-form boundary. -/
theorem normalForm_idempotent
    [DecidableEq Service]
    [BEq Service]
    [LawfulBEq Service]
    [Ord Service]
    [Std.TransOrd Service]
    [Std.LawfulEqOrd Service]
    (requirement : Requirement Service) :
    normalForm (normalize requirement) = normalForm requirement :=
  normalForm_eq_of_equivalent (normalize_equivalent requirement)

/-- Normalizing a requirement twice gives an equivalent requirement. -/
theorem normalize_idempotent
    [DecidableEq Service]
    [Ord Service]
    (requirement : Requirement Service) :
    OrderEquivalent (normalize (normalize requirement))
      (normalize requirement) :=
  equivalent_trans
    (normalize_equivalent (normalize requirement))
    (equivalent_refl (normalize requirement))

/-- The normal form of an intersection is the exact normal-form merge. -/
theorem normalForm_and
    [DecidableEq Service]
    [BEq Service]
    [LawfulBEq Service]
    [Ord Service]
    [Std.TransOrd Service]
    [Std.LawfulEqOrd Service]
    (left right : Requirement Service) :
    normalForm (Requirement.and left right) =
      Canonical.merge (normalForm left) (normalForm right) := by
  apply Canonical.normalize_eq_of_same_members
  intro service
  simp [normalForm, atoms, List.mem_append]

/-- The canonical normal form removes the order of intersection operands. -/
theorem normalForm_and_comm
    [DecidableEq Service]
    [BEq Service]
    [LawfulBEq Service]
    [Ord Service]
    [Std.TransOrd Service]
    [Std.LawfulEqOrd Service]
    (left right : Requirement Service) :
    normalForm (Requirement.and left right) =
      normalForm (Requirement.and right left) :=
  normalForm_eq_of_equivalent (and_comm left right)

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

/-- The failure leaves of an abstract error channel. -/
def atoms : ErrorType Failure -> List Failure
  | .nothing => []
  | .failure value => [value]
  | .or left right => atoms left ++ atoms right

/-- Rebuild an error channel from a finite list of failure leaves. -/
def ofAtoms : List Failure -> ErrorType Failure
  | [] => .nothing
  | head :: tail => ErrorType.or (.failure head) (ofAtoms tail)

/-- A canonical finite representation of an error channel. -/
abbrev NormalForm (Failure : Type u) := Canonical.NormalForm Failure

/-- Normalize an error channel into its sorted, duplicate-free failure list. -/
def normalForm [DecidableEq Failure] [Ord Failure]
    (error : ErrorType Failure) : NormalForm Failure :=
  Canonical.normalize error.atoms

/-- Rebuild a semantically equivalent error channel from its canonical form. -/
def normalize [DecidableEq Failure] [Ord Failure]
    (error : ErrorType Failure) : ErrorType Failure :=
  ofAtoms error.normalForm

/-- Error membership is exactly list membership in its leaves. -/
@[simp]
theorem mem_atoms
    {failure : Failure}
    {error : ErrorType Failure} :
    failure ∈ error.atoms ↔ Allows failure error := by
  induction error with
  | nothing => simp [atoms, Allows]
  | failure value => simp [atoms, Allows]
  | or left right leftHypothesis rightHypothesis =>
      simp [atoms, Allows, leftHypothesis, rightHypothesis]

/-- Rebuilding an error channel from atoms preserves membership. -/
@[simp]
theorem allows_ofAtoms
    {failure : Failure}
    {atoms : List Failure} :
    Allows failure (ofAtoms atoms) ↔ failure ∈ atoms := by
  induction atoms with
  | nil => simp [ofAtoms, Allows]
  | cons head tail inductionHypothesis =>
      simp [ofAtoms, Allows, inductionHypothesis]

/-- Normalization preserves the semantic error relation. -/
@[simp]
theorem allows_normalize
    [DecidableEq Failure]
    [Ord Failure]
    {failure : Failure}
    {error : ErrorType Failure} :
    Allows failure (normalize error) ↔ Allows failure error := by
  simp [normalize, normalForm]

/-- Normalization preserves error equivalence. -/
theorem normalize_equivalent
    [DecidableEq Failure]
    [Ord Failure]
    (error : ErrorType Failure) :
    OrderEquivalent (normalize error) error :=
  ⟨fun _ allowed => allows_normalize.mp allowed,
    fun _ allowed => allows_normalize.mpr allowed⟩

/-- Equivalent error channels have exactly equal canonical normal forms. -/
theorem normalForm_eq_of_equivalent
    [DecidableEq Failure]
    [BEq Failure]
    [LawfulBEq Failure]
    [Ord Failure]
    [Std.TransOrd Failure]
    [Std.LawfulEqOrd Failure]
    {left right : ErrorType Failure}
    (equivalent : OrderEquivalent left right) :
    normalForm left = normalForm right := by
  apply Canonical.normalize_eq_of_same_members
  intro failure
  simp only [mem_atoms]
  constructor
  · exact equivalent.1 failure
  · exact equivalent.2 failure

/-- Error normalization is exactly idempotent at the normal-form boundary. -/
theorem normalForm_idempotent
    [DecidableEq Failure]
    [BEq Failure]
    [LawfulBEq Failure]
    [Ord Failure]
    [Std.TransOrd Failure]
    [Std.LawfulEqOrd Failure]
    (error : ErrorType Failure) :
    normalForm (normalize error) = normalForm error :=
  normalForm_eq_of_equivalent (normalize_equivalent error)

/-- Normalizing an error twice gives an equivalent error channel. -/
theorem normalize_idempotent
    [DecidableEq Failure]
    [Ord Failure]
    (error : ErrorType Failure) :
    OrderEquivalent (normalize (normalize error)) (normalize error) :=
  equivalent_trans
    (normalize_equivalent (normalize error))
    (equivalent_refl (normalize error))

/-- The normal form of a union is the exact normal-form merge. -/
theorem normalForm_or
    [DecidableEq Failure]
    [BEq Failure]
    [LawfulBEq Failure]
    [Ord Failure]
    [Std.TransOrd Failure]
    [Std.LawfulEqOrd Failure]
    (left right : ErrorType Failure) :
    normalForm (ErrorType.or left right) =
      Canonical.merge (normalForm left) (normalForm right) := by
  apply Canonical.normalize_eq_of_same_members
  intro failure
  simp [normalForm, atoms, List.mem_append]

/-- The canonical normal form removes the order of union operands. -/
theorem normalForm_or_comm
    [DecidableEq Failure]
    [BEq Failure]
    [LawfulBEq Failure]
    [Ord Failure]
    [Std.TransOrd Failure]
    [Std.LawfulEqOrd Failure]
    (left right : ErrorType Failure) :
    normalForm (ErrorType.or left right) =
      normalForm (ErrorType.or right left) :=
  normalForm_eq_of_equivalent (or_comm left right)

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

/-- Requirement operand order does not change its canonical normal form. -/
example (left right : Requirement Nat) :
    Requirement.normalForm (Requirement.and left right) =
      Requirement.normalForm (Requirement.and right left) :=
  Requirement.normalForm_eq_of_equivalent (Requirement.and_comm left right)

/-- Error operand order does not change its canonical normal form. -/
example (left right : ErrorType Nat) :
    ErrorType.normalForm (ErrorType.or left right) =
      ErrorType.normalForm (ErrorType.or right left) :=
  ErrorType.normalForm_eq_of_equivalent (ErrorType.or_comm left right)

/-- Repeated requirement leaves have one canonical normal form. -/
example (requirement : Requirement Nat) :
    Requirement.normalForm (Requirement.and requirement requirement) =
      Requirement.normalForm requirement :=
  Requirement.normalForm_eq_of_equivalent
    (Requirement.and_idempotent requirement)

/-- Repeated error leaves have one canonical normal form. -/
example (error : ErrorType Nat) :
    ErrorType.normalForm (ErrorType.or error error) = ErrorType.normalForm error :=
  ErrorType.normalForm_eq_of_equivalent (ErrorType.or_idempotent error)

end Zenith.Formalization.TypeAlgebra
