import Zenith.Formalization.ServiceKeyLaws
import Zenith.Formalization.TypeAlgebra

/-!
Connection laws between stable service rows and the abstract requirement
algebra. A row is interpreted by its stable service keys; typed projection also
needs coherence so that equal keys identify equal complete entries.
-/

namespace Zenith.Formalization.ServiceRows

open TypeAlgebra

/-- Interpret a service row as an abstract requirement over stable keys. -/
def requirement : List Z.Entry -> Requirement Z.Key
  | [] => .any
  | entry :: entries => Requirement.and (.service entry.key) (requirement entries)

/-- One row provides another when it contains every required stable key. -/
def Provides (available required : List Z.Entry) : Prop :=
  Requirement.Subtype (requirement available) (requirement required)

/-- Key membership in a row is the positive form of `Row.Fresh`. -/
theorem requires_requirement
    (key : Z.Key)
    (entries : List Z.Entry) :
    Requirement.Requires key (requirement entries) ↔ ¬ Z.Row.Fresh key entries := by
  induction entries with
  | nil =>
      simp [requirement, Requirement.Requires, Z.Row.Fresh, Z.Row.isFresh]
  | cons head tail inductionHypothesis =>
      change
        (key = head.key ∨ Requirement.Requires key (requirement tail)) ↔
          ¬ Z.Row.Fresh key (head :: tail)
      rw [Z.Row.fresh_cons]
      constructor
      · intro requiredByRow freshness
        cases requiredByRow with
        | inl keyEquality => exact freshness.1 keyEquality
        | inr requiredByTail =>
            exact (inductionHypothesis.mp requiredByTail) freshness.2
      · intro notFresh
        by_cases keyEquality : key = head.key
        · exact Or.inl keyEquality
        · apply Or.inr
          apply inductionHypothesis.mpr
          intro freshTail
          exact notFresh ⟨keyEquality, freshTail⟩

/-- Equal row keys are exactly equivalent abstract requirements. -/
theorem sameKeys_iff_requirement_equivalent
    (left right : List Z.Entry) :
    Z.Row.SameKeys left right ↔
      OrderEquivalent (requirement left) (requirement right) := by
  constructor
  · intro sameKeys
    constructor
    · intro key requiredByRight
      apply (requires_requirement key left).mpr
      intro freshLeft
      exact (requires_requirement key right).mp requiredByRight
        ((sameKeys key).mp freshLeft)
    · intro key requiredByLeft
      apply (requires_requirement key right).mpr
      intro freshRight
      exact (requires_requirement key left).mp requiredByLeft
        ((sameKeys key).mpr freshRight)
  · rintro ⟨leftBelowRight, rightBelowLeft⟩ key
    constructor
    · intro freshLeft
      by_cases freshRight : Z.Row.Fresh key right
      · exact freshRight
      · exfalso
        exact (requires_requirement key left).mp
          (leftBelowRight key ((requires_requirement key right).mpr freshRight))
          freshLeft
    · intro freshRight
      by_cases freshLeft : Z.Row.Fresh key left
      · exact freshLeft
      · exfalso
        exact (requires_requirement key right).mp
          (rightBelowLeft key ((requires_requirement key left).mpr freshLeft))
          freshRight

/-- Row normalization preserves its abstract requirement exactly. -/
theorem requirement_normalize (entries : List Z.Entry) :
    OrderEquivalent (requirement (Z.Row.normalize entries))
      (requirement entries) :=
  (sameKeys_iff_requirement_equivalent _ _).mp
    (fun key => Z.Row.fresh_normalize key entries)

/-- Row merge implements abstract requirement intersection. -/
theorem requirement_merge (left right : List Z.Entry) :
    OrderEquivalent (requirement (Z.Row.merge left right))
      (Requirement.and (requirement left) (requirement right)) := by
  constructor
  · intro key requiredByEither
    apply (requires_requirement key (Z.Row.merge left right)).mpr
    intro freshMerged
    have freshParts := (Z.Row.fresh_merge key left right).mp freshMerged
    cases requiredByEither with
    | inl requiredByLeft =>
        exact (requires_requirement key left).mp requiredByLeft freshParts.1
    | inr requiredByRight =>
        exact (requires_requirement key right).mp requiredByRight freshParts.2
  · intro key requiredByMerged
    by_cases freshLeft : Z.Row.Fresh key left
    · apply Or.inr
      apply (requires_requirement key right).mpr
      intro freshRight
      exact (requires_requirement key (Z.Row.merge left right)).mp
        requiredByMerged ((Z.Row.fresh_merge key left right).mpr
          ⟨freshLeft, freshRight⟩)
    · apply Or.inl
      exact (requires_requirement key left).mpr freshLeft

private theorem selection_notFresh
    (selection : Z.Environment.Selection target entries) :
    ¬ Z.Row.Fresh target.key entries := by
  induction selection with
  | head =>
      rw [Z.Row.fresh_cons]
      intro freshness
      exact freshness.1 rfl
  | tail selection inductionHypothesis =>
      rw [Z.Row.fresh_cons]
      intro freshness
      exact inductionHypothesis freshness.2

/-- A typed row-membership witness implies that the matching key is present. -/
theorem contains_notFresh
    [contains : Z.Contains target entries] :
    ¬ Z.Row.Fresh target.key entries :=
  selection_notFresh contains.selection

/-- Every typed keyed-environment projection preserves abstract provision. -/
theorem projection_provides
    (projection : Z.Environment.Projection available required) :
    Provides available required :=
  match projection with
  | .empty => by
      intro key requiredByEmpty
      exact False.elim <|
        (requires_requirement key []).mp requiredByEmpty
          (by simp [Z.Row.Fresh, Z.Row.isFresh])
  | .cons (entry := entry) contains tail => by
      intro key requiredByRow
      change key = entry.key ∨ Requirement.Requires key (requirement _) at requiredByRow
      cases requiredByRow with
      | inl keyEquality =>
          subst key
          apply (requires_requirement entry.key _).mpr
          exact selection_notFresh contains.selection
      | inr requiredByTail =>
          exact projection_provides tail key requiredByTail

/-- A successful keyed `CanProvide` instance implies abstract provision. -/
theorem canProvide_provides
    [provider : Z.Environment.CanProvide available required] :
    Provides available required :=
  projection_provides provider.projection

private theorem contains_nonempty_of_mem
    {target : Z.Entry} :
    {entries : List Z.Entry} -> target ∈ entries -> Nonempty (Z.Contains target entries)
  | [], membership => by
      simp at membership
  | head :: tail, membership => by
      rcases List.mem_cons.mp membership with equality | tailMembership
      · subst target
        exact ⟨{ selection := .head }⟩
      · obtain ⟨tailContains⟩ := contains_nonempty_of_mem tailMembership
        exact ⟨{ selection := .tail tailContains.selection }⟩

private theorem compatible_of_coherent_append
    (coherent : Z.Row.Coherent (available ++ required)) :
    Z.Row.Compatible available required := by
  intro availableEntry availableMembership requiredEntry requiredMembership
      keyEquality
  exact coherent
    availableEntry (List.mem_append.mpr (Or.inl availableMembership))
    requiredEntry (List.mem_append.mpr (Or.inr requiredMembership))
    keyEquality

/--
Key provision plus coherence constructs a typed projection, up to nonempty
evidence. The result is deliberately a proposition: `Provides` itself is a
proposition and therefore cannot select a runtime row position.
-/
theorem canProvide_nonempty_of_provides
    (available required : List Z.Entry)
    (provides : Provides available required)
    (coherent : Z.Row.Coherent (available ++ required)) :
    Nonempty (Z.Environment.CanProvide available required) := by
  induction required generalizing available with
  | nil =>
      exact ⟨{ projection := .empty }⟩
  | cons entry tail inductionHypothesis =>
      have tailProvides : Provides available tail :=
        fun key requiredByTail => provides key (Or.inr requiredByTail)
      have extendMembership :
          ∀ {candidate : Z.Entry},
            candidate ∈ available ++ tail -> candidate ∈ available ++ entry :: tail := by
        intro candidate membership
        rw [List.mem_append] at membership ⊢
        cases membership with
        | inl availableMembership => exact Or.inl availableMembership
        | inr tailMembership =>
            exact Or.inr (List.mem_cons.mpr (Or.inr tailMembership))
      have tailCoherent : Z.Row.Coherent (available ++ tail) := by
        intro leftEntry leftMembership rightEntry rightMembership keyEquality
        exact coherent leftEntry (extendMembership leftMembership)
          rightEntry (extendMembership rightMembership) keyEquality
      have entryAvailable : Requirement.Requires entry.key (requirement available) :=
        provides entry.key (Or.inl rfl)
      have entryNotFresh : ¬ Z.Row.Fresh entry.key available :=
        (requires_requirement entry.key available).mp entryAvailable
      obtain ⟨availableEntry, availableMembership, keyEquality⟩ :=
        Z.Row.exists_mem_key_of_not_fresh entryNotFresh
      have entryEquality : availableEntry = entry :=
        compatible_of_coherent_append coherent availableEntry availableMembership
          entry (List.mem_cons.mpr (Or.inl rfl)) keyEquality
      have entryContains : Nonempty (Z.Contains entry available) := by
        subst availableEntry
        exact contains_nonempty_of_mem availableMembership
      obtain ⟨entryContains⟩ := entryContains
      obtain ⟨tailProvider⟩ :=
        inductionHypothesis available tailProvides tailCoherent
      exact ⟨{ projection := .cons entryContains tailProvider.projection }⟩

/--
Choose the projection guaranteed by `canProvide_nonempty_of_provides`.

This definition is noncomputable because a proposition about keys does not
contain a runtime position. Normal Zenith programs should use ordinary
structural `CanProvide` instances instead.
-/
noncomputable def chooseProjection
    (available required : List Z.Entry)
    (provides : Provides available required)
    (coherent : Z.Row.Coherent (available ++ required)) :
    Z.Environment.Projection available required :=
  (Classical.choice
    (canProvide_nonempty_of_provides available required provides coherent)).projection

end Zenith.Formalization.ServiceRows
