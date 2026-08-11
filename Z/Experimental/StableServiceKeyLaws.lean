import Z.Experimental.StableServiceKeys

/-!
Checked laws for the experimental stable service-row representation.

`Row.SameKeys` is capability equality: two rows contain the same qualified
service keys. It does not claim that two `Entry` values with the same key have
definitionally equal `Service` fields. This distinction is necessary because
`Entry` stores a type and its equality is not decidable.
-/

namespace StableServiceKeys

namespace Row

/-- Two rows contain exactly the same qualified service keys. -/
def SameKeys (left right : List Entry) : Prop :=
  ∀ key, Fresh key left ↔ Fresh key right

theorem sameKeys_refl (entries : List Entry) :
    SameKeys entries entries :=
  fun _ => Iff.rfl

theorem sameKeys_symm
    {left right : List Entry}
    (equivalent : SameKeys left right) :
    SameKeys right left :=
  fun key => (equivalent key).symm

theorem sameKeys_trans
    {first second third : List Entry}
    (left : SameKeys first second)
    (right : SameKeys second third) :
    SameKeys first third :=
  fun key => (left key).trans (right key)

/-- No entry in the tail has the head key. -/
def UniqueKeys : List Entry -> Prop
  | [] => True
  | head :: tail => Fresh head.key tail ∧ UniqueKeys tail

@[simp]
theorem fresh_empty (key : Key) : Fresh key [] := by
  simp [Fresh, isFresh]

@[simp]
theorem fresh_cons
    {key : Key}
    {head : Entry}
    {tail : List Entry} :
    Fresh key (head :: tail) ↔
      key ≠ head.key ∧ Fresh key tail := by
  simp [Fresh, isFresh]

theorem fresh_iff_forall
    (key : Key)
    (entries : List Entry) :
    Fresh key entries ↔
      ∀ entry ∈ entries, key ≠ entry.key := by
  induction entries with
  | nil => simp
  | cons head tail inductionHypothesis =>
      simp [fresh_cons, inductionHypothesis]

/-- Insertion adds exactly one key when that key is not already present. -/
@[simp]
theorem fresh_insert
    (key : Key)
    (entry : Entry)
    (entries : List Entry) :
    Fresh key (insert entry entries) ↔
      key ≠ entry.key ∧ Fresh key entries := by
  induction entries with
  | nil => simp [Fresh, isFresh]
  | cons head tail inductionHypothesis =>
      simp only [insert]
      split <;> rename_i equality
      · simp [equality, fresh_cons]
      · split
        · split <;>
            simp [fresh_cons, inductionHypothesis, and_left_comm]
        · simp [fresh_cons, inductionHypothesis, and_left_comm]
        · simp [fresh_cons, inductionHypothesis, and_left_comm]

/-- Normalization preserves exactly the observable service keys. -/
@[simp]
theorem fresh_normalize
    (key : Key)
    (entries : List Entry) :
    Fresh key (normalize entries) ↔ Fresh key entries := by
  induction entries with
  | nil => simp [normalize]
  | cons head tail inductionHypothesis =>
      change
        Fresh key (insert head (normalize tail)) ↔
          Fresh key (head :: tail)
      simp [fresh_insert, fresh_cons, inductionHypothesis]

/-- Merge contains the union of the keys from both input rows. -/
@[simp]
theorem fresh_merge
    (key : Key)
    (left right : List Entry) :
    Fresh key (merge left right) ↔
      Fresh key left ∧ Fresh key right := by
  induction right generalizing left with
  | nil => simp [merge]
  | cons head tail inductionHypothesis =>
      change
        Fresh key (merge (insert head left) tail) ↔
          Fresh key left ∧ Fresh key (head :: tail)
      rw [inductionHypothesis]
      simp [fresh_insert, fresh_cons, and_assoc, and_left_comm]

/-- Permuting a row does not change which keys are fresh. -/
theorem fresh_perm
    (key : Key)
    {left right : List Entry}
    (permutation : left.Perm right) :
    Fresh key left ↔ Fresh key right := by
  induction permutation with
  | nil => rfl
  | cons entry permutation inductionHypothesis =>
      simp [fresh_cons, inductionHypothesis]
  | swap first second entries =>
      simp only [fresh_cons]
      constructor <;>
        rintro ⟨one, two, tail⟩ <;>
        exact ⟨two, one, tail⟩
  | trans first second firstHypothesis secondHypothesis =>
      exact firstHypothesis.trans secondHypothesis

/-- Normalization is idempotent at the service-key boundary. -/
theorem normalize_idempotent (entries : List Entry) :
    SameKeys (normalize (normalize entries)) (normalize entries) := by
  intro key
  simp

/-- Normalization does not depend on the order of the input entries. -/
theorem normalize_perm
    {left right : List Entry}
    (permutation : left.Perm right) :
    SameKeys (normalize left) (normalize right) := by
  intro key
  rw [fresh_normalize, fresh_normalize]
  exact fresh_perm key permutation

/-- Merge is associative at the service-key boundary. -/
theorem merge_assoc (first second third : List Entry) :
    SameKeys
      (merge (merge first second) third)
      (merge first (merge second third)) := by
  intro key
  simp [fresh_merge, and_assoc]

/-- Merge is commutative at the service-key boundary. -/
theorem merge_comm (left right : List Entry) :
    SameKeys (merge left right) (merge right left) := by
  intro key
  simp [fresh_merge, and_comm]

/-- Merge is idempotent at the service-key boundary. -/
theorem merge_idempotent (entries : List Entry) :
    SameKeys (merge entries entries) entries := by
  intro key
  simp [fresh_merge]

theorem merge_empty_left (entries : List Entry) :
    SameKeys (merge [] entries) entries := by
  intro key
  rw [fresh_merge]
  simp

theorem merge_empty_right (entries : List Entry) :
    SameKeys (merge entries []) entries :=
  sameKeys_refl entries

/-- Insertion preserves the invariant that every qualified key is unique. -/
theorem uniqueKeys_insert
    (entry : Entry)
    {entries : List Entry}
    (unique : UniqueKeys entries) :
    UniqueKeys (insert entry entries) := by
  induction entries with
  | nil => simp [UniqueKeys, Fresh, isFresh]
  | cons head tail inductionHypothesis =>
      simp only [insert]
      split <;> rename_i equality
      · exact unique
      · split
        · split <;> rename_i tailFresh
          · exact
              ⟨fresh_cons.mpr ⟨equality, tailFresh⟩, unique⟩
          · exact
              ⟨(fresh_insert head.key entry tail).2
                  ⟨fun equal => equality equal.symm, unique.1⟩,
                inductionHypothesis unique.2⟩
        · exact
            ⟨(fresh_insert head.key entry tail).2
                ⟨fun equal => equality equal.symm, unique.1⟩,
              inductionHypothesis unique.2⟩
        · exact
            ⟨(fresh_insert head.key entry tail).2
                ⟨fun equal => equality equal.symm, unique.1⟩,
              inductionHypothesis unique.2⟩

/-- A normalized row has no duplicate qualified keys. -/
theorem uniqueKeys_normalize (entries : List Entry) :
    UniqueKeys (normalize entries) := by
  induction entries with
  | nil => simp [normalize, UniqueKeys]
  | cons head tail inductionHypothesis =>
      change UniqueKeys (insert head (normalize tail))
      exact uniqueKeys_insert head inductionHypothesis

/-- Merging into a unique row keeps the result unique. -/
theorem uniqueKeys_merge
    {left : List Entry}
    (right : List Entry)
    (unique : UniqueKeys left) :
    UniqueKeys (merge left right) := by
  induction right generalizing left with
  | nil => exact unique
  | cons head tail inductionHypothesis =>
      change UniqueKeys (merge (insert head left) tail)
      exact inductionHypothesis (uniqueKeys_insert head unique)

@[simp]
theorem disjoint_cons_right
    (left : List Entry)
    (head : Entry)
    (tail : List Entry) :
    Disjoint left (head :: tail) ↔
      Fresh head.key left ∧ Disjoint left tail := by
  simp [Disjoint, canMerge, Fresh]

/-- `Disjoint` means that no key occurs in both rows. -/
theorem disjoint_iff_forall
    (left right : List Entry) :
    Disjoint left right ↔
      ∀ rightEntry ∈ right,
        ∀ leftEntry ∈ left,
          rightEntry.key ≠ leftEntry.key := by
  induction right with
  | nil => simp [Disjoint, canMerge]
  | cons head tail inductionHypothesis =>
      rw [disjoint_cons_right, inductionHypothesis, fresh_iff_forall]
      simp only [List.mem_cons, forall_eq_or_imp]

/-- Disjointness depends only on the observable key sets. -/
theorem disjoint_iff_fresh
    (left right : List Entry) :
    Disjoint left right ↔
      ∀ key, Fresh key left ∨ Fresh key right := by
  classical
  rw [disjoint_iff_forall]
  constructor
  · intro disjoint key
    by_cases leftFresh : Fresh key left
    · exact Or.inl leftFresh
    · right
      rw [fresh_iff_forall]
      intro rightEntry rightMember equal
      rw [fresh_iff_forall] at leftFresh
      obtain ⟨leftEntry, leftFailure⟩ :=
        Classical.not_forall.mp leftFresh
      obtain ⟨leftMember, leftFailure⟩ :=
        Classical.not_imp.mp leftFailure
      have leftEqual : key = leftEntry.key :=
        Classical.not_not.mp leftFailure
      exact disjoint rightEntry rightMember leftEntry leftMember
        (equal.symm.trans leftEqual)
  · intro freshness rightEntry rightMember leftEntry leftMember equal
    rcases freshness rightEntry.key with leftFresh | rightFresh
    · have different :=
        (fresh_iff_forall _ _).mp leftFresh leftEntry leftMember
      exact different equal
    · have different :=
        (fresh_iff_forall _ _).mp rightFresh rightEntry rightMember
      exact different rfl

theorem disjoint_comm (left right : List Entry) :
    Disjoint left right ↔ Disjoint right left := by
  rw [disjoint_iff_forall, disjoint_iff_forall]
  constructor
  · intro disjoint leftEntry leftMember rightEntry rightMember
    exact fun equal =>
      disjoint rightEntry rightMember leftEntry leftMember equal.symm
  · intro disjoint rightEntry rightMember leftEntry leftMember
    exact fun equal =>
      disjoint leftEntry leftMember rightEntry rightMember equal.symm

theorem disjoint_congr
    {left leftPrime right rightPrime : List Entry}
    (leftKeys : SameKeys left leftPrime)
    (rightKeys : SameKeys right rightPrime) :
    Disjoint left right ↔ Disjoint leftPrime rightPrime := by
  rw [disjoint_iff_fresh, disjoint_iff_fresh]
  constructor
  · intro disjoint key
    rcases disjoint key with leftFresh | rightFresh
    · exact Or.inl ((leftKeys key).mp leftFresh)
    · exact Or.inr ((rightKeys key).mp rightFresh)
  · intro disjoint key
    rcases disjoint key with leftFresh | rightFresh
    · exact Or.inl ((leftKeys key).mpr leftFresh)
    · exact Or.inr ((rightKeys key).mpr rightFresh)

/-- Normalization preserves disjointness. -/
theorem disjoint_normalize (left right : List Entry) :
    Disjoint (normalize left) (normalize right) ↔
      Disjoint left right :=
  disjoint_congr
    (fun key => fresh_normalize key left)
    (fun key => fresh_normalize key right)

/-- Every entry selected by `missing` is fresh in the provided row. -/
theorem fresh_of_mem_missing
    {entry : Entry}
    {required provided : List Entry}
    (membership : entry ∈ missing required provided) :
    Fresh entry.key provided := by
  simp only [missing, List.mem_filter] at membership
  exact membership.2

/-- The missing part of a row is disjoint from the provided part. -/
theorem disjoint_missing
    (required provided : List Entry) :
    Disjoint provided (missing required provided) := by
  rw [disjoint_iff_forall]
  intro entry membership
  exact
    (fresh_iff_forall entry.key provided).mp
      (fresh_of_mem_missing membership)

end Row

namespace Environment

/-!
The recursive `CanProvide` instances construct a projection one requested
entry at a time. `Selection` lets the laws name one requested entry by its
structural row position.
-/

namespace Projection

/-- Find the source selection used for one requested position. -/
@[implicit_reducible]
def source
    (self : Projection available entries)
    (position : Selection target entries) :
    Selection target available :=
  match self, position with
  | .cons contains _, .head => contains.selection
  | .cons _ tail, .tail position => tail.source position

/-- Projection preserves the selected source value at every position. -/
theorem valueAt_provide
    (self : Projection available entries)
    (position : Selection target entries)
    (environment : Environment available) :
    position.get (self.provide environment) =
      (self.source position).get environment := by
  induction self with
  | empty => exact nomatch position
  | cons contains tail inductionHypothesis =>
      cases position with
      | head => rfl
      | tail position => exact inductionHypothesis position

end Projection

/-- Every `CanProvide` instance carries a value-preserving projection. -/
theorem valueAt_canProvide
    [provider : CanProvide available entries]
    (position : Selection target entries)
    (environment : Environment available) :
    position.get (provider.provide environment) =
      (provider.projection.source position).get environment :=
  provider.projection.valueAt_provide position environment

theorem provide_empty
    (environment : Environment available) :
    ((inferInstance : CanProvide available []).provide environment) =
      Environment.empty :=
  rfl

theorem provide_cons
    [contains : Contains entry available]
    [tail : CanProvide available entries]
    (environment : Environment available) :
    ((inferInstance : CanProvide available (entry :: entries)).provide
      environment) =
        Environment.cons
          (contains.get environment)
          (tail.provide environment) :=
  rfl

theorem provide_singleton
    [contains : Contains entry available]
    (environment : Environment available) :
    ((inferInstance : CanProvide available [entry]).provide environment) =
      Environment.cons (contains.get environment) Environment.empty :=
  rfl

end Environment

end StableServiceKeys
