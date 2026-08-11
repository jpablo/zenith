import Z.ServiceKeys

/-!
Checked laws for the stable service-row representation.

The public declarations are in the `Z` namespace.

`Row.SameKeys` is capability equality: two rows contain the same qualified
service keys. It does not claim that two `Entry` values with the same key have
definitionally equal `Service` fields. This distinction is necessary because
`Entry` stores a type and its equality is not decidable.
-/

namespace Z

namespace Key

/-- A marked value argument cannot equal a named type-constructor key. -/
theorem value_ne_named
    (type payload : Key)
    (owner name : String)
    (arguments : List Key) :
    Key.value type payload ≠ Key.named owner name arguments := by
  intro equality
  have partsEquality := congrArg Key.parts equality
  simp at partsEquality

end Key

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

/-!
Exact canonical-row laws need two additional properties. `Ordered` states
that a row is in strict key order. `Compatible` states that equal keys select
the same complete entry, including its service type.
-/

/-- Strict key order between two entries. -/
def KeyLT (left right : Entry) : Prop :=
  compare left.key right.key = .lt

/-- A row is in strict ascending key order. -/
def Ordered (entries : List Entry) : Prop :=
  entries.Pairwise KeyLT

/-- Equal keys across two rows identify equal complete entries. -/
def Compatible (left right : List Entry) : Prop :=
  ∀ leftEntry ∈ left,
    ∀ rightEntry ∈ right,
      leftEntry.key = rightEntry.key → leftEntry = rightEntry

/-- Every key in one row identifies only one complete entry. -/
def Coherent (entries : List Entry) : Prop :=
  Compatible entries entries

/-- A row with unique keys is coherent. -/
theorem uniqueKeys_coherent
    {entries : List Entry}
    (unique : UniqueKeys entries) :
    Coherent entries := by
  induction entries with
  | nil =>
      intro leftEntry leftMembership
      contradiction
  | cons head tail inductionHypothesis =>
      change Fresh head.key tail ∧ UniqueKeys tail at unique
      intro leftEntry leftMembership rightEntry rightMembership equality
      rw [List.mem_cons] at leftMembership rightMembership
      rcases leftMembership with leftHead | leftTail
      · subst leftEntry
        rcases rightMembership with rightHead | rightTail
        · exact rightHead.symm
        · have different :=
            (fresh_iff_forall head.key tail).mp unique.1
              rightEntry rightTail
          exact (different equality).elim
      · rcases rightMembership with rightHead | rightTail
        · subst rightEntry
          have different :=
            (fresh_iff_forall head.key tail).mp unique.1
              leftEntry leftTail
          exact (different equality.symm).elim
        · exact inductionHypothesis unique.2
            leftEntry leftTail rightEntry rightTail equality

/-- Every strictly ordered row has unique keys. -/
theorem ordered_uniqueKeys
    {entries : List Entry}
    (ordered : Ordered entries) :
    UniqueKeys entries := by
  induction entries with
  | nil => trivial
  | cons head tail inductionHypothesis =>
      rw [Ordered, List.pairwise_cons] at ordered
      have fresh : Fresh head.key tail := by
        rw [fresh_iff_forall]
        intro candidate membership equality
        have order := ordered.1 candidate membership
        change compare head.key candidate.key = .lt at order
        have compareEquality :
            compare head.key candidate.key = .eq :=
          Std.LawfulEqOrd.compare_eq_iff_eq.mpr equality
        rw [compareEquality] at order
        contradiction
      exact ⟨
        fresh,
        inductionHypothesis ordered.2
      ⟩

/-- Every strictly ordered row is coherent. -/
theorem ordered_coherent
    {entries : List Entry}
    (ordered : Ordered entries) :
    Coherent entries :=
  uniqueKeys_coherent (ordered_uniqueKeys ordered)

/-- Every normalized row is coherent. -/
theorem normalize_coherent (entries : List Entry) :
    Coherent (normalize entries) :=
  uniqueKeys_coherent (uniqueKeys_normalize entries)

/-- Every entry produced by insertion comes from its input entry or row. -/
theorem mem_insert_subset
    (candidate entry : Entry)
    (entries : List Entry) :
    candidate ∈ insert entry entries →
      candidate = entry ∨ candidate ∈ entries := by
  induction entries with
  | nil => simp [insert]
  | cons head tail inductionHypothesis =>
      have recurse :
          candidate ∈ head :: insert entry tail →
            candidate = entry ∨ candidate ∈ head :: tail := by
        intro membership
        rw [List.mem_cons] at membership
        rcases membership with headEquality | tailMembership
        · exact Or.inr (List.mem_cons.mpr (Or.inl headEquality))
        · rcases inductionHypothesis tailMembership with
            entryEquality | originalMembership
          · exact Or.inl entryEquality
          · exact Or.inr
              (List.mem_cons.mpr (Or.inr originalMembership))
      simp only [insert]
      split
      · exact fun membership => Or.inr membership
      · split
        · split
          · simp
          · exact recurse
        · exact recurse
        · exact recurse

/-- Insertion preserves strict key order. -/
theorem ordered_insert
    (entry : Entry)
    {entries : List Entry}
    (ordered : Ordered entries) :
    Ordered (insert entry entries) := by
  induction entries with
  | nil => simp [Ordered, insert]
  | cons head tail inductionHypothesis =>
      rw [Ordered, List.pairwise_cons] at ordered
      obtain ⟨headBefore, tailOrdered⟩ := ordered
      simp only [insert]
      split <;> rename_i equality
      · exact List.pairwise_cons.mpr ⟨headBefore, tailOrdered⟩
      · split <;> rename_i order
        · have tailFresh : isFresh entry.key tail = true := by
            change Fresh entry.key tail
            rw [fresh_iff_forall]
            intro candidate membership
            have entryBeforeCandidate :
                compare entry.key candidate.key = .lt :=
              Std.TransCmp.lt_trans order
                (headBefore candidate membership)
            intro keyEquality
            have compareEquality :
                compare entry.key candidate.key = .eq :=
              Std.LawfulEqOrd.compare_eq_iff_eq.mpr keyEquality
            rw [compareEquality] at entryBeforeCandidate
            contradiction
          simp only [tailFresh, ↓reduceIte]
          rw [Ordered, List.pairwise_cons]
          constructor
          · intro candidate membership
            rw [List.mem_cons] at membership
            rcases membership with headEquality | tailMembership
            · simpa [headEquality, KeyLT] using order
            · exact
                Std.TransCmp.lt_trans order
                  (headBefore candidate tailMembership)
          · exact List.pairwise_cons.mpr ⟨headBefore, tailOrdered⟩
        · exact
            (equality
              (Std.LawfulEqOrd.eq_of_compare order)).elim
        · rw [Ordered, List.pairwise_cons]
          constructor
          · intro candidate membership
            rcases mem_insert_subset candidate entry tail membership with
              entryEquality | originalMembership
            · subst candidate
              exact Std.OrientedCmp.lt_of_gt order
            · exact headBefore candidate originalMembership
          · exact inductionHypothesis tailOrdered

/-- Normalization always produces a strictly ordered row. -/
theorem ordered_normalize (entries : List Entry) :
    Ordered (normalize entries) := by
  induction entries with
  | nil => simp [normalize, Ordered]
  | cons head tail inductionHypothesis =>
      change Ordered (insert head (normalize tail))
      exact ordered_insert head inductionHypothesis

/-- An entry that is before every row member inserts at the head. -/
theorem insert_eq_cons_of_before
    (entry : Entry)
    (entries : List Entry)
    (before : ∀ candidate ∈ entries, KeyLT entry candidate) :
    insert entry entries = entry :: entries := by
  cases entries with
  | nil => rfl
  | cons head tail =>
      have order : compare entry.key head.key = .lt :=
        before head (List.mem_cons.mpr (Or.inl rfl))
      have different : entry.key ≠ head.key := by
        intro equality
        have compareEquality :
            compare entry.key head.key = .eq :=
          Std.LawfulEqOrd.compare_eq_iff_eq.mpr equality
        rw [compareEquality] at order
        contradiction
      have fresh : isFresh entry.key tail = true := by
        change Fresh entry.key tail
        rw [fresh_iff_forall]
        intro candidate membership keyEquality
        have candidateOrder :=
          before candidate (List.mem_cons.mpr (Or.inr membership))
        change compare entry.key candidate.key = .lt at candidateOrder
        have compareEquality :
            compare entry.key candidate.key = .eq :=
          Std.LawfulEqOrd.compare_eq_iff_eq.mpr keyEquality
        rw [compareEquality] at candidateOrder
        contradiction
      simp [insert, different, order, fresh]

/-- A strictly ordered row is a fixed point of normalization. -/
theorem normalize_eq_self_of_ordered
    {entries : List Entry}
    (ordered : Ordered entries) :
    normalize entries = entries := by
  induction entries with
  | nil => rfl
  | cons head tail inductionHypothesis =>
      rw [Ordered, List.pairwise_cons] at ordered
      obtain ⟨headBefore, tailOrdered⟩ := ordered
      change insert head (normalize tail) = head :: tail
      rw [inductionHypothesis tailOrdered]
      exact insert_eq_cons_of_before head tail headBefore

/-- Normalization is exactly idempotent. -/
theorem normalize_idempotent_exact (entries : List Entry) :
    normalize (normalize entries) = normalize entries :=
  normalize_eq_self_of_ordered (ordered_normalize entries)

/-- Every normalized entry occurs in the original row. -/
theorem mem_normalize_subset
    (candidate : Entry)
    (entries : List Entry) :
    candidate ∈ normalize entries → candidate ∈ entries := by
  induction entries with
  | nil => simp [normalize]
  | cons head tail inductionHypothesis =>
      change
        candidate ∈ insert head (normalize tail) →
          candidate ∈ head :: tail
      intro membership
      rcases mem_insert_subset candidate head (normalize tail) membership with
        headEquality | tailMembership
      · exact List.mem_cons.mpr (Or.inl headEquality)
      · exact List.mem_cons.mpr
          (Or.inr (inductionHypothesis tailMembership))

/-- A key that is not fresh has a matching row entry. -/
theorem exists_mem_key_of_not_fresh
    {key : Key}
    {entries : List Entry}
    (notFresh : ¬ Fresh key entries) :
    ∃ entry ∈ entries, entry.key = key := by
  classical
  rw [fresh_iff_forall] at notFresh
  obtain ⟨entry, failure⟩ := Classical.not_forall.mp notFresh
  obtain ⟨membership, failure⟩ := Classical.not_imp.mp failure
  exact ⟨entry, membership, (Classical.not_not.mp failure).symm⟩

/-- The head of an ordered row is at most each row member. -/
theorem ordered_head_isLE
    {head candidate : Entry}
    {tail : List Entry}
    (ordered : Ordered (head :: tail))
    (membership : candidate ∈ head :: tail) :
    (compare head.key candidate.key).isLE = true := by
  rw [Ordered, List.pairwise_cons] at ordered
  rw [List.mem_cons] at membership
  rcases membership with headEquality | tailMembership
  · subst candidate
    exact Ordering.isLE_of_eq_eq Std.ReflCmp.compare_self
  · exact Ordering.isLE_of_eq_lt
      (ordered.1 candidate tailMembership)

/-- The head key of an ordered row is fresh in its tail. -/
theorem fresh_tail_of_ordered
    {head : Entry}
    {tail : List Entry}
    (ordered : Ordered (head :: tail)) :
    Fresh head.key tail := by
  rw [Ordered, List.pairwise_cons] at ordered
  rw [fresh_iff_forall]
  intro candidate membership equality
  have order := ordered.1 candidate membership
  change compare head.key candidate.key = .lt at order
  have compareEquality : compare head.key candidate.key = .eq :=
    Std.LawfulEqOrd.compare_eq_iff_eq.mpr equality
  rw [compareEquality] at order
  contradiction

/-- Compatibility is preserved by normalization. -/
theorem compatible_normalize
    {left right : List Entry}
    (compatible : Compatible left right) :
    Compatible (normalize left) (normalize right) := by
  intro leftEntry leftMembership rightEntry rightMembership equality
  exact compatible
    leftEntry (mem_normalize_subset leftEntry left leftMembership)
    rightEntry (mem_normalize_subset rightEntry right rightMembership)
    equality

/-- Ordered compatible rows with the same keys are exactly equal. -/
theorem ordered_sameKeys_eq
    {left right : List Entry}
    (leftOrdered : Ordered left)
    (rightOrdered : Ordered right)
    (sameKeys : SameKeys left right)
    (compatible : Compatible left right) :
    left = right := by
  induction left generalizing right with
  | nil =>
      cases right with
      | nil => rfl
      | cons rightHead rightTail =>
          exfalso
          have rightFresh :
              Fresh rightHead.key (rightHead :: rightTail) :=
            (sameKeys rightHead.key).mp (fresh_empty rightHead.key)
          exact (fresh_cons.mp rightFresh).1 rfl
  | cons leftHead leftTail inductionHypothesis =>
      cases right with
      | nil =>
          exfalso
          have leftFresh :
              Fresh leftHead.key (leftHead :: leftTail) :=
            (sameKeys leftHead.key).mpr (fresh_empty leftHead.key)
          exact (fresh_cons.mp leftFresh).1 rfl
      | cons rightHead rightTail =>
          have rightHeadNotFresh :
              ¬ Fresh rightHead.key (rightHead :: rightTail) := by
            intro freshness
            exact (fresh_cons.mp freshness).1 rfl
          have rightHeadNotFreshLeft :
              ¬ Fresh rightHead.key (leftHead :: leftTail) :=
            fun freshness =>
              rightHeadNotFresh ((sameKeys rightHead.key).mp freshness)
          obtain ⟨leftWitness, leftMembership, leftKey⟩ :=
            exists_mem_key_of_not_fresh rightHeadNotFreshLeft
          have leftLE :
              (compare leftHead.key rightHead.key).isLE = true := by
            simpa [leftKey] using
              ordered_head_isLE leftOrdered leftMembership

          have leftHeadNotFresh :
              ¬ Fresh leftHead.key (leftHead :: leftTail) := by
            intro freshness
            exact (fresh_cons.mp freshness).1 rfl
          have leftHeadNotFreshRight :
              ¬ Fresh leftHead.key (rightHead :: rightTail) :=
            fun freshness =>
              leftHeadNotFresh ((sameKeys leftHead.key).mpr freshness)
          obtain ⟨rightWitness, rightMembership, rightKey⟩ :=
            exists_mem_key_of_not_fresh leftHeadNotFreshRight
          have rightLE :
              (compare rightHead.key leftHead.key).isLE = true := by
            simpa [rightKey] using
              ordered_head_isLE rightOrdered rightMembership

          have headKeyEquality : leftHead.key = rightHead.key :=
            Std.LawfulEqOrd.eq_of_compare
              (Std.OrientedCmp.isLE_antisymm leftLE rightLE)
          have headEquality : leftHead = rightHead :=
            compatible
              leftHead (List.mem_cons.mpr (Or.inl rfl))
              rightHead (List.mem_cons.mpr (Or.inl rfl))
              headKeyEquality
          subst rightHead

          rw [Ordered, List.pairwise_cons] at leftOrdered rightOrdered
          have tailSameKeys : SameKeys leftTail rightTail := by
            intro key
            by_cases equality : key = leftHead.key
            · subst key
              exact iff_of_true
                (fresh_tail_of_ordered
                  (List.pairwise_cons.mpr leftOrdered))
                (fresh_tail_of_ordered
                  (List.pairwise_cons.mpr rightOrdered))
            · have equivalence := sameKeys key
              rw [fresh_cons, fresh_cons] at equivalence
              simpa [equality] using equivalence
          have tailCompatible : Compatible leftTail rightTail := by
            intro leftEntry leftMembership rightEntry rightMembership
              equality
            exact compatible
              leftEntry (List.mem_cons.mpr (Or.inr leftMembership))
              rightEntry (List.mem_cons.mpr (Or.inr rightMembership))
              equality
          have tailEquality := inductionHypothesis
            leftOrdered.2 rightOrdered.2 tailSameKeys tailCompatible
          rw [tailEquality]

/-- Compatible rows with the same keys have one canonical normalization. -/
theorem normalize_eq_of_sameKeys
    {left right : List Entry}
    (sameKeys : SameKeys left right)
    (compatible : Compatible left right) :
    normalize left = normalize right := by
  apply ordered_sameKeys_eq
    (ordered_normalize left)
    (ordered_normalize right)
  · intro key
    simpa using sameKeys key
  · exact compatible_normalize compatible

/-- Permutations of one coherent row normalize to exact equality. -/
theorem normalize_perm_exact
    {left right : List Entry}
    (permutation : left.Perm right)
    (coherent : Coherent left) :
    normalize left = normalize right := by
  apply normalize_eq_of_sameKeys
  · exact fun key => fresh_perm key permutation
  · intro leftEntry leftMembership rightEntry rightMembership equality
    exact coherent
      leftEntry leftMembership
      rightEntry ((permutation.mem_iff).mpr rightMembership)
      equality

/-- Merging into an ordered row preserves strict key order. -/
theorem ordered_merge
    {left : List Entry}
    (right : List Entry)
    (ordered : Ordered left) :
    Ordered (merge left right) := by
  induction right generalizing left with
  | nil => exact ordered
  | cons head tail inductionHypothesis =>
      change Ordered (merge (insert head left) tail)
      exact inductionHypothesis (ordered_insert head ordered)

/-- Every merged entry comes from one of the input rows. -/
theorem mem_merge_subset
    (candidate : Entry)
    (left right : List Entry) :
    candidate ∈ merge left right →
      candidate ∈ left ∨ candidate ∈ right := by
  induction right generalizing left with
  | nil =>
      intro membership
      exact Or.inl membership
  | cons head tail inductionHypothesis =>
      change
        candidate ∈ merge (insert head left) tail →
          candidate ∈ left ∨ candidate ∈ head :: tail
      intro membership
      rcases inductionHypothesis (insert head left) membership with
        insertedMembership | tailMembership
      · rcases mem_insert_subset candidate head left insertedMembership with
          headEquality | leftMembership
        · exact Or.inr (List.mem_cons.mpr (Or.inl headEquality))
        · exact Or.inl leftMembership
      · exact Or.inr (List.mem_cons.mpr (Or.inr tailMembership))

/-- Exact commutativity for compatible canonical rows. -/
theorem merge_comm_exact
    {left right : List Entry}
    (leftOrdered : Ordered left)
    (rightOrdered : Ordered right)
    (coherent : Coherent (left ++ right)) :
    merge left right = merge right left := by
  apply ordered_sameKeys_eq
    (ordered_merge right leftOrdered)
    (ordered_merge left rightOrdered)
    (merge_comm left right)
  intro leftEntry leftMembership rightEntry rightMembership equality
  rcases mem_merge_subset leftEntry left right leftMembership with
    leftFromLeft | leftFromRight
  · rcases mem_merge_subset rightEntry right left rightMembership with
      rightFromRight | rightFromLeft
    · exact coherent
        leftEntry (List.mem_append.mpr (Or.inl leftFromLeft))
        rightEntry (List.mem_append.mpr (Or.inr rightFromRight))
        equality
    · exact coherent
        leftEntry (List.mem_append.mpr (Or.inl leftFromLeft))
        rightEntry (List.mem_append.mpr (Or.inl rightFromLeft))
        equality
  · rcases mem_merge_subset rightEntry right left rightMembership with
      rightFromRight | rightFromLeft
    · exact coherent
        leftEntry (List.mem_append.mpr (Or.inr leftFromRight))
        rightEntry (List.mem_append.mpr (Or.inr rightFromRight))
        equality
    · exact coherent
        leftEntry (List.mem_append.mpr (Or.inr leftFromRight))
        rightEntry (List.mem_append.mpr (Or.inl rightFromLeft))
        equality

/-- Exact idempotence for a coherent canonical row. -/
theorem merge_idempotent_exact
    {entries : List Entry}
    (ordered : Ordered entries)
    (coherent : Coherent entries) :
    merge entries entries = entries := by
  apply ordered_sameKeys_eq
    (ordered_merge entries ordered)
    ordered
    (merge_idempotent entries)
  intro leftEntry leftMembership rightEntry rightMembership equality
  rcases mem_merge_subset leftEntry entries entries leftMembership with
    originalMembership | originalMembership
  · exact coherent leftEntry originalMembership
      rightEntry rightMembership equality
  · exact coherent leftEntry originalMembership
      rightEntry rightMembership equality

/-- The empty row is an exact left identity for a coherent ordered row. -/
theorem merge_empty_left_exact
    {entries : List Entry}
    (ordered : Ordered entries)
    (coherent : Coherent entries) :
    merge [] entries = entries := by
  apply ordered_sameKeys_eq
    (ordered_merge entries (by simp [Ordered]))
    ordered
    (merge_empty_left entries)
  intro leftEntry leftMembership rightEntry rightMembership equality
  rcases mem_merge_subset leftEntry [] entries leftMembership with
    impossible | originalMembership
  · contradiction
  · exact coherent leftEntry originalMembership
      rightEntry rightMembership equality

/-- The empty row is always an exact right identity. -/
theorem merge_empty_right_exact (entries : List Entry) :
    merge entries [] = entries :=
  rfl

/-- Exact associativity for compatible canonical rows. -/
theorem merge_assoc_exact
    {first second third : List Entry}
    (firstOrdered : Ordered first)
    (coherent : Coherent
      (List.append first (List.append second third))) :
    merge (merge first second) third =
      merge first (merge second third) := by
  have leftOrdered : Ordered (merge (merge first second) third) :=
    ordered_merge third (ordered_merge second firstOrdered)
  have rightOrdered : Ordered (merge first (merge second third)) :=
    ordered_merge (merge second third) firstOrdered
  apply ordered_sameKeys_eq leftOrdered rightOrdered
    (merge_assoc first second third)
  intro leftEntry leftMembership rightEntry rightMembership equality

  have leftSource :
      leftEntry ∈ List.append first (List.append second third) := by
    rcases mem_merge_subset leftEntry (merge first second) third
        leftMembership with innerMembership | thirdMembership
    · rcases mem_merge_subset leftEntry first second innerMembership with
        firstMembership | secondMembership
      · exact List.mem_append.mpr (Or.inl firstMembership)
      · exact List.mem_append.mpr
          (Or.inr (List.mem_append.mpr (Or.inl secondMembership)))
    · exact List.mem_append.mpr
        (Or.inr (List.mem_append.mpr (Or.inr thirdMembership)))

  have rightSource :
      rightEntry ∈ List.append first (List.append second third) := by
    rcases mem_merge_subset rightEntry first (merge second third)
        rightMembership with firstMembership | innerMembership
    · exact List.mem_append.mpr (Or.inl firstMembership)
    · rcases mem_merge_subset rightEntry second third innerMembership with
        secondMembership | thirdMembership
      · exact List.mem_append.mpr
          (Or.inr (List.mem_append.mpr (Or.inl secondMembership)))
      · exact List.mem_append.mpr
          (Or.inr (List.mem_append.mpr (Or.inr thirdMembership)))

  exact coherent leftEntry leftSource rightEntry rightSource equality

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

/-- Disjoint coherent rows remain coherent when appended. -/
theorem coherent_append_of_disjoint
    {left right : List Entry}
    (leftCoherent : Coherent left)
    (rightCoherent : Coherent right)
    (disjoint : Disjoint left right) :
    Coherent (left ++ right) := by
  intro leftEntry leftMembership rightEntry rightMembership equality
  rw [List.mem_append] at leftMembership rightMembership
  rcases leftMembership with leftFromLeft | leftFromRight
  · rcases rightMembership with rightFromLeft | rightFromRight
    · exact leftCoherent leftEntry leftFromLeft
        rightEntry rightFromLeft equality
    · have different :=
        (disjoint_iff_forall left right).mp disjoint
          rightEntry rightFromRight leftEntry leftFromLeft
      exact (different equality.symm).elim
  · rcases rightMembership with rightFromLeft | rightFromRight
    · have different :=
        (disjoint_iff_forall left right).mp disjoint
          leftEntry leftFromRight rightEntry rightFromLeft
      exact (different equality).elim
    · exact rightCoherent leftEntry leftFromRight
        rightEntry rightFromRight equality

/-- Disjointness distributes over appending the right row. -/
theorem disjoint_append_right
    (left second third : List Entry) :
    Disjoint left (second ++ third) ↔
      Disjoint left second ∧ Disjoint left third := by
  rw [disjoint_iff_forall, disjoint_iff_forall, disjoint_iff_forall]
  constructor
  · intro combined
    constructor
    · intro entry membership
      exact combined entry (List.mem_append.mpr (Or.inl membership))
    · intro entry membership
      exact combined entry (List.mem_append.mpr (Or.inr membership))
  · rintro ⟨secondDisjoint, thirdDisjoint⟩ entry membership
    rw [List.mem_append] at membership
    rcases membership with secondMembership | thirdMembership
    · exact secondDisjoint entry secondMembership
    · exact thirdDisjoint entry thirdMembership

/-- Exact merge commutativity needs no coherence argument for disjoint rows. -/
theorem merge_comm_exact_of_disjoint
    {left right : List Entry}
    (leftOrdered : Ordered left)
    (rightOrdered : Ordered right)
    (disjoint : Disjoint left right) :
    merge left right = merge right left :=
  merge_comm_exact leftOrdered rightOrdered
    (coherent_append_of_disjoint
      (ordered_coherent leftOrdered)
      (ordered_coherent rightOrdered)
      disjoint)

/-- Exact merge idempotence follows from canonical row order. -/
theorem merge_idempotent_exact_of_ordered
    {entries : List Entry}
    (ordered : Ordered entries) :
    merge entries entries = entries :=
  merge_idempotent_exact ordered (ordered_coherent ordered)

/-- The empty row is an exact left identity for every canonical row. -/
theorem merge_empty_left_exact_of_ordered
    {entries : List Entry}
    (ordered : Ordered entries) :
    merge [] entries = entries :=
  merge_empty_left_exact ordered (ordered_coherent ordered)

/-- Exact associativity needs no coherence argument for pairwise-disjoint rows. -/
theorem merge_assoc_exact_of_pairwise_disjoint
    {first second third : List Entry}
    (firstOrdered : Ordered first)
    (secondOrdered : Ordered second)
    (thirdOrdered : Ordered third)
    (firstSecond : Disjoint first second)
    (firstThird : Disjoint first third)
    (secondThird : Disjoint second third) :
    merge (merge first second) third =
      merge first (merge second third) := by
  have secondThirdCoherent : Coherent (second ++ third) :=
    coherent_append_of_disjoint
      (ordered_coherent secondOrdered)
      (ordered_coherent thirdOrdered)
      secondThird
  have firstRestDisjoint : Disjoint first (second ++ third) :=
    (disjoint_append_right first second third).mpr
      ⟨firstSecond, firstThird⟩
  exact merge_assoc_exact firstOrdered
    (coherent_append_of_disjoint
      (ordered_coherent firstOrdered)
      secondThirdCoherent
      firstRestDisjoint)

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

end Z
