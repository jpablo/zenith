/-!
Stable keys and normalized service-row data.

This module contains only the data model. It has no dependency on the Zenith
effect runtime or Lean elaborator APIs.
-/

namespace Z

/-- One node in the prefix encoding of a service type. -/
structure KeyPart where
  isValue : Bool
  owner : String
  name : String
  argumentCount : Nat
  deriving BEq, DecidableEq, Repr

namespace KeyPart

/-- Compare key nodes by kind, owner, name, and argument count. -/
@[reducible] def compareKeyPart : KeyPart → KeyPart → Ordering :=
  compareLex
    (compareOn KeyPart.isValue)
    (compareLex
      (compareOn KeyPart.owner)
      (compareLex
        (compareOn KeyPart.name)
        (compareOn KeyPart.argumentCount)))

instance : Ord KeyPart where
  compare := compareKeyPart

instance : Std.TransOrd KeyPart := by
  change Std.TransCmp
    (compareLex
      (compareOn KeyPart.isValue)
      (compareLex
        (compareOn KeyPart.owner)
        (compareLex
          (compareOn KeyPart.name)
          (compareOn KeyPart.argumentCount))))
  infer_instance

instance : Std.LawfulEqOrd KeyPart where
  eq_of_compare {a b} equality := by
    simp only [compare, compareKeyPart, compareLex_eq_eq, compareOn]
      at equality
    obtain ⟨kindEquality, ownerEquality, nameEquality,
      argumentCountEquality⟩ := equality
    change compare a.isValue b.isValue = .eq at kindEquality
    change compare a.owner b.owner = .eq at ownerEquality
    change compare a.name b.name = .eq at nameEquality
    change compare a.argumentCount b.argumentCount = .eq
      at argumentCountEquality
    have kindEquality :=
      Std.LawfulEqOrd.eq_of_compare kindEquality
    have ownerEquality :=
      Std.LawfulEqOrd.eq_of_compare ownerEquality
    have nameEquality :=
      Std.LawfulEqOrd.eq_of_compare nameEquality
    have argumentCountEquality :=
      Std.LawfulEqOrd.eq_of_compare argumentCountEquality
    cases a
    cases b
    simp_all

end KeyPart

/-- A stable prefix encoding of one service type. -/
structure Key where
  parts : List KeyPart
  deriving BEq, DecidableEq, Repr

namespace Key

/-- Compare structural keys by their prefix encodings. -/
@[reducible] def compareKey : Key → Key → Ordering :=
  compareOn Key.parts

instance : Ord Key where
  compare := compareKey

instance : Std.TransOrd Key := by
  change Std.TransCmp (compareOn Key.parts)
  infer_instance

instance : Std.LawfulEqOrd Key where
  eq_of_compare {a b} equality := by
    change compare a.parts b.parts = .eq at equality
    have partsEquality :=
      Std.LawfulEqOrd.eq_of_compare equality
    cases a
    cases b
    simp_all

/-- Build one structural key node from its argument keys. -/
@[reducible] def named
    (owner : String)
    (name : String)
    (arguments : List Key) : Key :=
  ⟨{
      isValue := false
      owner
      name
      argumentCount := arguments.length
    } :: arguments.flatMap (fun argument => argument.parts)⟩

/-- Mark one type key and one value payload as a value argument. -/
@[reducible] def value (type : Key) (payload : Key) : Key :=
  ⟨{
      isValue := true
      owner := ""
      name := ""
      argumentCount := 2
    } :: List.append type.parts payload.parts⟩

end Key

/-- A stable key for a service type that is abstract in generic code. -/
class ServiceKey (Service : Type u) where
  private mk ::
  key : Key

namespace ServiceKey

/-- Construct a stable-key witness for syntax elaborators. -/
@[reducible] def create
    {Service : Type u}
    (key : Key) : ServiceKey Service :=
  ⟨key⟩

end ServiceKey

/-- A stable, injective key function for values used in service types. -/
class ServiceValueKey (Value : Type u) where
  key : Value -> Key

namespace ServiceValueKey

private def textPayload (value : String) : Key :=
  Key.named "" value []

instance : ServiceValueKey Nat where
  key value := textPayload (toString value)

instance : ServiceValueKey Int where
  key value := textPayload (toString value)

instance : ServiceValueKey Bool where
  key
    | false => textPayload "false"
    | true => textPayload "true"

instance : ServiceValueKey Char where
  key value := textPayload (toString value.toNat)

instance : ServiceValueKey String where
  key := textPayload

end ServiceValueKey

/-- A stable qualified key and the service type assigned to it. -/
structure Entry.{u} where
  private mk ::
  key : Key
  Service : Type u

namespace Entry

/-- Construct one keyed-row entry for syntax elaborators. -/
def create (key : Key) (Service : Type u) : Entry :=
  ⟨key, Service⟩

end Entry

namespace Row

/-- Compute whether no row entry has this qualified key. -/
def isFresh (key : Key) : List Entry -> Bool
  | [] => true
  | head :: tail =>
      if key = head.key then false else isFresh key tail

/-- Insert one entry in key order. Keep the existing entry for a duplicate. -/
@[reducible] def insert (entry : Entry) : List Entry -> List Entry
  | [] => [entry]
  | head :: tail =>
      if entry.key = head.key then
        head :: tail
      else
        match compare entry.key head.key with
        | .lt =>
            if isFresh entry.key tail then
              entry :: head :: tail
            else
              head :: insert entry tail
        | .eq => head :: insert entry tail
        | .gt => head :: insert entry tail

/-- Give any list of entries one stable order and remove duplicate keys. -/
def normalize (entries : List Entry) : List Entry :=
  entries.foldr insert []

/-- No entry in the row has this qualified key. -/
def Fresh (key : Key) (entries : List Entry) : Prop :=
  isFresh key entries = true

instance (key : Key) (entries : List Entry) : Decidable (Fresh key entries) := by
  unfold Fresh
  infer_instance

/-- Insert every entry from `right` into the normalized `left` row. -/
@[reducible] def merge (left : List Entry) : List Entry -> List Entry
  | [] => left
  | head :: tail => merge (insert head left) tail

/-- Keep the required entries that the provided row does not contain. -/
@[reducible] def missing
    (required provided : List Entry) : List Entry :=
  required.filter fun entry => isFresh entry.key provided

/-- Concatenate row shapes without key normalization. -/
@[reducible] def concat : List Entry -> List Entry -> List Entry
  | [], right => right
  | head :: tail, right => head :: concat tail right

/-- Compute whether a merge adds only new qualified keys. -/
def canMerge (left : List Entry) : List Entry -> Bool
  | [] => true
  | head :: tail =>
      isFresh head.key left && canMerge left tail

/-- State that two rows have no matching stable service keys. -/
def Disjoint (left right : List Entry) : Prop :=
  canMerge left right = true

instance (left right : List Entry) : Decidable (Disjoint left right) := by
  unfold Disjoint
  infer_instance

end Row

end Z
