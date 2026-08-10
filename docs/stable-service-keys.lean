import Z

/-!
A separate experiment for stable, normalized service rows.

This file does not change the production `Z` environment. It tests whether an
explicit service key can remove product-order knowledge from layer code.
-/

namespace StableServiceKeys

/-- A stable numeric key and the service type assigned to it. -/
structure Entry.{u} where
  key : Nat
  Service : Type u

namespace Row

/-- Insert one entry in key order. Keep the existing entry for a duplicate. -/
def insert (entry : Entry) : List Entry -> List Entry
  | [] => [entry]
  | head :: tail =>
      match compare entry.key head.key with
      | .lt => entry :: head :: tail
      | .eq => head :: tail
      | .gt => head :: insert entry tail

/-- Give any list of entries one stable order and remove duplicate keys. -/
def normalize (entries : List Entry) : List Entry :=
  entries.foldr insert []

end Row

/-- A typed value for every entry in a service row. -/
inductive Environment.{u} : List Entry.{u} -> Type (u + 1) where
  | empty : Environment []
  | cons (value : entry.Service) (tail : Environment entries) :
      Environment (entry :: entries)

namespace Environment

/-- Insert a service value at the position selected by its stable key. -/
def insert
    (entry : Entry)
    (value : entry.Service) :
    {entries : List Entry} ->
      Environment entries -> Environment (Row.insert entry entries)
  | [], .empty => .cons value .empty
  | head :: tail, .cons headValue tailValues => by
      cases order : compare entry.key head.key with
      | lt =>
          simpa [Row.insert, order] using
            (Environment.cons value
              (Environment.cons headValue tailValues))
      | eq =>
          simpa [Row.insert, order] using
            (Environment.cons headValue tailValues)
      | gt =>
          simpa [Row.insert, order] using
            (Environment.cons headValue
              (insert entry value tailValues))

end Environment

/-- Evidence that one exact entry occurs in a row. -/
class Contains (target : Entry) (entries : List Entry) where
  get : Environment entries -> target.Service

instance (priority := high) : Contains entry (entry :: entries) where
  get
    | .cons value _ => value

instance (priority := low) [Contains target entries] :
    Contains target (entry :: entries) where
  get
    | .cons _ tail => Contains.get tail

/-- Build a typed environment without knowledge of its final storage order. -/
structure Builder.{u} (entries : List Entry.{u}) where
  environment : Environment entries

namespace Builder

def empty : Builder [] :=
  ⟨Environment.empty⟩

def add
    (builder : Builder entries)
    (entry : Entry)
    (value : entry.Service) : Builder (Row.insert entry entries) :=
  ⟨Environment.insert entry value builder.environment⟩

def toLayer (builder : Builder entries) :
    Layer Unit Empty (Environment entries) :=
  Layer.succeed builder.environment

end Builder

/-! The example service keys use explicit ranks. -/

structure Config : Type 1 where
  organization : String

structure Github : Type 1 where
  issueCount : String -> Z Unit Empty Nat

structure Store : Type 1 where
  label : String

def configEntry : Entry.{1} := {
  key := 20
  Service := Config
}

def githubEntry : Entry.{1} := {
  key := 30
  Service := Github
}

def storeEntry : Entry.{1} := {
  key := 10
  Service := Store
}

abbrev Services : List Entry.{1} :=
  [storeEntry, configEntry, githubEntry]

example : Row.normalize [configEntry, githubEntry, storeEntry] =
    Row.normalize [storeEntry, configEntry, githubEntry] := rfl

example : Row.normalize [configEntry, configEntry, githubEntry, storeEntry] =
    Services := rfl

example : Row.normalize [storeEntry, configEntry, githubEntry] =
    Services := rfl

def config : Config := {
  organization := "lean"
}

def github : Github := {
  issueCount := fun _ => Z.succeedNow 2
}

def store : Store := {
  label := "issue"
}

def servicesForward : Builder Services :=
  Builder.empty
    |>.add configEntry config
    |>.add githubEntry github
    |>.add storeEntry store

def servicesReverse : Builder Services :=
  Builder.empty
    |>.add storeEntry store
    |>.add githubEntry github
    |>.add configEntry config

def servicesWithDuplicate : Builder Services :=
  Builder.empty
    |>.add configEntry config
    |>.add githubEntry github
    |>.add configEntry config
    |>.add storeEntry store

/-- Select a high-universe service without returning it as a fiber result. -/
def withServiceZ
    (entry : Entry)
    [Contains entry entries]
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment entries) E A :=
  Z.serviceWithZ fun environment =>
    operation (Contains.get environment)

def program : Z (Environment Services) Empty String := zdo
  let organization <- withServiceZ (entries := Services) configEntry fun config =>
    Z.succeedNow config.organization
  let count <- withServiceZ (entries := Services) githubEntry fun github =>
    github.issueCount organization
  let label <- withServiceZ (entries := Services) storeEntry fun store =>
    Z.succeedNow store.label
  pure s!"{label}:{count}"

def run : IO (Option (Exit Empty String)) :=
  servicesReverse.toLayer.run () program "stable-service-keys"

def demo : IO Unit := do
  match <- run with
  | some (.success "issue:2") =>
      IO.println "Stable service-key prototype passed."
  | some (.success value) =>
      throw (IO.userError s!"Unexpected prototype value: {value}")
  | some (.failure _) =>
      throw (IO.userError "The stable service-key prototype failed.")
  | none =>
      throw (IO.userError "The stable service-key prototype returned no result.")

end StableServiceKeys

def main : IO Unit :=
  StableServiceKeys.demo
