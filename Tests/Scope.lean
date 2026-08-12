import Tests.Support

/-!
Regression tests for dynamic resource scopes.
-/

namespace ScopeTests

structure Label where
  value : String

structure HighLabel : Type 1 where
  Marker : Type
  value : String

def record
    (events : IO.Ref (List String))
    (event : String) : Z Unit Empty Unit :=
  Z.succeed <| events.modify fun current => current ++ [event]

def resource
    (events : IO.Ref (List String))
    (name : String) : Z Scope Empty String :=
  Z.acquireRelease
    (record events s!"acquire-{name}" *> Z.succeedNow name)
    (fun value => record events s!"release-{value}")

def failingResource
    (events : IO.Ref (List String)) : Z Scope String String :=
  Z.acquireRelease
    (zdo
      record events "acquire-failed"
      Z.fail "acquisition failed")
    (fun value => record events s!"release-{value}")

def label : Z Label Empty String :=
  Z.serviceWith (fun service => service.value)

end ScopeTests

open ScopeTests in
def testScopeReleaseOrder : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let body := zdo
    let _ ← resource events "first"
    let _ ← resource events "second"
    record events "use"
  let program : Z Unit Empty Unit := Z.scoped body
  match ← runProgram "scope-release-order" program with
  | .success () => pure ()
  | _ => failTest "the scoped program failed"
  assertTrue "scope finalizers did not run in reverse order"
    ((← events.get) == [
      "acquire-first", "acquire-second", "use",
      "release-second", "release-first"])

open ScopeTests in
def testScopeReleasesAfterFailureAndDefect : IO Unit := do
  let failureEvents ← IO.mkRef ([] : List String)
  let failureBody : Z Scope String Unit := zdo
    let _ ← resource failureEvents "failure"
    Z.fail "expected"
  let failureProgram : Z Unit String Unit := Z.scoped failureBody
  match ← runProgram "scope-typed-failure" failureProgram with
  | .failure (.fail "expected") => pure ()
  | _ => failTest "the scope did not preserve the typed failure"
  assertTrue "the scope did not release after a typed failure"
    ((← failureEvents.get) == [
      "acquire-failure", "release-failure"])

  let defectEvents ← IO.mkRef ([] : List String)
  let defectBody : Z Scope Empty Unit := zdo
    let _ ← resource defectEvents "defect"
    Z.die (IO.userError "expected defect")
  let defectProgram : Z Unit Empty Unit := Z.scoped defectBody
  match ← runProgram "scope-defect" defectProgram with
  | .failure (.die _) => pure ()
  | _ => failTest "the scope did not preserve the defect"
  assertTrue "the scope did not release after a defect"
    ((← defectEvents.get) == ["acquire-defect", "release-defect"])

open ScopeTests in
def testScopeDoesNotReleaseFailedAcquisition : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let program : Z Unit String String := Z.scoped (failingResource events)
  match ← runProgram "scope-failed-acquisition" program with
  | .failure (.fail "acquisition failed") => pure ()
  | _ => failTest "the scoped acquisition returned the wrong exit"
  assertTrue "a failed acquisition registered its finalizer"
    ((← events.get) == ["acquire-failed"])

open ScopeTests in
def testScopeAcquisitionMasking : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let started ← IO.mkRef false
  let acquire : Z Unit Empty String := do
    Z.succeed (started.set true)
    Z.sleep 100
    record events "acquired"
    pure "slow"
  let scopedResource : Z Scope Empty String :=
    Z.acquireRelease acquire fun _ => record events "released"
  let body : Z Scope Empty Unit := do
    let _ ← scopedResource
    Z.sleep 2000
  let fiber ← Z.unsafeFork (Z.scoped body) "scope-acquisition-masking"
  waitForFlag "scope acquisition" started
  fiber.requestInterrupt
  match ← fiber.await with
  | .failure .interrupt => pure ()
  | _ => failTest "scope acquisition did not preserve interruption"
  fiber.awaitTask
  assertTrue "interruption abandoned an acquired resource"
    ((← events.get) == ["acquired", "released"])

open ScopeTests in
def testScopeRunsAllFinalizersAfterDefect : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let failingRelease : Z Unit Empty Unit :=
    record events "release-second" *>
      (Z.die (IO.userError "release failed") : Z Unit Empty Empty)
  let body := zdo
    let _ ← resource events "first"
    let _ ← Z.acquireRelease
      (record events "acquire-second" *> Z.succeedNow "second")
      (fun _ => failingRelease)
    pure ()
  let program : Z Unit Empty Unit := Z.scoped body
  match ← runProgram "scope-finalizer-defect" program with
  | .failure (.die _) => pure ()
  | _ => failTest "a finalizer defect did not fail scope closure"
  assertTrue "a finalizer defect abandoned an earlier finalizer"
    ((← events.get) == [
      "acquire-first", "acquire-second",
      "release-second", "release-first"])

open ScopeTests in
def testNestedScopesCloseIndependently : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let inner : Z Unit Empty Unit := Z.scoped <| zdo
    let _ ← resource events "inner"
    pure ()
  let outerBody := zdo
    let _ ← resource events "outer"
    inner
    record events "after-inner"
  let program : Z Unit Empty Unit := Z.scoped outerBody
  match ← runProgram "nested-scopes" program with
  | .success () => pure ()
  | _ => failTest "the nested scoped program failed"
  assertTrue "nested scopes did not close at their own boundaries"
    ((← events.get) == [
      "acquire-outer", "acquire-inner", "release-inner",
      "after-inner", "release-outer"])

open ScopeTests in
def testScopePreservesOtherEnvironmentRequirements : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let body := zdo[Empty]
    let name ← label
    let value ← resource events name
    pure value
  let program : Z Label Empty String := Z.scoped body
  match ← runProgram "scope-other-environment" <|
      program.provideEnvironment { value := "service" } with
  | .success "service" => pure ()
  | _ => failTest "Z.scoped did not preserve the other environment service"
  assertTrue "the combined service and scope program did not release"
    ((← events.get) == ["acquire-service", "release-service"])

open ScopeTests in
def testScopeSupportsDirectFinalizers : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let body : Z Scope Empty Unit :=
    Z.addFinalizer (record events "direct-finalizer")
  let program : Z Unit Empty Unit := Z.scoped body
  match ← runProgram "scope-direct-finalizer" program with
  | .success () => pure ()
  | _ => failTest "the direct scope finalizer program failed"
  assertTrue "the direct scope finalizer did not run exactly once"
    ((← events.get) == ["direct-finalizer"])

open ScopeTests in
def testScopePreservesHighUniverseRequirements : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let acquire : Z HighLabel Empty String :=
    Z.serviceWith fun service => service.value
  let release (value : String) : Z HighLabel Empty Unit :=
    Z.serviceWithZ fun _ => record events s!"release-{value}"
  let body := Z.acquireRelease acquire release
  let program : Z HighLabel Empty String := Z.scoped body
  let ready := program.provideEnvironment {
    Marker := Unit
    value := "high"
  }
  match ← runProgram "scope-high-universe-environment" ready with
  | .success "high" => pure ()
  | _ => failTest "Z.scoped lost a high-universe environment service"
  assertTrue "the high-universe resource did not release"
    ((← events.get) == ["release-high"])

def scopeTests : List (String × IO Unit) := [
  ("testScopeReleaseOrder", testScopeReleaseOrder),
  ("testScopeReleasesAfterFailureAndDefect",
    testScopeReleasesAfterFailureAndDefect),
  ("testScopeDoesNotReleaseFailedAcquisition",
    testScopeDoesNotReleaseFailedAcquisition),
  ("testScopeAcquisitionMasking", testScopeAcquisitionMasking),
  ("testScopeRunsAllFinalizersAfterDefect",
    testScopeRunsAllFinalizersAfterDefect),
  ("testNestedScopesCloseIndependently", testNestedScopesCloseIndependently),
  ("testScopePreservesOtherEnvironmentRequirements",
    testScopePreservesOtherEnvironmentRequirements),
  ("testScopeSupportsDirectFinalizers", testScopeSupportsDirectFinalizers),
  ("testScopePreservesHighUniverseRequirements",
    testScopePreservesHighUniverseRequirements)
]
