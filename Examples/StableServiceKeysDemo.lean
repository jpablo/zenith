import Z.KeyedLayerMake
import Z.ServiceKeyLaws

/-!
Checks for the public stable, normalized service rows with type-derived keys.

The examples test how an internal qualified service key removes product-order
knowledge from layer code without one central numeric registry. The explicit
`service_key` declarations below test the low-level key and builder APIs. The
application examples use only service types.
-/

namespace StableServiceKeys

open Z

/-! Low-level checks use explicit names for keys from service declarations. -/

structure Config : Type 1 where
  organization : String

structure Github : Type 1 where
  issueCount : String -> Z Unit Empty Nat

structure Store : Type 1 where
  label : String

structure Reporter : Type 1 where
  report : Z Unit Empty String

structure Metrics : Type 1 where
  count : Z Unit Empty Nat

service_key configEntry : Config

service_key githubEntry : Github

service_key storeEntry : Store

service_key reporterEntry : Reporter

service_key metricsEntry : Metrics

namespace OtherLibrary

structure Config : Type 1 where
  organization : String

end OtherLibrary

service_key otherConfigEntry : OtherLibrary.Config

example : configEntry.key =
    Key.named "StableServiceKeys" "Config" [] := rfl

example : otherConfigEntry.key =
    Key.named "StableServiceKeys.OtherLibrary" "Config" [] := rfl

example : ServiceKey Config :=
  serviceKey[Config]

/-! Concrete applications of parameterized service types have structural keys. -/

structure User : Type 1 where
  name : String
  deriving ServiceKey

structure Issue : Type 1 where
  number : Nat
  deriving ServiceKey

structure Repository (A : Type 1) : Type 1 where
  value : A
  deriving ServiceKey

service_key userRepositoryEntry : Repository User

service_key issueRepositoryEntry : Repository Issue

service_key userListRepositoryEntry : Repository (List User)

abbrev UserRepository := Repository User

service_key userRepositoryAliasEntry : UserRepository

example : userRepositoryEntry.key =
    Key.named "StableServiceKeys" "Repository" [
      Key.named "StableServiceKeys" "User" []
    ] := rfl

example : userListRepositoryEntry.key =
    Key.named "StableServiceKeys" "Repository" [
      Key.named "" "List" [
        Key.named "StableServiceKeys" "User" []
      ]
    ] := rfl

example : userRepositoryAliasEntry = userRepositoryEntry := rfl

example : ServiceKey.key (Service := User) =
    Key.named "StableServiceKeys" "User" [] := rfl

example : ServiceKey.key (Service := Repository User) =
    Key.named "StableServiceKeys" "Repository" [
      Key.named "StableServiceKeys" "User" []
    ] := rfl

example : Row.Fresh issueRepositoryEntry.key
    [userRepositoryEntry] := by decide

abbrev ParameterizedEntries : List Entry.{1} :=
  [issueRepositoryEntry, userRepositoryEntry]

example : Row.normalize
      [userRepositoryEntry, issueRepositoryEntry] =
    ParameterizedEntries := rfl

example : ServiceRow[Repository User, Repository Issue] =
    ParameterizedEntries := rfl

example : Services[Repository User, Repository Issue] =
    Environment ParameterizedEntries := rfl

abbrev Services : List Entry.{1} :=
  ServiceRow[Config, Github, Store]

example : Row.Ordered Services := by
  change Row.Ordered
    (Row.normalize [configEntry, githubEntry, storeEntry])
  exact Row.ordered_normalize _

example : Row.Coherent Services := by
  change Row.Coherent
    (Row.normalize [configEntry, githubEntry, storeEntry])
  exact Row.normalize_coherent _

example : Row.normalize [configEntry, githubEntry, storeEntry] =
    Row.normalize [storeEntry, configEntry, githubEntry] := rfl

example : Row.normalize [configEntry, configEntry, githubEntry, storeEntry] =
    Services := rfl

example : Row.normalize [storeEntry, configEntry, githubEntry] =
    Services := rfl

example : Row.Fresh otherConfigEntry.key [configEntry] := by decide

example : ¬ Row.Fresh configEntry.key [configEntry] := by decide

def config : Config := {
  organization := "lean"
}

def github : Github := {
  issueCount := fun _ => Z.succeedNow 2
}

def store : Store := {
  label := "issue"
}

def userRepository : Repository User := {
  value := { name := "Ada" }
}

def issueRepository : Repository Issue := {
  value := { number := 42 }
}

def userRepositoryLayer :=
  KeyedLayer.succeed userRepository

def issueRepositoryLayer :=
  KeyedLayer.succeed issueRepository

def genericServiceLayer
    (Service : Type 1)
    [ServiceKey Service]
    (service : Service) :
    KeyedLayer (Services[]) Empty (ServiceRow[Service]) :=
  KeyedLayer.succeed service

def genericRepositoryProgram
    (A : Type 1)
    [ServiceKey A]
    (inspect : A → Nat) :
    Z (Services[Repository A]) Empty Nat :=
  Z.serviceWith[Repository A] fun repository =>
    inspect repository.value

def getGenericService
    (Service : Type 1)
    [ServiceKey Service]
    (environment : Services[Service]) : Service :=
  Services.get[Service] environment

example : KeyedLayer (Services[]) Empty (ServiceRow[Repository User]) :=
  genericServiceLayer (Repository User) userRepository

example : KeyedLayer (Services[]) Empty (ServiceRow[Repository Issue]) :=
  genericServiceLayer (Repository Issue) issueRepository

example : Z (Services[Repository User]) Empty Nat :=
  genericRepositoryProgram User (fun user => user.name.length)

example (environment : Services[Repository User]) : Repository User :=
  getGenericService (Repository User) environment

def userRepositoryLayerFromLayer :=
  KeyedLayer.fromLayer (Layer.succeed userRepository)

example : KeyedLayer Unit Empty (ServiceRow[Repository User]) :=
  userRepositoryLayerFromLayer

def parameterizedProgram := zdo
  let name <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  let number <- Z.serviceWithZ[Repository Issue]
    (fun repository => Z.succeedNow repository.value.number)
  pure s!"{name}:{number}"

example : Z
    (Services[Repository User, Repository Issue]) Empty String :=
  parameterizedProgram

def parameterizedProgramReverse := zdo
  let number <- Z.serviceWith[Repository Issue]
    (fun repository => repository.value.number)
  let name <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  pure s!"{name}:{number}"

example : Z
    (Services[Repository User, Repository Issue]) Empty String :=
  parameterizedProgramReverse

def parameterizedProgramWithFixedError := zdo[Empty]
  let name <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  let number <- Z.serviceWith[Repository Issue]
    (fun repository => repository.value.number)
  pure s!"{name}:{number}"

example : Z
    (Services[Repository User, Repository Issue]) Empty String :=
  parameterizedProgramWithFixedError

def repeatedParameterizedService := zdo
  let first <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  let second <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  pure s!"{first}:{second}"

example : Z (Services[Repository User]) Empty String :=
  repeatedParameterizedService

def branchedParameterizedProgram (useUser : Bool) := zdo
  if useUser then
    let name <- Z.serviceWith[Repository User]
      (fun repository => repository.value.name)
    pure name
  else
    let number <- Z.serviceWith[Repository Issue]
      (fun repository => repository.value.number)
    pure s!"{number}"

example : Bool → Z
    (Services[Repository User, Repository Issue]) Empty String :=
  branchedParameterizedProgram

def scopedParameterizedProgram := zdo
  try
    Z.serviceWithZ[Repository User] fun repository =>
      (Z.succeedNow repository.value.name : Z Unit String String)
  catch _ =>
    let number <- Z.serviceWith[Repository Issue]
      (fun repository => repository.value.number)
    pure s!"{number}"
  finally
    let _ <- Z.serviceWith[Repository (List User)]
      (fun repository => repository.value.length)
    pure ()

example : Z
    (Services[
      Repository User,
      Repository Issue,
      Repository (List User)
    ])
    Empty
    String :=
  scopedParameterizedProgram

def parameterizedProgramWithErrors := zdo
  let name <- Z.serviceWithZ[Repository User]
    (fun repository =>
      (Z.succeedNow repository.value.name : Z Unit String String))
  let number <- Z.serviceWithZ[Repository Issue]
    (fun repository => Z.attempt (pure repository.value.number))
  pure s!"{name}:{number}"

example : Z
    (Services[Repository User, Repository Issue])
    (IO.Error ⊕ String)
    String :=
  parameterizedProgramWithErrors

def mixedParameterizedProgram := zdo
  let offset <- Z.environment Nat
  let name <- Z.serviceWith[Repository User]
    (fun repository => repository.value.name)
  let number <- Z.serviceWith[Repository Issue]
    (fun repository => repository.value.number)
  pure s!"{name}:{number + offset}"

example : Z
    (Nat × Services[Repository User, Repository Issue])
    Empty
    String :=
  mixedParameterizedProgram

def automaticallyProvidedParameterized :=
  Z.provide parameterizedProgram [
    genericServiceLayer (Repository User) userRepository,
    genericServiceLayer (Repository Issue) issueRepository
  ]

example : Z
    (Services[]) Empty String :=
  automaticallyProvidedParameterized

def checkParameterizedServiceKeys : IO Unit := do
  let effect := automaticallyProvidedParameterized.provideEnvironment
    Services.empty
  match ← Z.unsafeRunSync effect "stable-parameterized-service-keys" with
  | some (.success "Ada:42") => pure ()
  | _ => throw (IO.userError
      "Parameterized service-key provision failed.")

def servicesForward : Builder Services :=
  Builder.empty
    |>.addFresh configEntry config (by decide)
    |>.addFresh githubEntry github (by decide)
    |>.addFresh storeEntry store (by decide)

def servicesReverse : Builder Services :=
  Builder.empty
    |>.addFresh storeEntry store (by decide)
    |>.addFresh githubEntry github (by decide)
    |>.addFresh configEntry config (by decide)

def servicesBeforeDuplicate : Builder [configEntry, githubEntry] :=
  Builder.empty
    |>.addFresh configEntry config (by decide)
    |>.addFresh githubEntry github (by decide)

def servicesWithDuplicate : Builder Services :=
  servicesBeforeDuplicate
    |>.addExisting configEntry config
    |>.addFresh storeEntry store (by decide)

def program := zdo
  let organization <- Z.serviceWith[Config]
    (fun config => config.organization)
  let count <- Z.serviceWithZ[Github]
    (fun github => github.issueCount organization)
  let label <- Z.serviceWith[Store]
    (fun store => store.label)
  pure s!"{label}:{count}"

example : Z (Services[Config, Github, Store]) Empty String :=
  program

def run : IO (Option (Exit Empty String)) :=
  servicesReverse.toLayer.run () program "stable-service-keys"

def recordLayerEvent
    (events : IO.Ref (List String))
    (event : String) : HEIO (Cause E) Unit :=
  HEIO.bind
    (HEIO.liftIO.{0} Cause.die <|
      events.modify fun current => current ++ [event])
    fun _ => HEIO.pure ()

def trackedServiceLayer
    (events : IO.Ref (List String))
    (name : String)
    (value : A) : Layer Unit String A :=
  Layer.acquireRelease
    (fun _ =>
      HEIO.bind (recordLayerEvent events s!"acquire-{name}") fun _ =>
        HEIO.pure value)
    (fun _ _ => recordLayerEvent events s!"release-{name}")

def failingServiceLayer
    (events : IO.Ref (List String))
    (name : String) : Layer Unit String A :=
  Layer.fromHEIO fun _ =>
    HEIO.bind (recordLayerEvent events s!"acquire-{name}") fun _ =>
      HEIO.throw (.fail s!"{name} acquisition failed")

def keyedServicesReverse
    (events : IO.Ref (List String)) :
    KeyedLayer Unit String Services :=
  let storeLayer := KeyedLayer.fromLayer
    (trackedServiceLayer events "store" store)
  let githubLayer := KeyedLayer.fromLayer
    (trackedServiceLayer events "github" github)
  let configLayer := KeyedLayer.fromLayer
    (trackedServiceLayer events "config" config)
  storeLayer.zipFresh githubLayer (by decide)
    |>.zipFresh configLayer (by decide)

abbrev ConfigGithubServices : List Entry.{1} :=
  ServiceRow[Config, Github]

def failingKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer Unit String ConfigGithubServices :=
  let configLayer := KeyedLayer.fromLayer
    (trackedServiceLayer events "config" config)
  let githubLayer : KeyedLayer Unit String (ServiceRow[Github]) :=
    KeyedLayer.fromLayer (failingServiceLayer events "github")
  configLayer.zipFresh githubLayer (by decide)

def assertEvents
    (name : String)
    (events : IO.Ref (List String))
    (expected : List String) : IO Unit := do
  let actual <- events.get
  unless actual == expected do
    throw (IO.userError s!"{name}: unexpected layer events {actual}")

def assertEventsOneOf
    (name : String)
    (events : IO.Ref (List String))
    (expected : List (List String)) : IO Unit := do
  let actual ← events.get
  unless expected.contains actual do
    throw (IO.userError s!"{name}: unexpected layer events {actual}")

def assertParallelSiblingEvents
    (name : String)
    (events : IO.Ref (List String))
    (left right : String)
    (releaseEvents : List String) : IO Unit := do
  let initialEvents : List String := ["acquire-github-from-config"]
  assertEventsOneOf name events [
    initialEvents.append ([left, right].append releaseEvents),
    initialEvents.append ([right, left].append releaseEvents)
  ]

def assertParallelFailureEvents
    (name : String)
    (events : IO.Ref (List String))
    (failureEvent siblingEvent siblingReleaseEvent : String) : IO Unit := do
  let initialEvents : List String := ["acquire-github-from-config"]
  let finalEvents : List String := ["release-github-from-config"]
  assertEventsOneOf name events [
    initialEvents.append ([failureEvent].append finalEvents),
    initialEvents.append ([siblingEvent, failureEvent, siblingReleaseEvent].append finalEvents),
    initialEvents.append ([failureEvent, siblingEvent, siblingReleaseEvent].append finalEvents)
  ]

partial def waitForSignal
    (name : String)
    (signal : IO.Ref Bool)
    (attempts : Nat := 1000) : IO Unit := do
  if ← signal.get then
    pure ()
  else if attempts == 0 then
    throw (IO.userError s!"timed out while waiting for {name}")
  else
    IO.sleep 1
    waitForSignal name signal (attempts - 1)

def checkKeyedLayerSuccess : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z (Environment Services) String String := program
  match <- (keyedServicesReverse events).toLayer.run () effect
      "stable-keyed-layer-success" with
  | some (.success "issue:2") => pure ()
  | _ => throw (IO.userError "The keyed service layer did not run.")
  assertEvents "keyed success" events [
    "acquire-store",
    "acquire-github",
    "acquire-config",
    "release-config",
    "release-github",
    "release-store"
  ]

def checkKeyedLayerAcquisitionFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z (Environment ConfigGithubServices) String Unit :=
    Z.serviceWith fun _ => ()
  match <- (failingKeyedServices events).toLayer.run () effect
      "stable-keyed-layer-acquisition-failure" with
  | some (.failure (.fail "github acquisition failed")) => pure ()
  | _ => throw (IO.userError "The keyed layer failure was not preserved.")
  assertEvents "keyed acquisition failure" events [
    "acquire-config",
    "acquire-github",
    "release-config"
  ]

def checkKeyedLayerProgramFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z (Environment Services) String Unit :=
    Z.fail "program failed"
  match <- (keyedServicesReverse events).toLayer.run () effect
      "stable-keyed-layer-program-failure" with
  | some (.failure (.fail "program failed")) => pure ()
  | _ => throw (IO.userError "The keyed program failure was not preserved.")
  assertEvents "keyed program failure" events [
    "acquire-store",
    "acquire-github",
    "acquire-config",
    "release-config",
    "release-github",
    "release-store"
  ]

/-! Keyed layers with different input rows and error types. -/

inductive GithubBuildError where
  | unavailable
  deriving BEq, Repr

inductive OtherConfigBuildError where
  | unavailable
  deriving BEq, Repr

abbrev HeterogeneousInputs : List Entry.{1} :=
  ServiceRow[Config, Store]

abbrev HeterogeneousOutputs : List Entry.{1} :=
  ServiceRow[Github, OtherLibrary.Config]

def heterogeneousInputs : Builder HeterogeneousInputs :=
  Builder.empty
    |>.addFresh storeEntry store (by decide)
    |>.addFresh configEntry config (by decide)

def githubFromConfigLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config])
      GithubBuildError
      (ServiceRow[Github]) :=
  KeyedLayer.fromLayer (Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-github-from-config") fun _ =>
            let config := Services.get[Config] environment
            HEIO.pure {
              issueCount := fun organization =>
                Z.succeedNow <|
                  if organization == config.organization then 2 else 0
            })
      (fun _ _ =>
        recordLayerEvent events "release-github-from-config"))

def otherConfigFromStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Store])
      OtherConfigBuildError
      (ServiceRow[OtherLibrary.Config]) :=
  KeyedLayer.fromLayer (Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-other-config-from-store") fun _ =>
            let store := Services.get[Store] environment
            HEIO.pure { organization := store.label })
      (fun _ _ =>
        recordLayerEvent events "release-other-config-from-store"))

def failingOtherConfigFromStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Store])
      OtherConfigBuildError
      (ServiceRow[OtherLibrary.Config]) :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun _ =>
      HEIO.bind
        (recordLayerEvent events "acquire-other-config-from-store") fun _ =>
          HEIO.throw (.fail .unavailable))

def heterogeneousKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment HeterogeneousInputs)
      (GithubBuildError ⊕ OtherConfigBuildError)
      HeterogeneousOutputs :=
  (githubFromConfigLayer events).zipFreshMeetJoin
    (otherConfigFromStoreLayer events)
    (by rfl)
    (by decide)

/-- Select one stable error sum even when layer order changes. -/
def heterogeneousKeyedServicesReverse
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment HeterogeneousInputs)
      (GithubBuildError ⊕ OtherConfigBuildError)
      HeterogeneousOutputs :=
  (otherConfigFromStoreLayer events).zipFreshInto
    (githubFromConfigLayer events)
    (by rfl)
    (by decide)

def failingHeterogeneousKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment HeterogeneousInputs)
      (GithubBuildError ⊕ OtherConfigBuildError)
      HeterogeneousOutputs :=
  (githubFromConfigLayer events).zipFreshMeetJoin
    (failingOtherConfigFromStoreLayer events)
    (by rfl)
    (by decide)

def heterogeneousProgram := zdo
  let count <- Z.serviceWithZ[Github]
    (fun github => github.issueCount "lean")
  let organization <- Z.serviceWith[OtherLibrary.Config]
    (fun otherConfig => otherConfig.organization)
  pure s!"{organization}:{count}"

example : Z (Services[Github, OtherLibrary.Config]) Empty String :=
  heterogeneousProgram

def checkHeterogeneousKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Github, OtherLibrary.Config])
      (GithubBuildError ⊕ OtherConfigBuildError)
      String := heterogeneousProgram
  match <- (heterogeneousKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect "stable-keyed-layer-heterogeneous" with
  | some (.success "issue:2") => pure ()
  | _ => throw (IO.userError "The heterogeneous keyed layers did not run.")
  assertEvents "heterogeneous keyed layers" events [
    "acquire-github-from-config",
    "acquire-other-config-from-store",
    "release-other-config-from-store",
    "release-github-from-config"
  ]

def checkHeterogeneousKeyedLayersReverse : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Github, OtherLibrary.Config])
      (GithubBuildError ⊕ OtherConfigBuildError)
      String := heterogeneousProgram
  match <- (heterogeneousKeyedServicesReverse events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-heterogeneous-reverse" with
  | some (.success "issue:2") => pure ()
  | _ => throw (IO.userError "The reversed heterogeneous layers did not run.")
  assertEvents "reversed heterogeneous keyed layers" events [
    "acquire-other-config-from-store",
    "acquire-github-from-config",
    "release-github-from-config",
    "release-other-config-from-store"
  ]

def checkHeterogeneousKeyedLayerFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Github, OtherLibrary.Config])
      (GithubBuildError ⊕ OtherConfigBuildError)
      Unit := Z.serviceWith fun _ => ()
  match <- (failingHeterogeneousKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-heterogeneous-failure" with
  | some (.failure (.fail (.inr .unavailable))) => pure ()
  | _ => throw (IO.userError "The joined layer error was not preserved.")
  assertEvents "heterogeneous keyed layer failure" events [
    "acquire-github-from-config",
    "acquire-other-config-from-store",
    "release-github-from-config"
  ]

/-! Vertical composition supplies generated services to a later layer. -/

inductive ReporterBuildError where
  | unavailable
  deriving BEq, Repr

abbrev ReporterInputs : List Entry.{1} :=
  ServiceRow[Github, Store]

abbrev ReporterOutputs : List Entry.{1} :=
  ServiceRow[Reporter]

def reporterFromGithubAndStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment ReporterInputs)
      ReporterBuildError
      ReporterOutputs :=
  KeyedLayer.fromLayer (Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-reporter") fun _ =>
            let github := Services.get[Github] environment
            let store := Services.get[Store] environment
            HEIO.pure {
              report := github.issueCount "lean" |>.map fun count =>
                s!"{store.label}:{count}"
            })
      (fun _ _ => recordLayerEvent events "release-reporter"))

def failingReporterFromGithubAndStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment ReporterInputs)
      ReporterBuildError
      ReporterOutputs :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun _ =>
      HEIO.bind (recordLayerEvent events "acquire-reporter") fun _ =>
        HEIO.throw (.fail .unavailable))

def verticalKeyedServices
    (events : IO.Ref (List String)) :
  KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      ReporterOutputs :=
  (githubFromConfigLayer events).andThenMeetJoin
    (reporterFromGithubAndStoreLayer events)
    (by rfl)

def failingVerticalKeyedServices
    (events : IO.Ref (List String)) :
  KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      ReporterOutputs :=
  (githubFromConfigLayer events).andThenInto
    (failingReporterFromGithubAndStoreLayer events)
    (by rfl)

def reporterProgram :=
  Z.serviceWithZ[Reporter] (fun reporter => reporter.report)

example : Z (Services[Reporter]) Empty String :=
  reporterProgram

def checkVerticalKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Reporter])
      (GithubBuildError ⊕ ReporterBuildError)
      String := reporterProgram
  match <- (verticalKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect "stable-keyed-layer-vertical" with
  | some (.success "issue:2") => pure ()
  | _ => throw (IO.userError "The vertical keyed layers did not run.")
  assertEvents "vertical keyed layers" events [
    "acquire-github-from-config",
    "acquire-reporter",
    "release-reporter",
    "release-github-from-config"
  ]

def checkVerticalKeyedLayerFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Reporter])
      (GithubBuildError ⊕ ReporterBuildError)
      Unit := Z.serviceWith fun _ => ()
  match <- (failingVerticalKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-vertical-failure" with
  | some (.failure (.fail (.inr .unavailable))) => pure ()
  | _ => throw (IO.userError "The vertical layer error was not preserved.")
  assertEvents "vertical keyed layer failure" events [
    "acquire-github-from-config",
    "acquire-reporter",
    "release-github-from-config"
  ]

/-! Pass-through composition keeps upstream and downstream outputs. -/

abbrev PassThroughOutputs : List Entry.{1} :=
  ServiceRow[Github, Reporter]

example : Row.Disjoint [githubEntry] [reporterEntry] := by decide

example : ¬ Row.Disjoint [githubEntry] [githubEntry] := by decide

example : Row.merge [githubEntry] [reporterEntry] =
    Row.merge [reporterEntry] [githubEntry] := by
  apply Row.merge_comm_exact_of_disjoint
  · simp [Row.Ordered]
  · simp [Row.Ordered]
  · decide

def passThroughKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      PassThroughOutputs :=
  (githubFromConfigLayer events).andThenKeepFreshMeetJoin
    (reporterFromGithubAndStoreLayer events)
    (by rfl)
    (by decide)

def failingPassThroughKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      PassThroughOutputs :=
  (githubFromConfigLayer events).andThenKeepFreshInto
    (failingReporterFromGithubAndStoreLayer events)
    (by rfl)
    (by decide)

def passThroughProgram := zdo
  let report <- Z.serviceWithZ[Reporter]
    (fun reporter => reporter.report)
  let count <- Z.serviceWithZ[Github]
    (fun github => github.issueCount "lean")
  pure s!"{report}:{count}"

example : Z (Services[Github, Reporter]) Empty String :=
  passThroughProgram

def checkPassThroughKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Github, Reporter])
      (GithubBuildError ⊕ ReporterBuildError)
      String := passThroughProgram
  match <- (passThroughKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-pass-through" with
  | some (.success "issue:2:2") => pure ()
  | _ => throw (IO.userError "The pass-through keyed layers did not run.")
  assertEvents "pass-through keyed layers" events [
    "acquire-github-from-config",
    "acquire-reporter",
    "release-reporter",
    "release-github-from-config"
  ]

def checkPassThroughKeyedLayerFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Github, Reporter])
      (GithubBuildError ⊕ ReporterBuildError)
      Unit := Z.serviceWith fun _ => ()
  match <- (failingPassThroughKeyedServices events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-pass-through-failure" with
  | some (.failure (.fail (.inr .unavailable))) => pure ()
  | _ => throw (IO.userError "The pass-through layer error was not preserved.")
  assertEvents "pass-through keyed layer failure" events [
    "acquire-github-from-config",
    "acquire-reporter",
    "release-github-from-config"
  ]

/-! An explicit sharing scope memoizes one upstream service for two branches. -/

inductive MetricsBuildError where
  | unavailable
  deriving BEq, Repr

abbrev MetricsInputs : List Entry.{1} :=
  ServiceRow[Github]

abbrev MetricsOutputs : List Entry.{1} :=
  ServiceRow[Metrics]

abbrev SharedGraphError :=
  GithubBuildError ⊕ (ReporterBuildError ⊕ MetricsBuildError)

abbrev SharedGraphOutputs : List Entry.{1} :=
  ServiceRow[Metrics, Reporter]

def metricsFromGithubLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.fromLayer (Layer.acquireRelease
      (fun environment =>
        HEIO.bind (recordLayerEvent events "acquire-metrics") fun _ =>
          let github := Services.get[Github] environment
          HEIO.pure { count := github.issueCount "lean" })
      (fun _ _ => recordLayerEvent events "release-metrics"))

def slowMetricsFromGithubLayer
    (events : IO.Ref (List String))
    (started : IO.Ref Bool) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun environment =>
      HEIO.bind (recordLayerEvent events "acquire-slow-metrics") fun _ =>
        HEIO.bind
          (HEIO.liftIO.{0} Cause.die (started.set true)) fun _ =>
            let pending : HEIO
                (Cause MetricsBuildError)
                (ULift.{1} Unit) :=
              HEIO.asyncInterrupt Cause.die fun _ =>
                pure <| events.modify fun current =>
                  current ++ ["cancel-slow-metrics"]
            HEIO.bind pending fun _ =>
              let github := Services.get[Github] environment
              HEIO.pure { count := github.issueCount "lean" })

def failingMetricsFromGithubLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun _ =>
      HEIO.bind (recordLayerEvent events "acquire-metrics") fun _ =>
        HEIO.throw (.fail .unavailable))

def sharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      SharedGraphError
      SharedGraphOutputs := keyed_graph (error := SharedGraphError) {
  let github := (githubFromConfigLayer events).widenInput;
  let reporter := github >>> reporterFromGithubAndStoreLayer events;
  let metrics := github >>> metricsFromGithubLayer events;
  let outputs := reporter ++ metrics;
  yield outputs
}

def failingSharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      SharedGraphError
      SharedGraphOutputs := keyed_graph (error := SharedGraphError) {
  let github := (githubFromConfigLayer events).widenInput;
  let reporter := github >>> reporterFromGithubAndStoreLayer events;
  let metrics := github >>> failingMetricsFromGithubLayer events;
  let outputs := reporter ++ metrics;
  yield outputs
}

/-!
The automatic constructor accepts the same layers in dependency-independent
order. The expected type supplies the external input, error, and output rows.
-/

def automaticSharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      SharedGraphError
      SharedGraphOutputs := KeyedLayer.make [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticFailingSharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      SharedGraphError
      SharedGraphOutputs := KeyedLayer.make [
  failingMetricsFromGithubLayer events,
  githubFromConfigLayer events,
  reporterFromGithubAndStoreLayer events
]

def observeAutomaticParallelStart
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) : IO Nat := do
  let started ← counter.atomically do
    let started ← get
    let next := started + 1
    set next
    pure next
  if started == 1 then
    let _ ← IO.asTask do
      IO.sleep 1000
      barrier.resolve ()
  else
    barrier.resolve ()
  let _ ← IO.wait barrier.result?
  counter.atomically get

def parallelMetricsFromGithubLayer
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) :
    KeyedLayer
      (Services[Github])
      MetricsBuildError
      (ServiceRow[Metrics]) :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die
          (observeAutomaticParallelStart counter barrier))
        fun started => HEIO.pure {
          count := Z.succeedNow started.down
        })

def parallelReporterFromGithubLayer
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) :
    KeyedLayer
      (Services[Github])
      ReporterBuildError
      (ServiceRow[Reporter]) :=
  KeyedLayer.fromLayer (Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die
          (observeAutomaticParallelStart counter barrier))
        fun started => HEIO.pure {
          report := Z.succeedNow (toString started.down)
        })

def automaticParallelDependencyGraph
    (events : IO.Ref (List String))
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) :
    KeyedLayer
      (Services[Config])
      SharedGraphError
      SharedGraphOutputs := KeyedLayer.make [
  parallelMetricsFromGithubLayer counter barrier,
  githubFromConfigLayer events,
  parallelReporterFromGithubLayer counter barrier
]

def sharedGraphProgram := zdo
  let report <- Z.serviceWithZ[Reporter]
    (fun reporter => reporter.report)
  let count <- Z.serviceWithZ[Metrics]
    (fun metrics => metrics.count)
  pure s!"{report}:{count}"

example : Z (Services[Metrics, Reporter]) Empty String :=
  sharedGraphProgram

def sharedGraphUnitProgram : Z (Services[Metrics, Reporter]) Empty Unit :=
  Z.succeedNow ()

def parallelGraphProgram := zdo
  let count ← Z.serviceWithZ[Metrics]
    (fun metrics => metrics.count)
  let report ← Z.serviceWithZ[Reporter]
    (fun reporter => reporter.report)
  pure (count, report)

example : Z (Services[Metrics, Reporter]) Empty (Nat × String) :=
  parallelGraphProgram

def failingSharedGraphProgram :
    Z (Services[Metrics, Reporter]) SharedGraphError Unit :=
  (Z.failCause (R := Services[Metrics, Reporter])
    (.fail (.inl .unavailable))).map impossible

def waitingSharedGraphProgram
    (started : IO.Ref Bool) :
    Z (Services[Metrics, Reporter]) Empty Unit :=
  Z.async fun _ => started.set true

def metricsUnitProgram : Z (Services[Metrics]) Empty Unit :=
  Z.succeedNow ()

def automaticallyProvidedSharedGraph
    (events : IO.Ref (List String)) :=
  Z.provide sharedGraphProgram [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def typeFacadeApplication
    (events : IO.Ref (List String)) :=
  Z.provide sharedGraphProgram [
    metricsFromGithubLayer events,
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events,
    KeyedLayer.succeed config,
    KeyedLayer.succeed store
  ]

example (events : IO.Ref (List String)) :
    Z
      (Services[])
      (GithubBuildError ⊕ MetricsBuildError ⊕ ReporterBuildError)
      String :=
  typeFacadeApplication events

def automaticallyProvidedFailingSharedGraph
    (events : IO.Ref (List String)) :=
  Z.provide sharedGraphUnitProgram [
  failingMetricsFromGithubLayer events,
  githubFromConfigLayer events,
  reporterFromGithubAndStoreLayer events
]

def automaticallyProvidedFailingProgram
    (events : IO.Ref (List String)) :
    Z
      (Services[Config, Store])
      SharedGraphError
      Unit := Z.provide failingSharedGraphProgram [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticallyProvidedWaitingProgram
    (events : IO.Ref (List String))
    (started : IO.Ref Bool) :
    Z
      (Services[Config, Store])
      SharedGraphError
      Unit := Z.provide (waitingSharedGraphProgram started) [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticallyProvidedSlowMetrics
    (events : IO.Ref (List String))
    (started : IO.Ref Bool) :
    Z (Services[Config]) SharedGraphError Unit :=
  Z.provide metricsUnitProgram [
    slowMetricsFromGithubLayer events started,
    githubFromConfigLayer events
  ]

def checkSharedDependencyGraph : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Metrics, Reporter])
      SharedGraphError
      String := sharedGraphProgram
  match <- (sharedDependencyGraph events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-shared-graph" with
  | some (.success "issue:2:2") => pure ()
  | _ => throw (IO.userError "The shared dependency graph did not run.")
  assertParallelSiblingEvents "shared dependency graph" events
    "acquire-reporter" "acquire-metrics" [
      "release-metrics",
      "release-reporter",
      "release-github-from-config"
    ]

def checkSharedDependencyGraphFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Metrics, Reporter])
      SharedGraphError
      Unit := Z.serviceWith fun _ => ()
  match <- (failingSharedDependencyGraph events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-shared-graph-failure" with
  | some (.failure (.fail (.inr (.inr .unavailable)))) => pure ()
  | _ => throw (IO.userError "The shared graph error was not preserved.")
  assertParallelFailureEvents "shared dependency graph failure" events
    "acquire-metrics" "acquire-reporter" "release-reporter"

def checkAutomaticSharedDependencyGraph : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Metrics, Reporter])
      SharedGraphError
      String := sharedGraphProgram
  match <- (automaticSharedDependencyGraph events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-automatic-graph" with
  | some (.success "issue:2:2") => pure ()
  | _ => throw (IO.userError "The automatic dependency graph did not run.")
  assertParallelSiblingEvents "automatic dependency graph" events
    "acquire-metrics" "acquire-reporter" [
      "release-reporter",
      "release-metrics",
      "release-github-from-config"
    ]

def checkAutomaticParallelDependencyGraph : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let counter ← Std.Mutex.new 0
  let barrier ← IO.Promise.new (α := Unit)
  let input : Builder [configEntry] :=
    Builder.empty.addFresh configEntry config (by decide)
  match ← (automaticParallelDependencyGraph events counter barrier).toLayer.run
      input.environment parallelGraphProgram
      "stable-keyed-layer-automatic-parallel" with
  | some (.success (2, "2")) => pure ()
  | _ => throw (IO.userError
      "Independent automatic layer branches did not overlap.")
  assertEvents "automatic parallel dependency graph" events [
    "acquire-github-from-config",
    "release-github-from-config"
  ]

def checkAutomaticSharedDependencyGraphFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Services[Metrics, Reporter])
      SharedGraphError
      Unit := Z.serviceWith fun _ => ()
  match <- (automaticFailingSharedDependencyGraph events).toLayer.run
      heterogeneousInputs.environment effect
      "stable-keyed-layer-automatic-graph-failure" with
  | some (.failure (.fail (.inr (.inr .unavailable)))) => pure ()
  | _ => throw (IO.userError "The automatic graph error was not preserved.")
  assertParallelFailureEvents "automatic dependency graph failure" events
    "acquire-metrics" "acquire-reporter" "release-reporter"

def checkZProvide : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect :=
    (automaticallyProvidedSharedGraph events).provideEnvironment
      heterogeneousInputs.environment
  match <- Z.unsafeRunSync effect "stable-keyed-z-provide" with
  | some (.success "issue:2:2") => pure ()
  | _ => throw (IO.userError "Z.provide did not run the program.")
  assertParallelSiblingEvents "Z.provide" events
    "acquire-metrics" "acquire-reporter" [
      "release-reporter",
      "release-metrics",
      "release-github-from-config"
    ]

def checkTypeFacadeApplication : IO Unit := do
  let events ← IO.mkRef ([] : List String)
  let effect := (typeFacadeApplication events).provideEnvironment
    Services.empty
  match ← Z.unsafeRunSync effect "stable-keyed-type-facade" with
  | some (.success "issue:2:2") => pure ()
  | _ => throw (IO.userError "The type-based application did not run.")
  assertParallelSiblingEvents "type-based application" events
    "acquire-metrics" "acquire-reporter" [
      "release-reporter",
      "release-metrics",
      "release-github-from-config"
    ]

def checkZProvideFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect :=
    (automaticallyProvidedFailingSharedGraph events).provideEnvironment
      heterogeneousInputs.environment
  match <- Z.unsafeRunSync effect "stable-keyed-z-provide-failure" with
  | some (.failure (.fail (.inr (.inl .unavailable)))) => pure ()
  | _ => throw (IO.userError "Z.provide did not preserve the layer error.")
  assertParallelFailureEvents "Z.provide failure" events
    "acquire-metrics" "acquire-reporter" "release-reporter"

def checkZProvideProgramFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect :=
    (automaticallyProvidedFailingProgram events).provideEnvironment
      heterogeneousInputs.environment
  match <- Z.unsafeRunSync effect "stable-keyed-z-provide-program-failure" with
  | some (.failure (.fail (.inl .unavailable))) => pure ()
  | _ => throw (IO.userError "Z.provide did not preserve the program error.")
  assertParallelSiblingEvents "Z.provide program failure" events
    "acquire-metrics" "acquire-reporter" [
      "release-reporter",
      "release-metrics",
      "release-github-from-config"
    ]

def checkZProvideInterruption : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let started <- IO.mkRef false
  let effect :=
    (automaticallyProvidedWaitingProgram events started).provideEnvironment
      heterogeneousInputs.environment
  let fiber ← Z.unsafeFork effect "stable-keyed-z-provide-interruption"
  waitForSignal "provided program" started
  fiber.requestInterrupt
  match ← fiber.awaitPoll (fiberId := fiber.fiberId) with
  | some (.failure .interrupt) => pure ()
  | _ => throw (IO.userError "Z.provide did not preserve interruption.")
  fiber.awaitTask
  assertParallelSiblingEvents "Z.provide interruption" events
    "acquire-metrics" "acquire-reporter" [
      "release-reporter",
      "release-metrics",
      "release-github-from-config"
    ]

def checkZProvideAcquisitionInterruption : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let started <- IO.mkRef false
  let input : Builder [configEntry] :=
    Builder.empty.addFresh configEntry config (by decide)
  let effect :=
    (automaticallyProvidedSlowMetrics events started).provideEnvironment
      input.environment
  let fiber ← Z.unsafeFork effect
    "stable-keyed-z-provide-acquisition-interruption"
  waitForSignal "slow layer acquisition" started
  fiber.requestInterrupt
  match ← fiber.awaitPoll (fiberId := fiber.fiberId) with
  | some (.failure .interrupt) => pure ()
  | _ => throw (IO.userError
      "Z.provide did not preserve acquisition interruption.")
  fiber.awaitTask
  assertEvents "Z.provide acquisition interruption" events [
    "acquire-github-from-config",
    "acquire-slow-metrics",
    "cancel-slow-metrics",
    "release-github-from-config"
  ]

def demo : IO Unit := do
  match <- run with
  | some (.success "issue:2") =>
      IO.println "Stable service-key checks passed."
  | some (.success value) =>
      throw (IO.userError s!"Unexpected stable service-key value: {value}")
  | some (.failure _) =>
      throw (IO.userError "The stable service-key checks failed.")
  | none =>
      throw (IO.userError "The stable service-key checks returned no result.")
  checkParameterizedServiceKeys
  IO.println "Parameterized service-key checks passed."
  checkKeyedLayerSuccess
  checkKeyedLayerAcquisitionFailure
  checkKeyedLayerProgramFailure
  IO.println "Keyed layer lifecycle checks passed."
  checkHeterogeneousKeyedLayers
  checkHeterogeneousKeyedLayersReverse
  checkHeterogeneousKeyedLayerFailure
  IO.println "Heterogeneous keyed-layer checks passed."
  checkVerticalKeyedLayers
  checkVerticalKeyedLayerFailure
  IO.println "Vertical keyed-layer checks passed."
  checkPassThroughKeyedLayers
  checkPassThroughKeyedLayerFailure
  IO.println "Pass-through keyed-layer checks passed."
  checkSharedDependencyGraph
  checkSharedDependencyGraphFailure
  IO.println "Shared keyed-layer graph checks passed."
  checkAutomaticSharedDependencyGraph
  checkAutomaticParallelDependencyGraph
  checkAutomaticSharedDependencyGraphFailure
  IO.println "Automatic keyed-layer graph checks passed."
  checkTypeFacadeApplication
  IO.println "Type-based application checks passed."
  checkZProvide
  checkZProvideFailure
  checkZProvideProgramFailure
  checkZProvideInterruption
  checkZProvideAcquisitionInterruption
  IO.println "Z.provide checks passed."

end StableServiceKeys
