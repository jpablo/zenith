import Z.Experimental.KeyedLayerMake

/-!
A separate experiment for stable, normalized service rows.

This file does not change the production `Z` environment. It tests whether an
explicit qualified service key can remove product-order knowledge from layer
code without one central numeric registry.
-/

namespace StableServiceKeys

/-! The example service keys come from service declaration names. -/

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

example : configEntry.key = {
    owner := "StableServiceKeys"
    name := "Config"
  } := rfl

example : otherConfigEntry.key = {
    owner := "StableServiceKeys.OtherLibrary"
    name := "Config"
  } := rfl

abbrev Services : List Entry.{1} :=
  [configEntry, githubEntry, storeEntry]

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
  let storeLayer := KeyedLayer.singleton storeEntry <|
    trackedServiceLayer events "store" store
  let githubLayer := KeyedLayer.singleton githubEntry <|
    trackedServiceLayer events "github" github
  let configLayer := KeyedLayer.singleton configEntry <|
    trackedServiceLayer events "config" config
  storeLayer.zipFresh githubLayer (by decide)
    |>.zipFresh configLayer (by decide)

abbrev ConfigGithubServices : List Entry.{1} :=
  [configEntry, githubEntry]

def failingKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer Unit String ConfigGithubServices :=
  let configLayer := KeyedLayer.singleton configEntry <|
    trackedServiceLayer events "config" config
  let githubLayer := KeyedLayer.singleton githubEntry <|
    failingServiceLayer events "github"
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
  [configEntry, storeEntry]

abbrev HeterogeneousOutputs : List Entry.{1} :=
  [githubEntry, otherConfigEntry]

def heterogeneousInputs : Builder HeterogeneousInputs :=
  Builder.empty
    |>.addFresh storeEntry store (by decide)
    |>.addFresh configEntry config (by decide)

def githubFromConfigLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [configEntry])
      GithubBuildError
      [githubEntry] :=
  KeyedLayer.singleton githubEntry <|
    Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-github-from-config") fun _ =>
            let config := Contains.get (target := configEntry) environment
            HEIO.pure {
              issueCount := fun organization =>
                Z.succeedNow <|
                  if organization == config.organization then 2 else 0
            })
      (fun _ _ =>
        recordLayerEvent events "release-github-from-config")

def otherConfigFromStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [storeEntry])
      OtherConfigBuildError
      [otherConfigEntry] :=
  KeyedLayer.singleton otherConfigEntry <|
    Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-other-config-from-store") fun _ =>
            let store := Contains.get (target := storeEntry) environment
            HEIO.pure { organization := store.label })
      (fun _ _ =>
        recordLayerEvent events "release-other-config-from-store")

def failingOtherConfigFromStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [storeEntry])
      OtherConfigBuildError
      [otherConfigEntry] :=
  KeyedLayer.singleton otherConfigEntry <|
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (recordLayerEvent events "acquire-other-config-from-store") fun _ =>
          HEIO.throw (.fail .unavailable)

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

def heterogeneousProgram :
    Z (Environment HeterogeneousOutputs) Empty String := zdo
  let count <- withServiceZ
    (entries := HeterogeneousOutputs) githubEntry fun github =>
      github.issueCount "lean"
  let organization <- withServiceZ
    (entries := HeterogeneousOutputs) otherConfigEntry fun otherConfig =>
      Z.succeedNow otherConfig.organization
  pure s!"{organization}:{count}"

def checkHeterogeneousKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Environment HeterogeneousOutputs)
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
      (Environment HeterogeneousOutputs)
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
      (Environment HeterogeneousOutputs)
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
  [githubEntry, storeEntry]

abbrev ReporterOutputs : List Entry.{1} :=
  [reporterEntry]

def reporterFromGithubAndStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment ReporterInputs)
      ReporterBuildError
      ReporterOutputs :=
  KeyedLayer.singleton reporterEntry <|
    Layer.acquireRelease
      (fun environment =>
        HEIO.bind
          (recordLayerEvent events "acquire-reporter") fun _ =>
            let github := Contains.get (target := githubEntry) environment
            let store := Contains.get (target := storeEntry) environment
            HEIO.pure {
              report := github.issueCount "lean" |>.map fun count =>
                s!"{store.label}:{count}"
            })
      (fun _ _ => recordLayerEvent events "release-reporter")

def failingReporterFromGithubAndStoreLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment ReporterInputs)
      ReporterBuildError
      ReporterOutputs :=
  KeyedLayer.singleton reporterEntry <|
    Layer.fromHEIO fun _ =>
      HEIO.bind (recordLayerEvent events "acquire-reporter") fun _ =>
        HEIO.throw (.fail .unavailable)

def verticalKeyedServices
    (events : IO.Ref (List String)) :
  KeyedLayer
      (Environment [configEntry, storeEntry])
      (GithubBuildError ⊕ ReporterBuildError)
      ReporterOutputs :=
  (githubFromConfigLayer events).andThenMeetJoin
    (reporterFromGithubAndStoreLayer events)
    (by rfl)

def failingVerticalKeyedServices
    (events : IO.Ref (List String)) :
  KeyedLayer
      (Environment [configEntry, storeEntry])
      (GithubBuildError ⊕ ReporterBuildError)
      ReporterOutputs :=
  (githubFromConfigLayer events).andThenInto
    (failingReporterFromGithubAndStoreLayer events)
    (by rfl)

def reporterProgram : Z (Environment ReporterOutputs) Empty String :=
  withServiceZ (entries := ReporterOutputs) reporterEntry fun reporter =>
    reporter.report

def checkVerticalKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Environment ReporterOutputs)
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
      (Environment ReporterOutputs)
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
  [githubEntry, reporterEntry]

example : Row.Disjoint [githubEntry] [reporterEntry] := by decide

example : ¬ Row.Disjoint [githubEntry] [githubEntry] := by decide

def passThroughKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [configEntry, storeEntry])
      (GithubBuildError ⊕ ReporterBuildError)
      PassThroughOutputs :=
  (githubFromConfigLayer events).andThenKeepFreshMeetJoin
    (reporterFromGithubAndStoreLayer events)
    (by rfl)
    (by decide)

def failingPassThroughKeyedServices
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [configEntry, storeEntry])
      (GithubBuildError ⊕ ReporterBuildError)
      PassThroughOutputs :=
  (githubFromConfigLayer events).andThenKeepFreshInto
    (failingReporterFromGithubAndStoreLayer events)
    (by rfl)
    (by decide)

def passThroughProgram :
    Z (Environment PassThroughOutputs) Empty String := zdo
  let report <- withServiceZ
    (entries := PassThroughOutputs) reporterEntry fun reporter =>
      reporter.report
  let count <- withServiceZ
    (entries := PassThroughOutputs) githubEntry fun github =>
      github.issueCount "lean"
  pure s!"{report}:{count}"

def checkPassThroughKeyedLayers : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Environment PassThroughOutputs)
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
      (Environment PassThroughOutputs)
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
  [githubEntry]

abbrev MetricsOutputs : List Entry.{1} :=
  [metricsEntry]

abbrev SharedGraphError :=
  GithubBuildError ⊕ (ReporterBuildError ⊕ MetricsBuildError)

abbrev SharedGraphOutputs : List Entry.{1} :=
  [metricsEntry, reporterEntry]

def metricsFromGithubLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.singleton metricsEntry <|
    Layer.acquireRelease
      (fun environment =>
        HEIO.bind (recordLayerEvent events "acquire-metrics") fun _ =>
          let github := Contains.get (target := githubEntry) environment
          HEIO.pure { count := github.issueCount "lean" })
      (fun _ _ => recordLayerEvent events "release-metrics")

def slowMetricsFromGithubLayer
    (events : IO.Ref (List String))
    (started : IO.Ref Bool) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.singleton metricsEntry <|
    Layer.fromHEIO fun environment =>
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
              let github := Contains.get
                (target := githubEntry) environment
              HEIO.pure { count := github.issueCount "lean" }

def failingMetricsFromGithubLayer
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment MetricsInputs)
      MetricsBuildError
      MetricsOutputs :=
  KeyedLayer.singleton metricsEntry <|
    Layer.fromHEIO fun _ =>
      HEIO.bind (recordLayerEvent events "acquire-metrics") fun _ =>
        HEIO.throw (.fail .unavailable)

def sharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [configEntry, storeEntry])
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
      (Environment [configEntry, storeEntry])
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
      (Environment [configEntry, storeEntry])
      SharedGraphError
      SharedGraphOutputs := KeyedLayer.make [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticFailingSharedDependencyGraph
    (events : IO.Ref (List String)) :
    KeyedLayer
      (Environment [configEntry, storeEntry])
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
      (Environment [githubEntry])
      MetricsBuildError
      [metricsEntry] :=
  KeyedLayer.singleton metricsEntry <|
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die
          (observeAutomaticParallelStart counter barrier))
        fun started => HEIO.pure {
          count := Z.succeedNow started.down
        }

def parallelReporterFromGithubLayer
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) :
    KeyedLayer
      (Environment [githubEntry])
      ReporterBuildError
      [reporterEntry] :=
  KeyedLayer.singleton reporterEntry <|
    Layer.fromHEIO fun _ =>
      HEIO.bind
        (HEIO.liftIO.{0} Cause.die
          (observeAutomaticParallelStart counter barrier))
        fun started => HEIO.pure {
          report := Z.succeedNow (toString started.down)
        }

def automaticParallelDependencyGraph
    (events : IO.Ref (List String))
    (counter : Std.Mutex Nat)
    (barrier : IO.Promise Unit) :
    KeyedLayer
      (Environment [configEntry])
      SharedGraphError
      SharedGraphOutputs := KeyedLayer.make [
  parallelMetricsFromGithubLayer counter barrier,
  githubFromConfigLayer events,
  parallelReporterFromGithubLayer counter barrier
]

def sharedGraphProgram :
    Z (Environment SharedGraphOutputs) Empty String := zdo
  let report <- withServiceZ
    (entries := SharedGraphOutputs) reporterEntry fun reporter =>
      reporter.report
  let count <- withServiceZ
    (entries := SharedGraphOutputs) metricsEntry fun metrics =>
      metrics.count
  pure s!"{report}:{count}"

def sharedGraphUnitProgram :
    Z (Environment SharedGraphOutputs) Empty Unit :=
  Z.serviceWith fun _ => ()

def parallelGraphProgram :
    Z (Environment SharedGraphOutputs) Empty (Nat × String) := zdo
  let count ← withServiceZ
    (entries := SharedGraphOutputs) metricsEntry fun metrics =>
      metrics.count
  let report ← withServiceZ
    (entries := SharedGraphOutputs) reporterEntry fun reporter =>
      reporter.report
  pure (count, report)

def failingSharedGraphProgram :
    Z (Environment SharedGraphOutputs) SharedGraphError Unit :=
  (Z.failCause (R := Environment SharedGraphOutputs)
    (.fail (.inl .unavailable))).map impossible

def waitingSharedGraphProgram
    (started : IO.Ref Bool) :
    Z (Environment SharedGraphOutputs) Empty Unit :=
  Z.async fun _ => started.set true

def metricsUnitProgram :
    Z (Environment [metricsEntry]) Empty Unit :=
  Z.serviceWith fun _ => ()

def automaticallyProvidedSharedGraph
    (events : IO.Ref (List String)) :
    Z
      (Environment [configEntry, storeEntry])
      SharedGraphError
      String := Z.provide sharedGraphProgram [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticallyProvidedFailingSharedGraph
    (events : IO.Ref (List String)) :
    Z
      (Environment [configEntry, storeEntry])
      SharedGraphError
      Unit := Z.provide sharedGraphUnitProgram [
  failingMetricsFromGithubLayer events,
  githubFromConfigLayer events,
  reporterFromGithubAndStoreLayer events
]

def automaticallyProvidedFailingProgram
    (events : IO.Ref (List String)) :
    Z
      (Environment [configEntry, storeEntry])
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
      (Environment [configEntry, storeEntry])
      SharedGraphError
      Unit := Z.provide (waitingSharedGraphProgram started) [
  metricsFromGithubLayer events,
  reporterFromGithubAndStoreLayer events,
  githubFromConfigLayer events
]

def automaticallyProvidedSlowMetrics
    (events : IO.Ref (List String))
    (started : IO.Ref Bool) :
    Z (Environment [configEntry]) SharedGraphError Unit :=
  Z.provide metricsUnitProgram [
    slowMetricsFromGithubLayer events started,
    githubFromConfigLayer events
  ]

def checkSharedDependencyGraph : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect : Z
      (Environment SharedGraphOutputs)
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
      (Environment SharedGraphOutputs)
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
      (Environment SharedGraphOutputs)
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
      (Environment SharedGraphOutputs)
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

def checkZProvideFailure : IO Unit := do
  let events <- IO.mkRef ([] : List String)
  let effect :=
    (automaticallyProvidedFailingSharedGraph events).provideEnvironment
      heterogeneousInputs.environment
  match <- Z.unsafeRunSync effect "stable-keyed-z-provide-failure" with
  | some (.failure (.fail (.inr (.inr .unavailable)))) => pure ()
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
      IO.println "Stable service-key prototype passed."
  | some (.success value) =>
      throw (IO.userError s!"Unexpected prototype value: {value}")
  | some (.failure _) =>
      throw (IO.userError "The stable service-key prototype failed.")
  | none =>
      throw (IO.userError "The stable service-key prototype returned no result.")
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
  checkZProvide
  checkZProvideFailure
  checkZProvideProgramFailure
  checkZProvideInterruption
  checkZProvideAcquisitionInterruption
  IO.println "Z.provide checks passed."

end StableServiceKeys

def main : IO Unit :=
  StableServiceKeys.demo
