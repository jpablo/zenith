import Examples.StableServiceKeysDemo

/-! Compile-time checks for automatic keyed-layer graph diagnostics. -/

namespace StableServiceKeys.KeyedLayerMakeDiagnostics

open Z

private def githubFromNothing :
    KeyedLayer (Environment ([] : List Entry.{1})) Empty [githubEntry] :=
  KeyedLayer.singleton githubEntry <|
    Layer.fromFunction fun _ => {
      issueCount := fun _ => pure 0
    }

private def anotherGithubFromNothing :
    KeyedLayer (Environment ([] : List Entry.{1})) Empty [githubEntry] :=
  KeyedLayer.singleton githubEntry <|
    Layer.fromFunction fun _ => {
      issueCount := fun _ => pure 1
    }

private def metricsFromNothing :
    KeyedLayer (Environment ([] : List Entry.{1})) Empty [metricsEntry] :=
  KeyedLayer.singleton metricsEntry <|
    Layer.fromFunction fun _ => {
      count := pure 0
    }

private def githubAndMetricsFromNothing :=
  KeyedLayer.zipFresh githubFromNothing metricsFromNothing (by decide)

private def projectedMultiOutput :
    KeyedLayer (Environment []) Empty [githubEntry] :=
  KeyedLayer.make [githubAndMetricsFromNothing]

private def reporterFromGithub :
    KeyedLayer (Environment [githubEntry]) Empty [reporterEntry] :=
  KeyedLayer.singleton reporterEntry <|
    Layer.fromFunction fun _ => {
      report := pure "report"
    }

private def reporterFromGithubWithError :
    KeyedLayer
      (Environment [githubEntry]) ReporterBuildError [reporterEntry] :=
  KeyedLayer.singleton reporterEntry <|
    Layer.failCause (.fail ReporterBuildError.unavailable)

private def reporterFromGithubAndMetrics :
    KeyedLayer
      (Environment [githubEntry, metricsEntry]) Empty [reporterEntry] :=
  KeyedLayer.singleton reporterEntry <|
    Layer.fromFunction fun _ => {
      report := pure "report"
    }

private def twoInternalDependencies :
    KeyedLayer (Environment []) Empty [reporterEntry] :=
  KeyedLayer.make [
    reporterFromGithubAndMetrics,
    metricsFromNothing,
    githubFromNothing
  ]

private def githubFromMetrics :
    KeyedLayer (Environment [metricsEntry]) Empty [githubEntry] :=
  KeyedLayer.singleton githubEntry <|
    Layer.fromFunction fun _ => {
      issueCount := fun _ => pure 0
    }

private def metricsFromGithub :
    KeyedLayer (Environment [githubEntry]) Empty [metricsEntry] :=
  KeyedLayer.singleton metricsEntry <|
    Layer.fromFunction fun _ => {
      count := pure 0
    }

private def inferredStandalone :=
  KeyedLayer.make (ServiceRow[Reporter]) [
    reporterFromGithub,
    githubFromNothing
  ]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredStandalone -/
#guard_msgs (info, substring := true) in
#check inferredStandalone

example : KeyedLayer (Services[]) Empty (ServiceRow[Reporter]) :=
  inferredStandalone

private def inferredExternal :=
  KeyedLayer.make (ServiceRow[Reporter]) [reporterFromGithub]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredExternal -/
#guard_msgs (info, substring := true) in
#check inferredExternal

example : KeyedLayer (Services[Github]) Empty (ServiceRow[Reporter]) :=
  inferredExternal

private def inferredInputsAndErrors (events : IO.Ref (List String)) :=
  KeyedLayer.make (ServiceRow[Reporter]) [
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events
  ]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredInputsAndErrors -/
#guard_msgs (info, substring := true) in
#check inferredInputsAndErrors

example (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      (ServiceRow[Reporter]) :=
  inferredInputsAndErrors events

private def inferredInputsAndErrorsReverse
    (events : IO.Ref (List String)) :=
  KeyedLayer.make (ServiceRow[Reporter]) [
    githubFromConfigLayer events,
    reporterFromGithubAndStoreLayer events
  ]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredInputsAndErrorsReverse -/
#guard_msgs (info, substring := true) in
#check inferredInputsAndErrorsReverse

example (events : IO.Ref (List String)) :
    KeyedLayer
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError)
      (ServiceRow[Reporter]) :=
  inferredInputsAndErrorsReverse events

private def inferredProvided (events : IO.Ref (List String)) :=
  Z.provide sharedGraphProgram [
    metricsFromGithubLayer events,
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events
  ]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredProvided -/
#guard_msgs (info, substring := true) in
#check inferredProvided

example (events : IO.Ref (List String)) :
    Z
      (Services[Config, Store])
      (GithubBuildError ⊕ MetricsBuildError ⊕ ReporterBuildError)
      String :=
  inferredProvided events

private inductive ProgramError where
  | unavailable

private def programWithOwnError :
    Z (Environment [reporterEntry]) ProgramError Unit :=
  (Z.failCause (R := Environment [reporterEntry])
    (.fail ProgramError.unavailable)).map impossible

private def inferredProvidedProgramError
    (events : IO.Ref (List String)) :=
  Z.provide programWithOwnError [
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events
  ]

/-- info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredProvidedProgramError -/
#guard_msgs (info, substring := true) in
#check inferredProvidedProgramError

example (events : IO.Ref (List String)) :
    Z
      (Services[Config, Store])
      (GithubBuildError ⊕ ReporterBuildError ⊕ ProgramError)
      Unit :=
  inferredProvidedProgramError events

/--
info: Keyed layer graph
error type: Empty
external inputs: (none)
final outputs: Metrics, Reporter
selected providers:
  Metrics <- [0] metricsFromGithub
  Reporter <- [1] reporterFromGithub
selected candidates:
  [2] githubFromNothing
    inputs: (none)
    outputs: Github
  [0] metricsFromGithub
    inputs: Github
    outputs: Metrics
  [1] reporterFromGithub
    inputs: Github
    outputs: Reporter
dependency edges:
  [2] githubFromNothing -> [0] metricsFromGithub
  [2] githubFromNothing -> [1] reporterFromGithub
parallel groups:
  final providers: [0] metricsFromGithub | [1] reporterFromGithub
shared nodes:
  [2] githubFromNothing (2 consumers)
unused candidates:
  (none)
-/
#guard_msgs in
#keyed_layer_graph
  (ServiceRow[Metrics, Reporter])
  [metricsFromGithub, reporterFromGithub, githubFromNothing]

/--
info: Keyed layer graph
error type: ReporterBuildError
external inputs: Github
final outputs: Reporter
selected providers:
  Reporter <- [0] reporterFromGithubWithError
selected candidates:
  [0] reporterFromGithubWithError
    inputs: Github
    outputs: Reporter
dependency edges:
  (none)
parallel groups:
  (none)
shared nodes:
  (none)
unused candidates:
  (none)
-/
#guard_msgs in
#keyed_layer_graph
  (ServiceRow[Reporter])
  [reporterFromGithubWithError]

/--
error: no layer provides required service
-/
#guard_msgs (error, substring := true) in
#keyed_layer_graph
  (KeyedLayer (Environment []) Empty [reporterEntry])
  [reporterFromGithub]

/--
error: no layer provides required service
-/
#guard_msgs (error, substring := true) in
private def missingProvider :
    KeyedLayer (Environment []) Empty [reporterEntry] :=
  KeyedLayer.make [reporterFromGithub]

/--
error: more than one layer provides requested service
-/
#guard_msgs (error, substring := true) in
private def ambiguousProvider :
    KeyedLayer (Environment []) Empty [githubEntry] :=
  KeyedLayer.make [githubFromNothing, anotherGithubFromNothing]

/--
error: automatic keyed-layer construction found a dependency cycle
-/
#guard_msgs (error, substring := true) in
private def cyclicGraph :
    KeyedLayer (Environment []) Empty [metricsEntry] :=
  KeyedLayer.make [githubFromMetrics, metricsFromGithub]

/--
warning: unused automatic layer candidate
-/
#guard_msgs (warning, substring := true) in
private def unusedCandidate :
    KeyedLayer (Environment []) Empty [githubEntry] :=
  KeyedLayer.make [githubFromNothing, metricsFromNothing]

private structure IndexedService (index : Nat) : Type 1 where
  marker : Unit
  deriving ServiceKey

service_key indexedEntryOne : IndexedService 1

service_key indexedEntryTwo : IndexedService 2

example : Row.Fresh indexedEntryTwo.key [indexedEntryOne] := by decide

end StableServiceKeys.KeyedLayerMakeDiagnostics
