import Examples.StableServiceKeysDemo

/-! Compile-time checks for automatic keyed-layer graph diagnostics. -/

namespace StableServiceKeys.KeyedLayerMakeDiagnostics

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
  KeyedLayer.make ([reporterEntry]) [
    reporterFromGithub,
    githubFromNothing
  ]

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredStandalone : KeyedLayer (Environment []) Empty [reporterEntry]
-/
#guard_msgs in
#check inferredStandalone

private def inferredExternal :=
  KeyedLayer.make ([reporterEntry]) [reporterFromGithub]

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredExternal :
  KeyedLayer (Environment [githubEntry]) Empty [reporterEntry]
-/
#guard_msgs in
#check inferredExternal

private def inferredInputsAndErrors (events : IO.Ref (List String)) :=
  KeyedLayer.make ([reporterEntry]) [
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events
  ]

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredInputsAndErrors (events : IO.Ref (List String)) :
  KeyedLayer (Environment [configEntry, storeEntry]) (GithubBuildError ⊕ ReporterBuildError) [reporterEntry]
-/
#guard_msgs in
#check inferredInputsAndErrors

private def inferredInputsAndErrorsReverse
    (events : IO.Ref (List String)) :=
  KeyedLayer.make ([reporterEntry]) [
    githubFromConfigLayer events,
    reporterFromGithubAndStoreLayer events
  ]

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredInputsAndErrorsReverse (events : IO.Ref (List String)) :
  KeyedLayer (Environment [configEntry, storeEntry]) (GithubBuildError ⊕ ReporterBuildError) [reporterEntry]
-/
#guard_msgs in
#check inferredInputsAndErrorsReverse

private def inferredProvided (events : IO.Ref (List String)) :=
  Z.provide sharedGraphProgram [
    metricsFromGithubLayer events,
    reporterFromGithubAndStoreLayer events,
    githubFromConfigLayer events
  ]

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredProvided (events : IO.Ref (List String)) :
  Z (Environment [configEntry, storeEntry]) (GithubBuildError ⊕ MetricsBuildError ⊕ ReporterBuildError) String
-/
#guard_msgs in
#check inferredProvided

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

/--
info: StableServiceKeys.KeyedLayerMakeDiagnostics.inferredProvidedProgramError (events : IO.Ref (List String)) :
  Z (Environment [configEntry, storeEntry]) (GithubBuildError ⊕ ReporterBuildError ⊕ ProgramError) Unit
-/
#guard_msgs in
#check inferredProvidedProgramError

/--
info: Keyed layer graph
error type: Empty
external inputs: (none)
final outputs: metricsEntry, reporterEntry
selected providers:
  metricsEntry <- [0] metricsFromGithub
  reporterEntry <- [1] reporterFromGithub
selected candidates:
  [2] githubFromNothing
    inputs: (none)
    outputs: githubEntry
  [0] metricsFromGithub
    inputs: githubEntry
    outputs: metricsEntry
  [1] reporterFromGithub
    inputs: githubEntry
    outputs: reporterEntry
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
  ([metricsEntry, reporterEntry])
  [metricsFromGithub, reporterFromGithub, githubFromNothing]

/--
info: Keyed layer graph
error type: ReporterBuildError
external inputs: githubEntry
final outputs: reporterEntry
selected providers:
  reporterEntry <- [0] reporterFromGithubWithError
selected candidates:
  [0] reporterFromGithubWithError
    inputs: githubEntry
    outputs: reporterEntry
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
  ([reporterEntry])
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

end StableServiceKeys.KeyedLayerMakeDiagnostics
