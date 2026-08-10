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
  (KeyedLayer
    (Environment [])
    Empty
    [metricsEntry, reporterEntry])
  [metricsFromGithub, reporterFromGithub, githubFromNothing]

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
