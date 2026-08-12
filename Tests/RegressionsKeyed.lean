import Tests.Support

/-!
Regression tests for keyed-layer graph planning.

`andThenInto` gives dependency outputs priority over external services with the
same key, so the plan the automatic constructor selects has to bind candidate
inputs the same way. These tests run graphs whose external input row and whose
selected layers both carry one service key.
-/

namespace KeyedGraphPlan

open Z

structure Label : Type 1 where
  text : String

structure Note : Type 1 where
  text : String

structure Report : Type 1 where
  render : Z Unit Empty String

service_key labelEntry : Label

service_key noteEntry : Note

service_key reportEntry : Report

def externalLabel : Label :=
  { text := "external" }

def labelFromNothing :
    KeyedLayer (Environment ([] : List Entry.{1})) Empty [labelEntry] :=
  KeyedLayer.singleton labelEntry <|
    Layer.fromFunction fun _ => { text := "layer" }

def noteFromNothing :
    KeyedLayer (Environment ([] : List Entry.{1})) Empty [noteEntry] :=
  KeyedLayer.singleton noteEntry <|
    Layer.fromFunction fun _ => { text := "note" }

def labelAndNoteFromNothing :
    KeyedLayer
      (Environment ([] : List Entry.{1})) Empty [labelEntry, noteEntry] :=
  KeyedLayer.zipFresh labelFromNothing noteFromNothing (by decide)

def reportFromLabel :
    KeyedLayer (Services[Label]) Empty [reportEntry] :=
  KeyedLayer.singleton reportEntry <|
    Layer.fromFunction fun environment => {
      render := Z.succeedNow (Services.get[Label] environment).text
    }

def reportFromLabelAndNote :
    KeyedLayer (Services[Label, Note]) Empty [reportEntry] :=
  KeyedLayer.singleton reportEntry <|
    Layer.fromFunction fun environment => {
      render :=
        let label := (Services.get[Label] environment).text
        let note := (Services.get[Note] environment).text
        Z.succeedNow s!"{label}:{note}"
    }

def singleProviderGraph :
    KeyedLayer (Services[Label]) Empty [reportEntry] :=
  KeyedLayer.make [reportFromLabel, labelFromNothing]

def sharedProviderGraph :
    KeyedLayer (Services[Label]) Empty [reportEntry] :=
  KeyedLayer.make [reportFromLabelAndNote, labelAndNoteFromNothing]

def reportProgram : Z (Environment [reportEntry]) Empty String :=
  Z.serviceWithZ[Report] fun reporter => reporter.render

def runGraph
    (graph : KeyedLayer (Services[Label]) Empty [reportEntry])
    (name : String) : IO (Exit Empty String) := do
  let input : Builder [labelEntry] :=
    Builder.empty.addFresh labelEntry externalLabel (by decide)
  graph.toLayer.run input.environment reportProgram name

end KeyedGraphPlan

open KeyedGraphPlan in
/--
The automatic graph must bind a candidate input the same way the generated
composition does: a selected layer output wins over an external service with
the same key.
-/
def testKeyedGraphPlanMatchesRuntimeInputBinding : IO Unit := do
  match ← runGraph singleProviderGraph "keyed-graph-single-provider" with
  | .success "layer" => pure ()
  | .success other =>
      failTest s!"the planned provider was dropped and the graph produced {other}"
  | .failure cause => failTest s!"the single-provider graph failed: {cause}"

  match ← runGraph sharedProviderGraph "keyed-graph-shared-provider" with
  | .success "layer:note" => pure ()
  | .success other =>
      failTest s!"the shared provider graph produced {other}"
  | .failure cause => failTest s!"the shared provider graph failed: {cause}"

def keyedRegressionTests : List (String × IO Unit) := [
  ("testKeyedGraphPlanMatchesRuntimeInputBinding",
    testKeyedGraphPlanMatchesRuntimeInputBinding)
]
