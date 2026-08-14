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
      render := Z.succeed (Services.get[Label] environment).text
    }

def reportFromLabelAndNote :
    KeyedLayer (Services[Label, Note]) Empty [reportEntry] :=
  KeyedLayer.singleton reportEntry <|
    Layer.fromFunction fun environment => {
      render :=
        let label := (Services.get[Label] environment).text
        let note := (Services.get[Note] environment).text
        Z.succeed s!"{label}:{note}"
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

namespace LayerDerive

open Z

structure Config : Type 1 where
  base : String
  deriving ServiceKey

structure Clock : Type 1 where
  suffix : String
  deriving ServiceKey

structure Repository : Type 1 where
  label : String
  deriving ServiceKey

structure Application : Type 1 where
  run : Z Unit Empty String
  deriving ServiceKey

def makeRepository (config : Config) (clock : Clock) : Repository :=
  { label := config.base ++ clock.suffix }

def makeApplication
    (repository : Repository)
    (config : Config) : Application :=
  { run := Z.succeed s!"{config.base}:{repository.label}" }

def makeDefaultConfig : Config :=
  { base := "default" }

def compareConfig (left right : Config) : Repository :=
  { label := s!"{left.base}:{right.base}" }

def repositoryLayer :=
  KeyedLayer.derive makeRepository

example :
    KeyedLayer
      (Services[Config, Clock])
      Empty
      (ServiceRow[Repository]) :=
  repositoryLayer

def applicationLayer :=
  KeyedLayer.derive makeApplication

example :
    KeyedLayer
      (Services[Repository, Config])
      Empty
      (ServiceRow[Application]) :=
  applicationLayer

def defaultConfigLayer :=
  KeyedLayer.derive makeDefaultConfig

example :
    KeyedLayer
      (Services[])
      Empty
      (ServiceRow[Config]) :=
  defaultConfigLayer

def repeatedDependencyLayer :=
  KeyedLayer.derive compareConfig

example :
    KeyedLayer
      (Services[Config])
      Empty
      (ServiceRow[Repository]) :=
  repeatedDependencyLayer

structure Composite : Type 1 where
  config : Config
  clock : Clock
  deriving ServiceKey

def compositeLayer :=
  KeyedLayer.derive[Composite]

example :
    KeyedLayer
      (Services[Config, Clock])
      Empty
      (ServiceRow[Composite]) :=
  compositeLayer

structure Box (Value : Type 1) : Type 1 where
  value : Value
  deriving ServiceKey

def boxedConfigLayer :=
  KeyedLayer.derive[Box Config]

example :
    KeyedLayer
      (Services[Config])
      Empty
      (ServiceRow[Box Config]) :=
  boxedConfigLayer

def program : Z (Services[Application]) Empty String :=
  Z.serviceWithZ[Application] fun application => application.run

def provided : Z (Services[]) Empty String :=
  Z.provide program [
    applicationLayer,
    repositoryLayer,
    KeyedLayer.succeed ({ base := "left" } : Config),
    KeyedLayer.succeed ({ suffix := "-right" } : Clock)
  ]

end LayerDerive

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

def testConstructorLayerDerivation : IO Unit := do
  let closed := LayerDerive.provided.provideEnvironment Z.Services.empty
  match ← runProgram "constructor-layer-derivation" closed with
  | .success "left:left-right" => pure ()
  | .success value =>
      failTest s!"the derived constructor graph returned {value}"
  | .failure cause =>
      failTest s!"the derived constructor graph failed: {cause}"

def keyedRegressionTests : List (String × IO Unit) := [
  ("testKeyedGraphPlanMatchesRuntimeInputBinding",
    testKeyedGraphPlanMatchesRuntimeInputBinding),
  ("testConstructorLayerDerivation", testConstructorLayerDerivation)
]
