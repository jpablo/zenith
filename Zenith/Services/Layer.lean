
import Z.Resource.Internal.HEIO
import Z.Resource.Layer
import Z.Runtime.Interpreter
import Zenith.Services.Environment

/-!
Keyed layers and keyed service access.
-/

namespace Z

namespace Services

/-- The empty keyed service environment. -/
def empty : Environment ([] : List Entry.{u}) :=
  .empty

end Services

/-- Build a typed environment without knowledge of its final storage order. -/
structure Builder.{u} (entries : List Entry.{u}) where
  environment : Environment entries

namespace Builder

/-- Start a builder for an empty keyed service environment. -/
def empty : Builder [] :=
  ⟨Environment.empty⟩

/-- Add a service whose key is not yet present in the builder. -/
def addFresh
    (builder : Builder entries)
    (entry : Entry)
    (value : entry.Service)
    (_fresh : Row.Fresh entry.key entries) :
    Builder (Row.insert entry entries) :=
  ⟨Environment.insert entry value builder.environment⟩

/-- Keep one value when the exact service entry is already in the row. -/
def addExisting
    (builder : Builder entries)
    (entry : Entry)
    [Contains entry entries]
    (_value : entry.Service) : Builder entries :=
  builder

/-- Convert the completed builder into a closed layer. -/
def toLayer (builder : Builder entries) :
    Layer Unit Empty (Environment entries) :=
  Layer.succeed builder.environment

end Builder

/-- A production layer whose output is a normalized keyed environment. -/
structure KeyedLayer.{uin, uout}
    (R : Type uin)
    (E : Type)
    (entries : List Entry.{uout}) where
  layer : Layer R E (Environment entries)

namespace KeyedLayer

/-- Give one service layer a one-entry keyed environment. -/
def singleton
    (entry : Entry)
    (layer : Layer R E entry.Service) :
    KeyedLayer R E [entry] :=
  ⟨layer.map fun value => .cons value .empty⟩

/-- Build a closed one-service keyed layer. -/
def succeedEntry
    (entry : Entry.{u})
    (value : entry.Service) :
    KeyedLayer
      (Environment ([] : List Entry.{u}))
      Empty
      [entry] :=
  singleton entry (Layer.fromFunction fun _ => value)

/-- Let a keyed layer read its required row from a larger input row. -/
def widenInput
    [provider : Environment.CanProvide available required]
    (self : KeyedLayer (Environment required) E entries) :
    KeyedLayer (Environment available) E entries :=
  ⟨self.layer.contramap provider.provide⟩

/--
Build a keyed layer once inside `use`, even when `use` refers to it more than
once. Shallow layer values require this explicit sharing scope because they do
not expose node identity for automatic graph memoization.
-/
def shareInto
    [error : ErrorChannel.CanInject E EOut]
    (self : KeyedLayer R E entries)
    (use : KeyedLayer R EOut entries -> KeyedLayer R EOut outEntries) :
    KeyedLayer R EOut outEntries :=
  let adapted := self.layer.mapError error.inject
  ⟨adapted.share fun shared =>
    (use { layer := shared }).layer⟩

/-- Keep only the requested output services. -/
def projectOutput
    [provider : Environment.CanProvide available required]
    (self : KeyedLayer R E available) :
    KeyedLayer R E required :=
  ⟨self.layer.map provider.provide⟩

/--
Build two keyed layers in sequence and merge their outputs by stable key.
The existing `Layer.zipWith` keeps acquisition failure and release behavior.
-/
def zipFresh
    (left : KeyedLayer R E leftEntries)
    (right : KeyedLayer R E rightEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer R E (Row.merge leftEntries rightEntries) :=
  ⟨left.layer.zipWith right.layer fun leftEnvironment rightEnvironment =>
    Environment.merge leftEnvironment rightEnvironment⟩

/-- Build two independent keyed layers in parallel and merge their outputs. -/
def zipFreshPar
    (left : KeyedLayer R E leftEntries)
    (right : KeyedLayer R E rightEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer R E (Row.merge leftEntries rightEntries) :=
  ⟨left.layer.zipWithPar right.layer fun leftEnvironment rightEnvironment =>
    Environment.merge leftEnvironment rightEnvironment⟩

/--
Build two keyed layers that require different keyed input rows. Both input
rows are projected from their stable union. Both errors are injected into the
selected result error type.
-/
def zipFreshInto
    {inputEntries : List Entry}
    [leftInput : Environment.CanProvide
      inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      inputEntries rightInputs]
    [leftError : ErrorChannel.CanInject ELeft E]
    [rightError : ErrorChannel.CanInject ERight E]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion : Row.merge leftInputs rightInputs = inputEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer
      (Environment inputEntries)
      E
      (Row.merge leftEntries rightEntries) :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError leftError.inject
  let adaptedRight :=
    right.layer.contramap rightInput.provide
      |>.mapError rightError.inject
  ⟨adaptedLeft.zipWith adaptedRight fun leftEnvironment rightEnvironment =>
    Environment.merge leftEnvironment rightEnvironment⟩

/-- Infer the least common error channel for two keyed input layers. -/
def zipFreshMeetJoin
    {inputEntries : List Entry}
    [join : ErrorChannel.Join ELeft ERight E]
    [leftInput : Environment.CanProvide
      inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      inputEntries rightInputs]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion : Row.merge leftInputs rightInputs = inputEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer
      (Environment inputEntries)
      E
      (Row.merge leftEntries rightEntries) :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError join.left
  let adaptedRight :=
    right.layer.contramap rightInput.provide
      |>.mapError join.right
  ⟨adaptedLeft.zipWith adaptedRight fun leftEnvironment rightEnvironment =>
    Environment.merge leftEnvironment rightEnvironment⟩

/--
Feed the output of `left` to `right`. The result input contains the input of
`left` plus only the inputs of `right` that `left` does not produce. Output
services take priority over external services with the same key.
-/
def andThenInto
    {inputEntries : List Entry}
    [leftInput : Environment.CanProvide inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      (Row.concat leftEntries inputEntries) rightInputs]
    [leftError : ErrorChannel.CanInject ELeft E]
    [rightError : ErrorChannel.CanInject ERight E]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion :
      Row.merge leftInputs (Row.missing rightInputs leftEntries) =
        inputEntries) :
    KeyedLayer (Environment inputEntries) E rightEntries :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError leftError.inject
  ⟨adaptedLeft.flatMap fun leftEnvironment =>
    right.layer
      |>.contramap (fun inputEnvironment =>
        rightInput.provide <|
          Environment.append leftEnvironment inputEnvironment)
      |>.mapError rightError.inject⟩

/-- Infer the common error channel for vertical keyed-layer composition. -/
def andThenMeetJoin
    {inputEntries : List Entry}
    [join : ErrorChannel.Join ELeft ERight E]
    [leftInput : Environment.CanProvide inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      (Row.concat leftEntries inputEntries) rightInputs]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion :
      Row.merge leftInputs (Row.missing rightInputs leftEntries) =
        inputEntries) :
    KeyedLayer (Environment inputEntries) E rightEntries :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError join.left
  ⟨adaptedLeft.flatMap fun leftEnvironment =>
    right.layer
      |>.contramap (fun inputEnvironment =>
        rightInput.provide <|
          Environment.append leftEnvironment inputEnvironment)
      |>.mapError join.right⟩

/--
Vertically compose two keyed layers and keep both output rows. The rows must
be disjoint so that pass-through does not silently select one duplicate value.
-/
def andThenKeepFreshInto
    {inputEntries : List Entry}
    [leftInput : Environment.CanProvide inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      (Row.concat leftEntries inputEntries) rightInputs]
    [leftError : ErrorChannel.CanInject ELeft E]
    [rightError : ErrorChannel.CanInject ERight E]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion :
      Row.merge leftInputs (Row.missing rightInputs leftEntries) =
        inputEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer
      (Environment inputEntries)
      E
      (Row.merge leftEntries rightEntries) :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError leftError.inject
  ⟨adaptedLeft.flatMap fun leftEnvironment =>
    let adaptedRight :=
      right.layer
        |>.contramap (fun inputEnvironment =>
          rightInput.provide <|
            Environment.append leftEnvironment inputEnvironment)
        |>.mapError rightError.inject
    adaptedRight.map fun rightEnvironment =>
      Environment.merge leftEnvironment rightEnvironment⟩

/-- Infer the error channel for pass-through vertical composition. -/
def andThenKeepFreshMeetJoin
    {inputEntries : List Entry}
    [join : ErrorChannel.Join ELeft ERight E]
    [leftInput : Environment.CanProvide inputEntries leftInputs]
    [rightInput : Environment.CanProvide
      (Row.concat leftEntries inputEntries) rightInputs]
    (left : KeyedLayer (Environment leftInputs) ELeft leftEntries)
    (right : KeyedLayer (Environment rightInputs) ERight rightEntries)
    (_inputUnion :
      Row.merge leftInputs (Row.missing rightInputs leftEntries) =
        inputEntries)
    (_disjoint : Row.Disjoint leftEntries rightEntries) :
    KeyedLayer
      (Environment inputEntries)
      E
      (Row.merge leftEntries rightEntries) :=
  let adaptedLeft :=
    left.layer.contramap leftInput.provide
      |>.mapError join.left
  ⟨adaptedLeft.flatMap fun leftEnvironment =>
    let adaptedRight :=
      right.layer
        |>.contramap (fun inputEnvironment =>
          rightInput.provide <|
            Environment.append leftEnvironment inputEnvironment)
        |>.mapError join.right
    adaptedRight.map fun rightEnvironment =>
      Environment.merge leftEnvironment rightEnvironment⟩

/-- View a keyed layer as an ordinary layer that provides its full row. -/
def toLayer (layer : KeyedLayer R E entries) :
    Layer R E (Environment entries) :=
  layer.layer

/--
Build a keyed layer, supply its environment to a program, and release the
layer after the program completes.

The current implementation runs the closed program in a nested fiber because
`ZCore` cannot store a service environment from an arbitrary universe. Outer
interruption cancels layer acquisition or that fiber, and waits for this layer
scope to release.
-/
def provide
    (self : KeyedLayer.{uin, uout} R E entries)
    (program : Z (Environment entries) E A)
    (fiberId : FiberId := "Z.provide") : Z R E A :=
  Z.fromCore fun input =>
    ZCore.asyncInterrupt fun observer => do
      let interruption ← HEIO.Interruption.new
      let builtAndRun :
          HEIO (Cause E) (ULift.{uout} (Exit E A)) :=
        HEIO.bind (self.layer.build input) fun resource =>
          let runProgram :
              HEIO (Cause E) (ULift.{uout} (Exit E A)) :=
            HEIO.bind HEIO.checkInterrupted fun _ =>
              HEIO.asyncInterrupt.{uout} Cause.die fun callback => do
                let fiber ← Z.unsafeFork
                  (program.provideEnvironment resource.value)
                  fiberId
                let waiter ← IO.asTask do
                  callback (.ok (<- fiber.await))
                  fiber.awaitTask
                pure do
                  fiber.requestInterrupt
                  fiber.awaitTask
                  let _ ← IO.wait waiter
                  pure ()
          runProgram.ensuring resource.release
      let worker ← IO.asTask do
        match ← HEIO.toIOResultInterruptible
            interruption .interrupt builtAndRun with
        | .ok exit => observer exit
        | .error cause => observer (.failure cause)
      pure do
        interruption.request
        let _ ← IO.wait worker
        pure ()

end KeyedLayer

/-!
`keyed_graph` gives each lexical binding one explicit sharing scope. The macro
keeps `Layer` shallow and lowers graph nodes to `KeyedLayer.shareInto`.
-/

declare_syntax_cat keyedGraphBinding
syntax "let " ident " := " term ";" : keyedGraphBinding
syntax "let " ident " := " ident " >>> " term ";" : keyedGraphBinding
syntax "let " ident " := " ident " ++ " ident ";" : keyedGraphBinding
syntax "keyed_graph" "{" keyedGraphBinding* "yield " term "}" : term
syntax "keyed_graph" "(" ident " := " term ")"
  "{" keyedGraphBinding* "yield " term "}" : term

macro_rules
  | `(keyed_graph { yield $result:term }) => `($result)
  | `(keyed_graph {
      let $name:ident := $value:term;
      $rest:keyedGraphBinding*
      yield $result:term
    }) =>
      `(KeyedLayer.shareInto $value fun $name =>
        keyed_graph { $rest* yield $result })
  | `(keyed_graph (error := $errorType:term) { yield $result:term }) =>
      `(show KeyedLayer _ $errorType _ from $result)
  | `(keyed_graph (error := $errorType:term) {
      let $name:ident := $left:ident >>> $right:term;
      $rest:keyedGraphBinding*
      yield $result:term
    }) =>
      `(KeyedLayer.shareInto (EOut := $errorType)
          (KeyedLayer.andThenInto (E := $errorType)
            $left $right (by rfl)) fun $name =>
        keyed_graph (error := $errorType) { $rest* yield $result })
  | `(keyed_graph (error := $errorType:term) {
      let $name:ident := $left:ident ++ $right:ident;
      $rest:keyedGraphBinding*
      yield $result:term
    }) =>
      `(KeyedLayer.shareInto (EOut := $errorType)
          (KeyedLayer.zipFreshPar $left $right (by decide)) fun $name =>
        keyed_graph (error := $errorType) { $rest* yield $result })
  | `(keyed_graph (error := $errorType:term) {
      let $name:ident := $value:term;
      $rest:keyedGraphBinding*
      yield $result:term
    }) =>
      `(KeyedLayer.shareInto (EOut := $errorType) $value fun $name =>
        keyed_graph (error := $errorType) { $rest* yield $result })

/-- Select a low-universe result from a keyed service. -/
def withService
    (entry : Entry)
    [Contains entry entries]
    (operation : entry.Service -> A) :
    Z (Environment entries) Empty A :=
  Z.serviceWith fun environment =>
    operation (Contains.get environment)

/-- Select a high-universe service without returning it as a fiber result. -/
def withServiceM
    (entry : Entry)
    [Contains entry entries]
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment entries) E A :=
  Z.serviceWithM fun environment =>
    operation (Contains.get environment)

/-- Select a low-universe result from one inferred service entry. -/
def withServiceEntry
    (entry : Entry)
    (operation : entry.Service -> A) :
    Z (Environment [entry]) Empty A :=
  withService (entries := [entry]) entry operation

/-- Run an effect with one inferred high-universe service entry. -/
def withServiceMEntry
    (entry : Entry)
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment [entry]) E A :=
  withServiceM (entries := [entry]) entry operation

end Z
