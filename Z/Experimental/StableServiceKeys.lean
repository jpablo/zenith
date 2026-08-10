import Lean
import Z

/-!
Experimental stable, normalized service rows.

This module is not imported by the public `Z` module. It tests a possible
replacement for product environments without changing the production API.
-/

namespace StableServiceKeys

/-- A library owner and a service-local name form one stable identity. -/
structure Key where
  owner : String
  name : String
  deriving BEq, DecidableEq, Ord, Repr

/-- A stable qualified key and the service type assigned to it. -/
structure Entry.{u} where
  private mk ::
  key : Key
  Service : Type u

namespace Entry

private abbrev create (key : Key) (Service : Type u) : Entry :=
  ⟨key, Service⟩

end Entry

namespace Row

/-- Insert one entry in key order. Keep the existing entry for a duplicate. -/
@[reducible] def insert (entry : Entry) : List Entry -> List Entry
  | [] => [entry]
  | head :: tail =>
      match compare entry.key head.key with
      | .lt => entry :: head :: tail
      | .eq => head :: tail
      | .gt => head :: insert entry tail

/-- Give any list of entries one stable order and remove duplicate keys. -/
def normalize (entries : List Entry) : List Entry :=
  entries.foldr insert []

/-- Compute whether no row entry has this qualified key. -/
def isFresh (key : Key) : List Entry -> Bool
  | [] => true
  | head :: tail => key != head.key && isFresh key tail

/-- No entry in the row has this qualified key. -/
def Fresh (key : Key) (entries : List Entry) : Prop :=
  isFresh key entries = true

instance (key : Key) (entries : List Entry) : Decidable (Fresh key entries) := by
  unfold Fresh
  infer_instance

/-- Insert every entry from `right` into the normalized `left` row. -/
@[reducible] def merge (left : List Entry) : List Entry -> List Entry
  | [] => left
  | head :: tail => merge (insert head left) tail

/-- Keep the required entries that the provided row does not contain. -/
@[reducible] def missing
    (required provided : List Entry) : List Entry :=
  required.filter fun entry => isFresh entry.key provided

/-- Concatenate row shapes without key normalization. -/
@[reducible] def concat : List Entry -> List Entry -> List Entry
  | [], right => right
  | head :: tail, right => head :: concat tail right

/-- Compute whether a merge adds only new qualified keys. -/
def canMerge (left : List Entry) : List Entry -> Bool
  | [] => true
  | head :: tail =>
      isFresh head.key left && canMerge (insert head left) tail

def Disjoint (left right : List Entry) : Prop :=
  canMerge left right = true

instance (left right : List Entry) : Decidable (Disjoint left right) := by
  unfold Disjoint
  infer_instance

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

/-- Merge two typed environments in stable key order. -/
def merge
    (left : Environment leftEntries) :
    {rightEntries : List Entry} ->
      Environment rightEntries ->
      Environment (Row.merge leftEntries rightEntries)
  | [], .empty => left
  | entry :: _, .cons value tail =>
      merge (insert entry value left) tail

/-- Append environments without normalization for temporary projection. -/
def append
    (left : Environment leftEntries)
    (right : Environment rightEntries) :
    Environment (Row.concat leftEntries rightEntries) :=
  match left with
  | .empty => right
  | .cons value tail => .cons value (append tail right)

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

namespace Environment

/-- Project one required keyed row from a larger available keyed row. -/
class CanProvide
    (available : List Entry)
    (required : List Entry) where
  provide : Environment available -> Environment required

namespace CanProvide

instance : CanProvide available [] where
  provide _ := .empty

instance [Contains entry available] [CanProvide available entries] :
    CanProvide available (entry :: entries) where
  provide environment :=
    .cons (Contains.get environment) (CanProvide.provide environment)

end CanProvide

end Environment

/-- Build a typed environment without knowledge of its final storage order. -/
structure Builder.{u} (entries : List Entry.{u}) where
  environment : Environment entries

namespace Builder

def empty : Builder [] :=
  ⟨Environment.empty⟩

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

def toLayer (layer : KeyedLayer R E entries) :
    Layer R E (Environment entries) :=
  layer.layer

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
syntax "keyed_graph" "(" "error" " := " term ")"
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
          (KeyedLayer.zipFresh $left $right (by decide)) fun $name =>
        keyed_graph (error := $errorType) { $rest* yield $result })
  | `(keyed_graph (error := $errorType:term) {
      let $name:ident := $value:term;
      $rest:keyedGraphBinding*
      yield $result:term
    }) =>
      `(KeyedLayer.shareInto (EOut := $errorType) $value fun $name =>
        keyed_graph (error := $errorType) { $rest* yield $result })

/-!
`service_key entryName : ServiceType` resolves `ServiceType` and uses its full
Lean declaration name. Normal code does not write an owner string.
-/

open Lean Elab Command Term

syntax (name := serviceKeyDecl)
  "service_key " ident " : " ident : command

@[command_elab serviceKeyDecl]
meta def elabServiceKeyDecl : CommandElab
  | `(service_key $entryName:ident : $serviceType:ident) => do
      let serviceName ← liftTermElabM <|
        realizeGlobalConstNoOverloadWithInfo serviceType
      let .str owner localName := serviceName |
        throwErrorAt serviceType
          "a service key requires a named Lean declaration"
      let owner := Syntax.mkStrLit owner.toString
      let localName := Syntax.mkStrLit localName
      elabCommand <| ← `(abbrev $entryName : Entry :=
        Entry.create
          { owner := $owner, name := $localName }
          $serviceType)
  | _ => throwUnsupportedSyntax

/-- Select a high-universe service without returning it as a fiber result. -/
def withServiceZ
    (entry : Entry)
    [Contains entry entries]
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment entries) E A :=
  Z.serviceWithZ fun environment =>
    operation (Contains.get environment)

end StableServiceKeys
