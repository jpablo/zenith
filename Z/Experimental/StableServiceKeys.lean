import Lean
import Z

/-!
Experimental stable, normalized service rows.

This module is not imported by the public `Z` module. It tests a possible
replacement for product environments without changing the production API.
-/

namespace StableServiceKeys

/-- One node in the prefix encoding of a service type. -/
structure KeyPart where
  owner : String
  name : String
  argumentCount : Nat
  deriving BEq, DecidableEq, Ord, Repr

/-- A stable prefix encoding of one concrete service type. -/
structure Key where
  parts : List KeyPart
  deriving BEq, DecidableEq, Ord, Repr

namespace Key

/-- Build one structural key node from its argument keys. -/
@[reducible] def named
    (owner : String)
    (name : String)
    (arguments : List Key) : Key :=
  ⟨{
      owner
      name
      argumentCount := arguments.length
    } :: arguments.flatMap (fun argument => argument.parts)⟩

end Key

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
@[zdo_row_environment Row.normalize]
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

/-- Let ordinary `Z` combinators project one keyed environment row. -/
instance (priority := high)
    [projection : Environment.CanProvide available required] :
    _root_.Environment.CanProvide
      (Environment available)
      (Environment required) where
  provide := projection.provide

/-- Find and project a keyed row in the left side of a product environment. -/
instance (priority := high)
    [projection : _root_.Environment.CanProvide
      left (Environment required)] :
    _root_.Environment.CanProvide
      (left × right)
      (Environment required) where
  provide environment := projection.provide environment.1

/-- Find and project a keyed row in the right side of a product environment. -/
instance (priority := high)
    [projection : _root_.Environment.CanProvide
      right (Environment required)] :
    _root_.Environment.CanProvide
      (left × right)
      (Environment required) where
  provide environment := projection.provide environment.2

namespace Services

/-- The empty keyed service environment. -/
def empty : Environment ([] : List Entry.{u}) :=
  .empty

end Services

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
                  match ← fiber.awaitPoll (fiberId := fiber.fiberId) with
                  | some exit => callback (.ok exit)
                  | none => callback (.error (.die <| IO.userError
                      "the provided program did not return an exit value"))
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
def withServiceZ
    (entry : Entry)
    [Contains entry entries]
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment entries) E A :=
  Z.serviceWithZ fun environment =>
    operation (Contains.get environment)

/-- Select a low-universe result from one inferred service entry. -/
def withServiceEntry
    (entry : Entry)
    (operation : entry.Service -> A) :
    Z (Environment [entry]) Empty A :=
  withService (entries := [entry]) entry operation

/-- Run an effect with one inferred high-universe service entry. -/
def withServiceZEntry
    (entry : Entry)
    (operation : entry.Service -> Z Unit E A) :
    Z (Environment [entry]) E A :=
  withServiceZ (entries := [entry]) entry operation

/-!
`service_key entryName : ServiceType` resolves `ServiceType` and uses the full
Lean declaration names of its constructor and concrete type arguments. Normal
code does not write an owner string or construct a key.
-/

open Lean Meta Elab Command Term

syntax (name := serviceKeyDecl)
  "service_key " ident " : " term : command

syntax (name := serviceRowType)
  "ServiceRow[" term,* "]" : term

syntax (name := servicesType)
  "Services[" term,* "]" : term

syntax (name := keyedLayerFromLayerType)
  "KeyedLayer.fromLayer" term:arg : term

syntax (name := keyedLayerSucceedType)
  "KeyedLayer.succeed" term:arg : term

syntax (name := keyedServiceWithType)
  "Z.serviceWithType" "(" term ")" term:arg : term

syntax (name := keyedServiceWithZType)
  "Z.serviceWithZType" "(" term ")" term:arg : term

syntax (name := servicesGetType)
  "Services.get" "[" term "]" term:arg : term

macro_rules
  | `(Z.serviceWith[$serviceType] $operation) =>
      `(Z.serviceWithType ($serviceType) $operation)
  | `(Z.serviceWithZ[$serviceType] $operation) =>
      `(Z.serviceWithZType ($serviceType) $operation)

private partial def keySyntaxForType
    (reference : Syntax)
    (type : Expr) : TermElabM (TSyntax `term) := do
  let type ← whnf (← instantiateMVars type)
  let .const typeName _ := type.getAppFn |
    throwErrorAt reference
      "a service key requires a named type constructor"
  let .str owner localName := typeName |
    throwErrorAt reference
      "a service key requires a named Lean declaration"
  let mut argumentKeys : Array (TSyntax `term) := #[]
  for argument in type.getAppArgs do
    let argumentType ← whnf (← inferType argument)
    let .sort _ := argumentType |
      throwErrorAt reference
        "a service key currently supports only type arguments"
    argumentKeys := argumentKeys.push
      (← keySyntaxForType reference argument)
  let owner := Syntax.mkStrLit <| match owner with
    | .anonymous => ""
    | owner => owner.toString
  let localName := Syntax.mkStrLit localName
  `(Key.named $owner $localName [$argumentKeys,*])

private def entrySyntaxForType
    (reference : Syntax)
    (serviceType : Term) : TermElabM Term := do
  let serviceTypeExpr ← Term.elabType serviceType
  let key ← keySyntaxForType reference
    (← instantiateMVars serviceTypeExpr)
  `(Entry.create $key $serviceType)

private def entrySyntaxForTypeExpr
    (reference : Syntax)
    (serviceType : Expr) : TermElabM Term := do
  let serviceType ← instantiateMVars serviceType
  let serviceTypeSyntax ← Term.exprToSyntax serviceType
  let key ← keySyntaxForType reference
    serviceType
  `(Entry.create $key $serviceTypeSyntax)

private partial def serviceRowEntries
    (reference : Syntax)
    (row : Expr) : TermElabM (Array Expr) := do
  let row ← whnf row
  if row.getAppFn.isConstOf ``List.nil then
    return #[]
  if row.getAppFn.isConstOf ``List.cons then
    let arguments := row.getAppArgs
    unless arguments.size == 3 do
      throwErrorAt reference "invalid service row"
    return #[arguments[1]!] ++
      (← serviceRowEntries reference arguments[2]!)
  throwErrorAt reference "a service row must reduce to a list"

private def elaborateServiceRow
    (reference : Syntax)
    (serviceTypes : Array Term) : TermElabM Expr := do
  let mut entries := #[]
  for serviceType in serviceTypes do
    let entrySyntax ← entrySyntaxForType serviceType serviceType
    let entry ← Term.elabTerm entrySyntax none
    entries := entries.push (← instantiateMVars entry)
  let entryType ←
    if entries.isEmpty then
      pure <| Lean.mkConst ``Entry [← mkFreshLevelMVar]
    else
      inferType entries[0]!
  for entry in entries do
    unless ← isDefEq (← inferType entry) entryType do
      throwErrorAt reference
        "all services in one row must use the same universe"
  let row ← mkListLit entryType entries.toList
  let normalized ← mkAppM ``Row.normalize #[row]
  let normalizedEntries ← serviceRowEntries reference normalized
  mkListLit entryType normalizedEntries.toList

private def ensureExpectedType
    (expectedType? : Option Expr)
    (expression : Expr) : TermElabM Expr :=
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType expression
  | none => pure expression

private structure ExpectedSingleServiceLayer where
  input : Expr
  errorType : Expr
  service : Expr

private def expectedSingleServiceLayer?
    (expectedType? : Option Expr) :
    TermElabM (Option ExpectedSingleServiceLayer) := do
  let some expectedType := expectedType? | return none
  let expectedType ← instantiateMVars expectedType
  if ← hasAssignableMVar expectedType then return none
  let expectedType ← whnf expectedType
  unless expectedType.isAppOfArity ``KeyedLayer 3 do return none
  let arguments := expectedType.getAppArgs
  let row ← whnf arguments[2]!
  unless row.getAppFn.isConstOf ``List.cons do return none
  let rowArguments := row.getAppArgs
  unless rowArguments.size == 3 do return none
  let tail ← whnf rowArguments[2]!
  unless tail.getAppFn.isConstOf ``List.nil do return none
  let service ← mkAppM ``Entry.Service #[rowArguments[1]!]
  return some {
    input := arguments[0]!
    errorType := arguments[1]!
    service
  }

@[term_elab serviceRowType]
def elabServiceRowType : TermElab := fun stx expectedType? => do
  let `(ServiceRow[$serviceTypes,*]) := stx | throwUnsupportedSyntax
  let row ← elaborateServiceRow stx serviceTypes.getElems
  ensureExpectedType expectedType? row

@[term_elab servicesType]
def elabServicesType : TermElab := fun stx expectedType? => do
  let `(Services[$serviceTypes,*]) := stx | throwUnsupportedSyntax
  let row ← elaborateServiceRow stx serviceTypes.getElems
  let environment ← mkAppM ``Environment #[row]
  ensureExpectedType expectedType? environment

@[term_elab keyedLayerFromLayerType]
def elabKeyedLayerFromLayerType : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.fromLayer $layer) := stx |
    throwUnsupportedSyntax
  let expectedLayer? ← expectedSingleServiceLayer? expectedType?
  let layerExpectedType? ← expectedLayer?.mapM fun expected =>
    mkAppM ``Layer #[expected.input, expected.errorType, expected.service]
  let layer ← Term.elabTerm layer layerExpectedType?
  let layerType ← whnf (← inferType layer)
  unless layerType.isAppOfArity ``Layer 3 do
    throwErrorAt stx
      "`KeyedLayer.fromLayer` requires an ordinary `Layer` value"
  let serviceType := layerType.getAppArgs[2]!
  let entrySyntax ← entrySyntaxForTypeExpr stx serviceType
  let entry ← Term.elabTerm entrySyntax none
  let result ← mkAppM ``KeyedLayer.singleton #[entry, layer]
  ensureExpectedType expectedType? result

@[term_elab keyedLayerSucceedType]
def elabKeyedLayerSucceedType : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.succeed $value) := stx |
    throwUnsupportedSyntax
  let expectedLayer? ← expectedSingleServiceLayer? expectedType?
  let value ← Term.elabTerm value (expectedLayer?.map (·.service))
  let serviceType ← inferType value
  let entrySyntax ← entrySyntaxForTypeExpr stx serviceType
  let entry ← Term.elabTerm entrySyntax none
  let result ← mkAppM ``KeyedLayer.succeedEntry #[entry, value]
  ensureExpectedType expectedType? result

@[term_elab keyedServiceWithType]
def elabKeyedServiceWithType : TermElab := fun stx expectedType? => do
  let `(Z.serviceWithType ($serviceType) $operation) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ← `(withServiceEntry $entry $operation)
  match expectedType? with
  | some expectedType =>
      if ← hasAssignableMVar expectedType then
        Term.elabTerm generated none
      else
        Term.elabTerm generated expectedType?
  | none => Term.elabTerm generated none

@[term_elab keyedServiceWithZType]
def elabKeyedServiceWithZType : TermElab := fun stx expectedType? => do
  let `(Z.serviceWithZType ($serviceType) $operation) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ← `(withServiceZEntry $entry $operation)
  match expectedType? with
  | some expectedType =>
      if ← hasAssignableMVar expectedType then
        Term.elabTerm generated none
      else
        Term.elabTerm generated expectedType?
  | none => Term.elabTerm generated none

@[term_elab servicesGetType]
def elabServicesGetType : TermElab := fun stx expectedType? => do
  let `(Services.get[$serviceType] $environment) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ←
    `(Contains.get (target := $entry) $environment)
  Term.elabTerm generated expectedType?

@[command_elab serviceKeyDecl]
meta def elabServiceKeyDecl : CommandElab
  | `(service_key $entryName:ident : $serviceType:term) => do
      let entry ← liftTermElabM <|
        entrySyntaxForType serviceType serviceType
      elabCommand <| ← `(abbrev $entryName : Entry := $entry)
  | _ => throwUnsupportedSyntax

end StableServiceKeys
