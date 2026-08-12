import Lean
import Z.Layer
import Z.Do

/-!
Stable, normalized service rows and keyed layers.

The public `Z` module imports this implementation. Its declarations are in
the `Z` namespace. Service types receive structural keys, value indices use
stable key witnesses, and normalized rows provide canonical environment
types.
-/

namespace Z

/-- One node in the prefix encoding of a service type. -/
structure KeyPart where
  isValue : Bool
  owner : String
  name : String
  argumentCount : Nat
  deriving BEq, DecidableEq, Repr

namespace KeyPart

/-- Compare key nodes by kind, owner, name, and argument count. -/
@[reducible] def compareKeyPart : KeyPart → KeyPart → Ordering :=
  compareLex
    (compareOn KeyPart.isValue)
    (compareLex
      (compareOn KeyPart.owner)
      (compareLex
        (compareOn KeyPart.name)
        (compareOn KeyPart.argumentCount)))

instance : Ord KeyPart where
  compare := compareKeyPart

instance : Std.TransOrd KeyPart := by
  change Std.TransCmp
    (compareLex
      (compareOn KeyPart.isValue)
      (compareLex
        (compareOn KeyPart.owner)
        (compareLex
          (compareOn KeyPart.name)
          (compareOn KeyPart.argumentCount))))
  infer_instance

instance : Std.LawfulEqOrd KeyPart where
  eq_of_compare {a b} equality := by
    simp only [compare, compareKeyPart, compareLex_eq_eq, compareOn]
      at equality
    obtain ⟨kindEquality, ownerEquality, nameEquality,
      argumentCountEquality⟩ := equality
    change compare a.isValue b.isValue = .eq at kindEquality
    change compare a.owner b.owner = .eq at ownerEquality
    change compare a.name b.name = .eq at nameEquality
    change compare a.argumentCount b.argumentCount = .eq
      at argumentCountEquality
    have kindEquality :=
      Std.LawfulEqOrd.eq_of_compare kindEquality
    have ownerEquality :=
      Std.LawfulEqOrd.eq_of_compare ownerEquality
    have nameEquality :=
      Std.LawfulEqOrd.eq_of_compare nameEquality
    have argumentCountEquality :=
      Std.LawfulEqOrd.eq_of_compare argumentCountEquality
    cases a
    cases b
    simp_all

end KeyPart

/-- A stable prefix encoding of one service type. -/
structure Key where
  parts : List KeyPart
  deriving BEq, DecidableEq, Repr

namespace Key

/-- Compare structural keys by their prefix encodings. -/
@[reducible] def compareKey : Key → Key → Ordering :=
  compareOn Key.parts

instance : Ord Key where
  compare := compareKey

instance : Std.TransOrd Key := by
  change Std.TransCmp (compareOn Key.parts)
  infer_instance

instance : Std.LawfulEqOrd Key where
  eq_of_compare {a b} equality := by
    change compare a.parts b.parts = .eq at equality
    have partsEquality :=
      Std.LawfulEqOrd.eq_of_compare equality
    cases a
    cases b
    simp_all

/-- Build one structural key node from its argument keys. -/
@[reducible] def named
    (owner : String)
    (name : String)
    (arguments : List Key) : Key :=
  ⟨{
      isValue := false
      owner
      name
      argumentCount := arguments.length
    } :: arguments.flatMap (fun argument => argument.parts)⟩

/-- Mark one type key and one value payload as a value argument. -/
@[reducible] def value (type : Key) (payload : Key) : Key :=
  ⟨{
      isValue := true
      owner := ""
      name := ""
      argumentCount := 2
    } :: List.append type.parts payload.parts⟩

end Key

/-- A stable key for a service type that is abstract in generic code. -/
class ServiceKey (Service : Type u) where
  private mk ::
  key : Key

namespace ServiceKey

private abbrev create
    {Service : Type u}
    (key : Key) : ServiceKey Service :=
  ⟨key⟩

end ServiceKey

/-- A stable, injective key function for values used in service types. -/
class ServiceValueKey (Value : Type u) where
  key : Value -> Key

namespace ServiceValueKey

private def textPayload (value : String) : Key :=
  Key.named "" value []

instance : ServiceValueKey Nat where
  key value := textPayload (toString value)

instance : ServiceValueKey Int where
  key value := textPayload (toString value)

instance : ServiceValueKey Bool where
  key
    | false => textPayload "false"
    | true => textPayload "true"

instance : ServiceValueKey Char where
  key value := textPayload (toString value.toNat)

instance : ServiceValueKey String where
  key := textPayload

end ServiceValueKey

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

/-- Compute whether no row entry has this qualified key. -/
def isFresh (key : Key) : List Entry -> Bool
  | [] => true
  | head :: tail =>
      if key = head.key then false else isFresh key tail

/-- Insert one entry in key order. Keep the existing entry for a duplicate. -/
@[reducible] def insert (entry : Entry) : List Entry -> List Entry
  | [] => [entry]
  | head :: tail =>
      if entry.key = head.key then
        head :: tail
      else
        match compare entry.key head.key with
        | .lt =>
            if isFresh entry.key tail then
              entry :: head :: tail
            else
              head :: insert entry tail
        | .eq => head :: insert entry tail
        | .gt => head :: insert entry tail

/-- Give any list of entries one stable order and remove duplicate keys. -/
def normalize (entries : List Entry) : List Entry :=
  entries.foldr insert []

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
      isFresh head.key left && canMerge left tail

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
      if equality : entry.key = head.key then
        simpa [Row.insert, equality] using
          (Environment.cons headValue tailValues)
      else
        cases order : compare entry.key head.key with
        | lt =>
            if tailFresh : Row.isFresh entry.key tail then
              simpa [Row.insert, equality, order, tailFresh] using
                (Environment.cons value
                  (Environment.cons headValue tailValues))
            else
              simpa [Row.insert, equality, order, tailFresh] using
                (Environment.cons headValue
                  (insert entry value tailValues))
        | eq =>
            simpa [Row.insert, equality, order] using
              (Environment.cons headValue
                (insert entry value tailValues))
        | gt =>
            simpa [Row.insert, equality, order] using
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

namespace Environment

/-- The structural position of one exact entry in a service row. -/
inductive Selection.{u}
    (target : Entry.{u}) : List Entry.{u} -> Type (u + 1) where
  | head : Selection target (target :: entries)
  | tail :
      Selection target entries -> Selection target (entry :: entries)

namespace Selection

/-- Read the service value at a structural row position. -/
def get
    (self : Selection target entries)
    (environment : Environment entries) :
    target.Service :=
  match self with
  | .head =>
      match environment with
      | .cons value _ => value
  | .tail position =>
      match environment with
      | .cons _ tailValues => position.get tailValues

end Selection

end Environment

/-- Structural evidence that one exact entry occurs in a row. -/
class Contains (target : Entry) (entries : List Entry) where
  selection : Environment.Selection target entries

namespace Contains

/-- Read the value selected by structural membership evidence. -/
def get
    [self : Contains target entries]
    (environment : Environment entries) :
    target.Service :=
  self.selection.get environment

instance (priority := high) : Contains entry (entry :: entries) where
  selection := .head

instance (priority := low) [tail : Contains target entries] :
    Contains target (entry :: entries) where
  selection := .tail tail.selection

end Contains

namespace Environment

/-- Structural evidence for each service selected from an available row. -/
inductive Projection.{u}
    (available : List Entry.{u}) : List Entry.{u} -> Type (u + 1) where
  | empty : Projection available []
  | cons
      (contains : Contains entry available)
      (tail : Projection available entries) :
      Projection available (entry :: entries)

namespace Projection

/-- Apply structural projection evidence to a typed environment. -/
def provide
    (self : Projection available required)
    (environment : Environment available) :
    Environment required :=
  match self with
  | .empty => .empty
  | .cons contains tail =>
      .cons (contains.get environment) (tail.provide environment)

end Projection

/-- Project one required keyed row from a larger available keyed row. -/
class CanProvide
    (available : List Entry)
    (required : List Entry) where
  projection : Projection available required

namespace CanProvide

def provide
    (self : CanProvide available required)
    (environment : Environment available) :
    Environment required :=
  self.projection.provide environment

instance : CanProvide available [] where
  projection := .empty

instance
    [contains : Contains entry available]
    [tail : CanProvide available entries] :
    CanProvide available (entry :: entries) where
  projection := .cons contains tail.projection

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
Lean declaration names of its constructor and type arguments. An abstract type
argument uses its `ServiceKey` witness. A value argument uses the
`ServiceValueKey` function for its type. Normal code does not write an owner
string or construct a key.
-/

open Lean Meta Elab Command Term
open Parser.Term

syntax (name := serviceKeyDecl)
  "service_key " ident " : " term : command

syntax (name := serviceRowType)
  "ServiceRow[" term,* "]" : term

syntax (name := servicesType)
  "Services[" term,* "]" : term

syntax (name := serviceKeyTerm)
  "serviceKey[" term "]" : term

syntax (name := keyedLayerFromLayerType)
  "KeyedLayer.fromLayer" term:arg : term

syntax (name := keyedLayerSucceedType)
  "KeyedLayer.succeed" term:arg : term

syntax (name := keyedLayerDeriveConstructor)
  "KeyedLayer.derive" term:arg : term

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
  let typeFunction := type.getAppFn
  if typeFunction.isFVar then
    let typeSyntax ← Term.exprToSyntax type
    return ← `(ServiceKey.key (Service := $typeSyntax))
  let .const typeName _ := typeFunction |
    throwErrorAt reference
      "a service key requires a named type constructor or a `ServiceKey` witness"
  let .str owner localName := typeName |
    throwErrorAt reference
      "a service key requires a named Lean declaration"
  let mut argumentKeys : Array (TSyntax `term) := #[]
  for argument in type.getAppArgs do
    let argumentType ← whnf (← inferType argument)
    if argumentType.isSort then
      argumentKeys := argumentKeys.push
        (← keySyntaxForType reference argument)
    else
      unless ← isType argumentType do
        throwErrorAt reference
          "a service value argument must have a type"
      let argumentSyntax ← PrettyPrinter.delab argument
      let argumentTypeSyntax ← PrettyPrinter.delab argumentType
      let typeKey ← keySyntaxForType reference argumentType
      let payload ← `(ServiceValueKey.key
        (Value := $argumentTypeSyntax) $argumentSyntax)
      argumentKeys := argumentKeys.push
        (← `(Key.value $typeKey $payload))
  let owner := Syntax.mkStrLit <| match owner with
    | .anonymous => ""
    | owner => owner.toString
  let localName := Syntax.mkStrLit localName
  `(Key.named $owner $localName [$argumentKeys,*])

@[term_elab serviceKeyTerm]
def elabServiceKeyTerm : TermElab := fun stx expectedType? => do
  let `(serviceKey[$serviceType]) := stx | throwUnsupportedSyntax
  let serviceTypeExpr ← Term.elabType serviceType
  let key ← keySyntaxForType serviceType
    (← instantiateMVars serviceTypeExpr)
  let generated ←
    `(ServiceKey.create (Service := $serviceType) $key)
  Term.elabTerm generated expectedType?

private def deriveServiceKey
    (declarationNames : Array Name) : CommandElabM Bool := do
  let #[typeName] := declarationNames | return false
  let inductiveValue ← getConstInfoInduct typeName
  if inductiveValue.all.length != 1 then
    throwError "mutually inductive service-key derivation is not supported"
  elabCommand <| ← liftTermElabM do
    forallTelescopeReducing inductiveValue.type fun parameters _ => do
      let mut localContext ← getLCtx
      for index in [0:parameters.size] do
        let parameter := parameters[index]!
        let userName := Name.mkSimple s!"serviceKeyParameter{index}"
        localContext := localContext.setUserName
          parameter.fvarId! userName
      withLCtx' localContext do
        let mut instanceBinders := #[]
        let mut valueTypes : Array Expr := #[]
        for parameter in parameters do
          if ← isType parameter then
            let parameterName :=
              mkIdent (← getFVarLocalDecl parameter).userName
            let binder ←
              `(bracketedBinderF| [ServiceKey $parameterName:ident])
            instanceBinders := instanceBinders.push binder
          else
            let parameterType ← whnf (← inferType parameter)
            unless ← isType parameterType do
              throwError
                "a service value parameter must have a type"
            let mut alreadyRequired := false
            for existing in valueTypes do
              if ← isDefEq existing parameterType then
                alreadyRequired := true
            unless alreadyRequired do
              let parameterTypeSyntax ← PrettyPrinter.delab parameterType
              let binder ←
                `(bracketedBinderF|
                  [ServiceValueKey $parameterTypeSyntax])
              instanceBinders := instanceBinders.push binder
              valueTypes := valueTypes.push parameterType
        let parameterNames ← parameters.mapM fun parameter =>
          return mkIdent (← getFVarLocalDecl parameter).userName
        let serviceType ←
          `(@$(mkCIdent typeName) $parameterNames*)
        `(variable $instanceBinders* in
          instance : ServiceKey $serviceType := serviceKey[$serviceType])
  return true

initialize
  registerDerivingHandler ``ServiceKey deriveServiceKey

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

/-- Reject a stable-key collision before normalization can hide it. -/
private def ensureCompatibleEntries
    (reference : Syntax)
    (entries : Array Expr) : TermElabM Unit := do
  for leftPosition in [0:entries.size] do
    for rightPosition in [leftPosition + 1:entries.size] do
      let left := entries[leftPosition]!
      let right := entries[rightPosition]!
      let leftKey ← mkAppM ``Entry.key #[left]
      let rightKey ← mkAppM ``Entry.key #[right]
      if ← isDefEq leftKey rightKey then
        unless ← isDefEq left right do
          let leftService ← whnf (← mkAppM ``Entry.Service #[left])
          let rightService ← whnf (← mkAppM ``Entry.Service #[right])
          throwErrorAt reference m!
            "service types {leftService} and {rightService} have the same stable key"

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
  ensureCompatibleEntries reference entries
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

private structure DerivedConstructor where
  constructor : Term
  dependencies : Array Term
  output : Term

private def analyzeDerivedConstructor
    (reference : Syntax)
    (constructor : Expr) : TermElabM DerivedConstructor := do
  let constructorType ← inferType constructor
  let (arguments, binderInfos, result) ←
    forallMetaTelescopeReducing constructorType
  let mut dependencies : Array Term := #[]
  for index in [0:arguments.size] do
    unless binderInfos[index]!.isExplicit do continue
    let dependency ← instantiateMVars (← inferType arguments[index]!)
    unless ← isType dependency do
      throwErrorAt reference m!
        "constructor parameter {dependency} is not a service type"
    if dependency.hasExprMVar then
      throwErrorAt reference
        "could not infer a constructor parameter type; apply all type parameters before `KeyedLayer.derive`"
    dependencies := dependencies.push
      (← Term.exprToSyntax dependency)
  let output ← instantiateMVars result
  unless ← isType output do
    throwErrorAt reference m!
      "the constructor result {output} is not a service type"
  if output.hasExprMVar then
    throwErrorAt reference
      "could not infer the constructed service type; apply all type parameters before `KeyedLayer.derive`"
  return {
    constructor := ← Term.exprToSyntax constructor
    dependencies
    output := ← Term.exprToSyntax output
  }

private def elaborateAnalyzedConstructor
    (reference : Syntax)
    (analyzed : DerivedConstructor)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let environment :=
    mkIdentFrom reference `_keyedLayerDeriveEnvironment
  let mut constructed := analyzed.constructor
  for dependency in analyzed.dependencies do
    let argument ← `(Services.get[$dependency] $environment)
    constructed ← `($constructed $argument)
  let generated ←
    `(KeyedLayer.fromLayer
      (show Layer
          (Services[$(analyzed.dependencies),*])
          Empty
          $(analyzed.output) from
        Layer.fromFunction fun $environment => $constructed))
  Term.elabTerm generated expectedType?

private def elaborateDerivedConstructor
    (reference : Syntax)
    (constructor : Expr)
    (expectedType? : Option Expr) : TermElabM Expr := do
  elaborateAnalyzedConstructor reference
    (← analyzeDerivedConstructor reference constructor)
    expectedType?

private def elaborateDerivedStructure
    (reference : Syntax)
    (serviceTypeSyntax : Term)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let serviceType ← whnf (← Term.elabType serviceTypeSyntax)
  let .const serviceName levels := serviceType.getAppFn |
    throwErrorAt reference
      "`KeyedLayer.derive[Service]` requires a structure type"
  unless isStructure (← getEnv) serviceName do
    throwErrorAt reference m!
      "{serviceType} is not a structure type"
  let inductiveValue ← getConstInfoInduct serviceName
  let constructorName := inductiveValue.ctors[0]!
  let parameters :=
    serviceType.getAppArgs.extract 0 inductiveValue.numParams
  let constructor := mkAppN (mkConst constructorName levels) parameters
  let constructorResult ← analyzeDerivedConstructor reference constructor
  let resultType ← Term.elabType constructorResult.output
  unless ← isDefEq resultType serviceType do
    throwErrorAt reference m!
      "the structure constructor produces {resultType}, not {serviceType}"
  elaborateAnalyzedConstructor reference constructorResult expectedType?

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

@[term_elab keyedLayerDeriveConstructor]
def elabKeyedLayerDeriveConstructor : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.derive $constructorSyntax) := stx |
    throwUnsupportedSyntax
  -- The argument parser represents `[Service]` as a singleton bracketed term.
  -- Handle this form before it can elaborate as a list value.
  match constructorSyntax with
  | `([$serviceType]) =>
      elaborateDerivedStructure stx serviceType expectedType?
  | _ =>
      let constructor ← Term.elabTerm constructorSyntax none
      Term.synthesizeSyntheticMVarsNoPostponing
      elaborateDerivedConstructor stx constructor expectedType?

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

end Z
