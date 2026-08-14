import Z.Environment
import Z.Syntax.Do
import Zenith.Services.Model

/-!
Typed keyed service environments and their structural projections.
-/

namespace Z

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

/-- Project the required row from an available row using this evidence. -/
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

end Z
