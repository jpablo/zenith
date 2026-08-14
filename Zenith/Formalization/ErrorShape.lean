import Z
import Zenith.Formalization.TypeAlgebra

/-!
The checked syntax used to describe Zenith's public nested-`Sum` error
representation. This is not a replacement for arbitrary Lean error types:
the `zdo` elaborator still decides their canonical order. It gives the
representation a small kernel-checked model.
-/

namespace Zenith.Formalization.ErrorShape

open TypeAlgebra

/-- A reified nested-`Sum` error channel with ordinary Lean error types at its leaves. -/
inductive Shape : Type 1 where
  /-- The impossible error channel. -/
  | empty
  /-- One concrete Lean error type. -/
  | leaf (error : Type)
  /-- The tagged union of two error shapes. -/
  | sum (left right : Shape)

/-- Interpret a checked shape as Zenith's public nested-`Sum` error type. -/
def interpret : Shape -> Type
  | .empty => Empty
  | .leaf error => error
  | .sum left right => Sum (interpret left) (interpret right)

/-- Translate a checked shape into the abstract error algebra. -/
def toAlgebra : Shape -> ErrorType Type
  | .empty => .nothing
  | .leaf error => .failure error
  | .sum left right => .or (toAlgebra left) (toAlgebra right)

/-- A concrete error type occurs at one leaf of a checked error shape. -/
def Occurs (error : Type) : Shape -> Prop
  | .empty => False
  | .leaf candidate => error = candidate
  | .sum left right => Occurs error left ∨ Occurs error right

@[simp]
theorem allows_toAlgebra
    {error : Type}
    {shape : Shape} :
    ErrorType.Allows error (toAlgebra shape) ↔ Occurs error shape := by
  induction shape with
  | empty => simp [toAlgebra, Occurs, ErrorType.Allows]
  | leaf error => simp [toAlgebra, Occurs, ErrorType.Allows]
  | sum left right leftHypothesis rightHypothesis =>
      simp [toAlgebra, Occurs, ErrorType.Allows, leftHypothesis, rightHypothesis]

/-- The empty checked shape has Zenith's impossible error type. -/
theorem interpret_empty : interpret .empty = Empty := rfl

/-- A leaf keeps its original Lean error type. -/
theorem interpret_leaf (error : Type) : interpret (.leaf error) = error := rfl

/-- A checked union is interpreted as the public nested `Sum` representation. -/
theorem interpret_sum (left right : Shape) :
    interpret (.sum left right) = Sum (interpret left) (interpret right) := rfl

/-- The left branch injects into the nested sum represented by the shape. -/
theorem canInjectLeft (left right : Shape) :
    Nonempty (ErrorChannel.CanInject (interpret left) (interpret (.sum left right))) :=
  ⟨{ inject := Sum.inl }⟩

/-- The right branch injects into the nested sum represented by the shape. -/
theorem canInjectRight (left right : Shape) :
    Nonempty (ErrorChannel.CanInject (interpret right) (interpret (.sum left right))) :=
  ⟨{ inject := Sum.inr }⟩

/-- A checked union is a production `ErrorChannel.Join` upper-bound witness. -/
theorem joinUpperBound (left right : Shape) :
    Nonempty (ErrorChannel.Join
      (interpret left) (interpret right) (interpret (.sum left right))) :=
  ⟨{ left := Sum.inl, right := Sum.inr }⟩

/-- Abstract error union agrees exactly with the checked-shape translation. -/
theorem toAlgebra_sum (left right : Shape) :
    toAlgebra (.sum left right) = ErrorType.or (toAlgebra left) (toAlgebra right) := rfl

end Zenith.Formalization.ErrorShape
