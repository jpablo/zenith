import Zenith.Formalization.ErrorShape
import Zenith.Formalization.ServiceRowConnection

/-!
Checked type-level laws for the production `Z` variance and composition API.
The definitions in this file are elaboration checks with explicit result
types. They do not model interpreter execution.
-/

namespace Zenith.Formalization.VarianceLaws

/-- `Z.adapt` implements contravariant environment and covariant error and success changes. -/
def adapt
    (environment : R₀ -> R₁)
    (error : E₀ -> E₁)
    (success : A₀ -> A₁)
    (effect : Z R₁ E₀ A₀) : Z R₀ E₁ A₁ :=
  Z.adapt environment error success effect

/-- `Z.widen` implements the environment/error part of the variance rule. -/
def widen
    [environment : _root_.Environment.CanProvide R₀ R₁]
    [error : E₀ <: E₁]
    (effect : Z R₁ E₀ A) : Z R₀ E₁ A :=
  Z.widen effect

/-- The environment-focused `CoeTC` instance has the contravariant direction. -/
def coerceEnvironment
    [environment : _root_.Environment.CanProvide R₀ R₁]
    (effect : Z R₁ E A) : Z R₀ E A :=
  effect

/-- The error-focused `CoeTC` instance has the covariant direction. -/
def coerceError
    [error : E₀ <: E₁]
    (effect : Z R E₀ A) : Z R E₁ A :=
  effect

/-- The success-focused `CoeTC` instance has the covariant direction. -/
def coerceSuccess
    [success : A₀ <: A₁]
    (effect : Z R E A₀) : Z R E A₁ :=
  effect

/-- The combined `CoeTC` instance implements all three variance premises. -/
def coerce
    [environment : _root_.Environment.CanProvide R₀ R₁]
    [error : E₀ <: E₁]
    [success : A₀ <: A₁]
    (effect : Z R₁ E₀ A₀) : Z R₀ E₁ A₁ :=
  effect

/-- Heterogeneous sequencing computes both the environment meet and error join. -/
def flatMapMeetJoin
    [meet : _root_.Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (effect : Z R₁ E₁ A)
    (next : A -> Z R₂ E₂ B) : Z R E B :=
  Z.flatMapMeetJoin effect next

/--
Use abstract row provision to widen a keyed environment effect.

The definition is noncomputable for the same reason as `chooseProjection`:
the abstract proposition contains no runtime row position. It is a formal
bridge, not a replacement for Zenith's ordinary structural instances.
-/
noncomputable def widenFromRowProof
    (available required : List Z.Entry)
    (provides : ServiceRows.Provides available required)
    (coherent : Z.Row.Coherent (available ++ required))
    (effect : Z (Z.Environment required) E A) :
    Z (Z.Environment available) E A := by
  letI : Z.Environment.CanProvide available required := {
    projection := ServiceRows.chooseProjection available required provides coherent
  }
  exact Z.widen effect

/-- Inject the left checked error shape into its explicit nested-`Sum` union. -/
noncomputable def widenShapeLeft
    (left right : ErrorShape.Shape)
    (effect : Z R (ErrorShape.interpret left) A) :
    Z R (ErrorShape.interpret (.sum left right)) A := by
  letI : ErrorChannel.CanInject
      (ErrorShape.interpret left) (ErrorShape.interpret (.sum left right)) :=
    Classical.choice (ErrorShape.canInjectLeft left right)
  exact Z.widenWithErrorInjection effect

/-- Inject the right checked error shape into its explicit nested-`Sum` union. -/
noncomputable def widenShapeRight
    (left right : ErrorShape.Shape)
    (effect : Z R (ErrorShape.interpret right) A) :
    Z R (ErrorShape.interpret (.sum left right)) A := by
  letI : ErrorChannel.CanInject
      (ErrorShape.interpret right) (ErrorShape.interpret (.sum left right)) :=
    Classical.choice (ErrorShape.canInjectRight left right)
  exact Z.widenWithErrorInjection effect

end Zenith.Formalization.VarianceLaws
