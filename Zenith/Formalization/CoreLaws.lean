import Z

/-!
Pure laws for core Zenith values.

These theorems replace regression tests that only evaluate recursive data
functions or type-directed environment projections. They do not model the
interpreter, fibers, or Lean `IO` runtime.
-/

namespace Zenith.Formalization

namespace CauseLaws

/-- Mapping a typed failure applies the function to its error value. -/
theorem map_fail (f : E -> E₁) (error : E) :
    Cause.map f (.fail error) = .fail (f error) :=
  rfl

/-- Mapping a defect leaves its `IO.Error` unchanged. -/
theorem map_die (f : E -> E₁) (error : IO.Error) :
    Cause.map f (.die error : Cause E) = .die error :=
  rfl

/-- Mapping a cause preserves interruption. -/
theorem map_interrupt (f : E -> E₁) :
    Cause.map f (.interrupt : Cause E) = .interrupt :=
  rfl

/-- Mapping a sequential cause maps both branches in order. -/
theorem map_sequential
    (f : E -> E₁)
    (left right : Cause E) :
    Cause.map f (.sequential left right) =
      .sequential (Cause.map f left) (Cause.map f right) :=
  rfl

/-- Mapping a parallel cause maps both branches without changing its shape. -/
theorem map_parallel
    (f : E -> E₁)
    (left right : Cause E) :
    Cause.map f (.parallel left right) =
      .parallel (Cause.map f left) (Cause.map f right) :=
  rfl

/-- A typed failure is the first failure contained in itself. -/
theorem failureOption_fail (error : E) :
    Cause.failureOption (.fail error) = some error :=
  rfl

/-- A defect does not contain a typed failure. -/
theorem failureOption_die (error : IO.Error) :
    Cause.failureOption (.die error : Cause E) = none :=
  rfl

/-- Interruption does not contain a typed failure. -/
theorem failureOption_interrupt :
    Cause.failureOption (.interrupt : Cause E) = none :=
  rfl

/-- Sequential causes search the left branch before the right branch. -/
theorem failureOption_sequential (left right : Cause E) :
    Cause.failureOption (.sequential left right) =
      match Cause.failureOption left with
      | some error => some error
      | none => Cause.failureOption right :=
  rfl

/-- Parallel causes use the same deterministic left-first failure selection. -/
theorem failureOption_parallel (left right : Cause E) :
    Cause.failureOption (.parallel left right) =
      match Cause.failureOption left with
      | some error => some error
      | none => Cause.failureOption right :=
  rfl

/-- A typed failure is returned directly by `failureOrCause`. -/
theorem failureOrCause_fail (error : E) :
    Cause.failureOrCause (R := R) (.fail error) = .inl error :=
  rfl

/-- A defect is retained as a remaining cause by `failureOrCause`. -/
theorem failureOrCause_die (error : IO.Error) :
    Cause.failureOrCause (R := R) (.die error : Cause E) =
      .inr (.die error) :=
  rfl

/-- Interruption is retained as a remaining cause by `failureOrCause`. -/
theorem failureOrCause_interrupt :
    Cause.failureOrCause (E := E) (R := R) .interrupt = .inr .interrupt :=
  rfl

/-- `Cause.show` has a stable representation for interruption. -/
theorem show_interrupt [ToString E] :
    Cause.show (.interrupt : Cause E) = "Cause.interrupt" :=
  rfl

/-- `Cause.show` renders sequential composition from its rendered branches. -/
theorem show_sequential [ToString E] (left right : Cause E) :
    Cause.show (.sequential left right) =
      s!"Cause.sequential ({left.show}, {right.show})" :=
  rfl

/-- `Cause.show` renders parallel composition from its rendered branches. -/
theorem show_parallel [ToString E] (left right : Cause E) :
    Cause.show (.parallel left right) =
      s!"Cause.parallel ({left.show}, {right.show})" :=
  rfl

end CauseLaws

namespace ExitLaws

/-- Rendering a successful exit hides its successful value. -/
theorem show_success [ToString E] (value : A) :
    Exit.show (.success value : Exit E A) = "Exit.success (...)" :=
  rfl

/-- Rendering a failed exit delegates to the contained cause. -/
theorem show_failure [ToString E] (cause : Cause E) :
    Exit.show (.failure cause : Exit E A) =
      s!"Exit.failure ({cause})" :=
  rfl

/-- Equal successful exits have equal successful values. -/
theorem success_injective
    {left right : A}
    (equality : (.success left : Exit E A) = .success right) :
    left = right := by
  injection equality

/-- A successful exit cannot equal a failed exit. -/
theorem success_ne_failure (value : A) (cause : Cause E) :
    (.success value : Exit E A) ≠ .failure cause := by
  intro equality
  cases equality

/-- A failed exit cannot equal a successful exit. -/
theorem failure_ne_success (cause : Cause E) (value : A) :
    (.failure cause : Exit E A) ≠ .success value := by
  intro equality
  cases equality

end ExitLaws

namespace InterruptStatusLaws

/-- The interruptible status converts to `true`. -/
theorem interruptible_toBool :
    InterruptStatus.interruptible.toBool = true :=
  rfl

/-- The uninterruptible status converts to `false`. -/
theorem uninterruptible_toBool :
    InterruptStatus.uninterruptible.toBool = false :=
  rfl

/-- The interruptible status has stable diagnostic text. -/
theorem show_interruptible :
    toString InterruptStatus.interruptible = "interruptible" :=
  rfl

/-- The uninterruptible status has stable diagnostic text. -/
theorem show_uninterruptible :
    toString InterruptStatus.uninterruptible = "uninterruptible" :=
  rfl

end InterruptStatusLaws

namespace EnvironmentLaws

/-- Mapping a provider applies the mapping function after the provider. -/
theorem canProvide_map_apply
    (provider : Environment.CanProvide Available Required)
    (f : Required -> Provided)
    (environment : Available) :
    (provider.map f).provide environment = f (provider.provide environment) :=
  rfl

/-- Product-environment projection can reorder independently available services. -/
theorem reordered_projection :
    (inferInstance : Environment.CanProvide
      (Char × String × Nat) (Nat × String)).provide ('x', "value", 42) =
      (42, "value") :=
  rfl

/-- A duplicate requirement consumes two distinct matching product positions. -/
theorem duplicate_projection :
    (inferInstance : Environment.CanProvide
      (String × String × Nat) (String × String)).provide
        ("first", "second", 0) =
      ("first", "second") :=
  rfl

/-- A mapped provider derives a new service from the selected service. -/
theorem mapped_projection :
    (Environment.CanProvide.map
      (inferInstance : Environment.CanProvide (Char × Nat) Nat) toString).provide
        ('x', 7) = "7" :=
  rfl

end EnvironmentLaws

end Zenith.Formalization
