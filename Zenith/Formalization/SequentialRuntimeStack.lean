import Zenith.Formalization.SequentialMachine

/-!
A structural connection from the verified pure sequential stack to the
production interpreter stack. This module does not execute `IO` or prove the
production run loop. It proves that the two stack representations have the
same continuation-frame shape for the pure sequential subset.
-/

namespace Zenith.Formalization.SequentialRuntimeStack

open SequentialCore SequentialMachine

/-- A production stack with an existential next-continuation result type. -/
inductive RuntimeStack (E A : Type) : Type 1 where
  | pack (stack : _root_.Stack E A E₁ A₁) : RuntimeStack E A

/-- The number of production continuation frames in an existential stack. -/
def runtimeStackSize : RuntimeStack E A -> Nat
  | .pack stack => _root_.Stack.size stack

/--
The pure stack corresponds to a production stack when it has the same
continuation frames, compiled through `SequentialCore.toZCore`.

The production stack indexes only its immediate continuation. `RuntimeStack`
therefore hides that index while retaining the complete stack value.
-/
inductive Corresponds
    (complete : Observer E₁ A₁) :
    SequentialMachine.Stack E A E₁ A₁ -> RuntimeStack E A -> Prop where
  | done :
      Corresponds complete .done (.pack (.done complete))
  | flatMap
      (next : A -> Program R E B)
      (environment : R)
      (tail : SequentialMachine.Stack E B E₁ A₁)
      (runtimeTail : _root_.Stack E B E₂ A₂) :
      Corresponds complete tail (.pack runtimeTail) ->
      Corresponds complete (.flatMap next environment tail)
        (.pack (.more
          (fun value => toZCore (next value))
          none
          (some (.up rfl))
          runtimeTail
          none
          inferInstance
          environment))
  | foldCauseM
      (errorHandler : Cause E -> Program R EHandler B)
      (next : A -> Program R EHandler B)
      (environment : R)
      (tail : SequentialMachine.Stack EHandler B E₁ A₁)
      (runtimeTail : _root_.Stack EHandler B E₃ A₃) :
      Corresponds complete tail (.pack runtimeTail) ->
      Corresponds complete (.foldCauseM errorHandler next environment tail)
        (.pack (.more
          (fun value => toZCore (next value))
          (some fun cause => toZCore (errorHandler cause))
          none
          runtimeTail
          none
          inferInstance
          environment))

/-- Count continuation frames in the verified pure stack. -/
def stackSize : SequentialMachine.Stack E A E₁ A₁ -> Nat
  | .done => 0
  | .flatMap _ _ tail => 1 + stackSize tail
  | .foldCauseM _ _ _ tail => 1 + stackSize tail

/-- Every verified pure stack has a structurally corresponding production stack. -/
theorem exists_corresponding_runtimeStack
    (stack : SequentialMachine.Stack E A E₁ A₁)
    (complete : Observer E₁ A₁) :
    ∃ runtimeStack, Corresponds complete stack runtimeStack := by
  induction stack with
  | done => exact ⟨.pack (.done complete), .done⟩
  | flatMap next environment tail inductionHypothesis =>
      obtain ⟨runtimeTail, correspondence⟩ := inductionHypothesis complete
      rcases runtimeTail with ⟨runtimeTail⟩
      exact ⟨
        .pack (.more
          (fun value => toZCore (next value))
          none
          (some (.up rfl))
          runtimeTail
          none
          inferInstance
          environment),
        .flatMap next environment tail runtimeTail correspondence
      ⟩
  | foldCauseM errorHandler next environment tail inductionHypothesis =>
      obtain ⟨runtimeTail, correspondence⟩ := inductionHypothesis complete
      rcases runtimeTail with ⟨runtimeTail⟩
      exact ⟨
        .pack (.more
          (fun value => toZCore (next value))
          (some fun cause => toZCore (errorHandler cause))
          none
          runtimeTail
          none
          inferInstance
          environment),
        .foldCauseM errorHandler next environment tail runtimeTail correspondence
      ⟩

/-- Corresponding stacks have exactly the same continuation-frame count. -/
theorem corresponding_size
    (correspondence : Corresponds complete stack runtimeStack) :
    runtimeStackSize runtimeStack = stackSize stack := by
  induction correspondence with
  | done => rfl
  | flatMap next environment tail runtimeTail tailCorrespondence tailInductionHypothesis =>
      change 1 + _root_.Stack.size runtimeTail = 1 + stackSize tail
      have tailSize : _root_.Stack.size runtimeTail = stackSize tail := by
        simpa [runtimeStackSize] using tailInductionHypothesis
      exact congrArg (fun count => 1 + count) tailSize
  | foldCauseM errorHandler next environment tail runtimeTail tailCorrespondence
      tailInductionHypothesis =>
      change 1 + _root_.Stack.size runtimeTail = 1 + stackSize tail
      have tailSize : _root_.Stack.size runtimeTail = stackSize tail := by
        simpa [runtimeStackSize] using tailInductionHypothesis
      exact congrArg (fun count => 1 + count) tailSize

/-- A model success frame has the production success-continuation shape. -/
example (next : A -> Program R E B) (environment : R) :
    ∃ runtimeStack,
      Corresponds (fun _ => pure ()) (.flatMap next environment .done) runtimeStack ∧
        runtimeStackSize runtimeStack = 1 := by
  obtain ⟨runtimeStack, correspondence⟩ :=
    exists_corresponding_runtimeStack (.flatMap next environment .done) (fun _ => pure ())
  exact ⟨runtimeStack, correspondence, by simpa [stackSize] using corresponding_size correspondence⟩

/-- A model handler frame has the production error-handler shape. -/
example
    (errorHandler : Cause E -> Program R E₁ B)
    (next : A -> Program R E₁ B)
    (environment : R) :
    ∃ runtimeStack,
      Corresponds (fun _ => pure ())
          (.foldCauseM errorHandler next environment .done) runtimeStack ∧
        runtimeStackSize runtimeStack = 1 := by
  obtain ⟨runtimeStack, correspondence⟩ :=
    exists_corresponding_runtimeStack
      (.foldCauseM errorHandler next environment .done) (fun _ => pure ())
  exact ⟨runtimeStack, correspondence, by simpa [stackSize] using corresponding_size correspondence⟩

end Zenith.Formalization.SequentialRuntimeStack
