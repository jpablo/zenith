import Zenith.Formalization.SequentialCore

/-!
A typed stack machine for `SequentialCore.Program`.

The machine is a pure model of the sequential branches of the production
interpreter: `runLoop`, `continueOrComplete`, and `runWithErrorHandler`.
It contains no raw `IO`, fibers, callbacks, interruption, or observability
bookkeeping.
-/

namespace Zenith.Formalization.SequentialMachine

open SequentialCore

/-- A continuation stack from the current exit type to a final exit type. -/
inductive Stack : Type -> Type -> Type -> Type -> Type 1 where
  /-- Finish with the current exit. -/
  | done : Stack E A E A
  /-- Run `next` after a successful current exit. -/
  | flatMap
      (next : A -> Program R E B)
      (environment : R)
      (tail : Stack E B E₁ A₁) :
      Stack E A E₁ A₁
  /-- Select an error or success continuation from the current exit. -/
  | foldCauseM
      (errorHandler : Cause E -> Program R E₁ B)
      (next : A -> Program R E₁ B)
      (environment : R)
      (tail : Stack E₁ B E₂ A₂) :
      Stack E A E₂ A₂

/-- A machine state with a fixed final error and success type. -/
inductive State (E A : Type) : Type 1 where
  /-- Evaluate one program against its exact environment. -/
  | evaluate
      (program : Program R E₁ A₁)
      (environment : R)
      (stack : Stack E₁ A₁ E A) : State E A
  /-- Deliver one exit to the current continuation stack. -/
  | resume
      (exit : Exit E₁ A₁)
      (stack : Stack E₁ A₁ E A) : State E A
  /-- The stack has completed with its final exit. -/
  | halt (exit : Exit E A) : State E A

/-- One pure stack-machine transition. -/
inductive Step : State E A -> State E A -> Prop where
  | evaluate_done :
      Step (.evaluate (.done exit) environment stack) (.resume exit stack)
  | evaluate_flatMap :
      Step (.evaluate (.flatMap effect next) environment stack)
        (.evaluate effect environment (.flatMap next environment stack))
  | evaluate_foldCauseM :
      Step (.evaluate (.foldCauseM effect errorHandler next) environment stack)
        (.evaluate effect environment
          (.foldCauseM errorHandler next environment stack))
  | evaluate_contramap :
      Step (.evaluate (.contramap provide effect) environment stack)
        (.evaluate effect (provide environment) stack)
  | evaluate_environment :
      Step (.evaluate (.environment : Program R Empty R) environment stack)
        (.resume (.success environment) stack)
  | evaluate_provideEnvironment :
      Step (.evaluate (.provideEnvironment effect provided) () stack)
        (.evaluate effect provided stack)
  | resume_done (exit : Exit E A) :
      Step (.resume exit (.done : Stack E A E A)) (.halt exit)
  | resume_flatMap_success :
      Step (.resume (.success value) (.flatMap next environment tail))
        (.evaluate (next value) environment tail)
  | resume_flatMap_failure :
      Step (.resume (.failure cause) (.flatMap next environment tail))
        (.resume (.failure cause) tail)
  | resume_foldCauseM_success :
      Step (.resume (.success value) (.foldCauseM errorHandler next environment tail))
        (.evaluate (next value) environment tail)
  | resume_foldCauseM_failure :
      Step (.resume (.failure cause) (.foldCauseM errorHandler next environment tail))
        (.evaluate (errorHandler cause) environment tail)

/-- Zero or more pure stack-machine transitions. -/
inductive Steps : State E A -> State E A -> Prop where
  | refl : Steps state state
  | tail : Steps first second -> Step second third -> Steps first third

namespace Steps

/-- One transition is a nonempty transition sequence. -/
theorem single (step : Step first second) : Steps first second :=
  .tail .refl step

/-- Concatenate two transition sequences. -/
theorem trans : Steps first second -> Steps second third -> Steps first third
  | first, .refl => first
  | first, .tail rest step => .tail (trans first rest) step

end Steps

/--
Evaluation in the direct semantic model reaches the matching resume state for
any continuation stack.
-/
theorem evaluation_runs_to_resume
    {R E A E₁ A₁ : Type}
    {program : Program R E A}
    {environment : R}
    {exit : Exit E A}
    (evaluation : Evaluates program environment exit)
    (stack : Stack E A E₁ A₁) :
    Steps (.evaluate program environment stack) (.resume exit stack) := by
  induction evaluation generalizing E₁ A₁ with
  | done =>
      exact Steps.single .evaluate_done
  | flatMap_success effect next environment value exit effectEvaluation nextEvaluation
      effectInductionHypothesis nextInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_flatMap)
      apply Steps.trans (effectInductionHypothesis (.flatMap next environment stack))
      apply Steps.trans (Steps.single .resume_flatMap_success)
      exact nextInductionHypothesis stack
  | flatMap_failure effect next environment cause effectEvaluation effectInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_flatMap)
      apply Steps.trans (effectInductionHypothesis (.flatMap next environment stack))
      exact Steps.single .resume_flatMap_failure
  | foldCauseM_success effect errorHandler next environment value exit effectEvaluation
      nextEvaluation effectInductionHypothesis nextInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_foldCauseM)
      apply Steps.trans
        (effectInductionHypothesis (.foldCauseM errorHandler next environment stack))
      apply Steps.trans (Steps.single .resume_foldCauseM_success)
      exact nextInductionHypothesis stack
  | foldCauseM_failure effect errorHandler next environment cause exit effectEvaluation
      handlerEvaluation effectInductionHypothesis handlerInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_foldCauseM)
      apply Steps.trans
        (effectInductionHypothesis (.foldCauseM errorHandler next environment stack))
      apply Steps.trans (Steps.single .resume_foldCauseM_failure)
      exact handlerInductionHypothesis stack
  | contramap provide effect environment exit innerEvaluation innerInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_contramap)
      exact innerInductionHypothesis stack
  | environment =>
      exact Steps.single .evaluate_environment
  | provideEnvironment effect provided exit innerEvaluation innerInductionHypothesis =>
      apply Steps.trans (Steps.single .evaluate_provideEnvironment)
      exact innerInductionHypothesis stack

/-- A direct model evaluation reaches the same final exit in the stack machine. -/
theorem evaluation_runs_to_halt
    {R E A : Type}
    {program : Program R E A}
    {environment : R}
    {exit : Exit E A}
    (evaluation : Evaluates program environment exit) :
    Steps (.evaluate program environment (.done : Stack E A E A)) (.halt exit) :=
  Steps.trans (evaluation_runs_to_resume evaluation .done)
    (Steps.single (.resume_done exit))

/-- A simple successful bind reaches its final exit through the stack machine. -/
example :
    Steps
      (.evaluate
        (.flatMap (.done (.success 2)) fun value => .done (.success (value + 1)) :
          Program Unit Empty Nat)
        ()
        (.done : Stack Empty Nat Empty Nat))
      (.halt (.success 3)) := by
  apply evaluation_runs_to_halt
  apply Evaluates.flatMap_success
  · exact .done _ _
  · exact .done _ _

end Zenith.Formalization.SequentialMachine
