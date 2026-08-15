import Zenith.Formalization.SequentialRuntimeStack

/-!
A pure transition relation for the sequential branches of the production
interpreter.

`ZCore.runLoop` is private and executes `IO` bookkeeping. This module states
the part of that loop that is relevant to the first correctness boundary:
the instruction, available environment, `CanProvide` evidence, and production
continuation stack. It deliberately omits diagrams, logging, interruption,
fibers, callbacks, and final observer invocation.
-/

namespace Zenith.Formalization.SequentialRuntime

open SequentialCore SequentialMachine
open SequentialRuntimeStack

namespace Production

/-- The execution-relevant part of a production interpreter state. -/
inductive State : Type 1 where
  /-- Evaluate an instruction with an available environment and a continuation stack. -/
  | evaluate
      (self : ZCore R E₁ A₁)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (stack : _root_.Stack E₁ A₁ E₂ A₂) : State
  /-- Deliver a completed exit to a continuation stack. -/
  | resume
      (exit : Exit E₁ A₁)
      (stack : _root_.Stack E₁ A₁ E₂ A₂) : State
  /-- The production stack has reached its observer. -/
  | halt (exit : Exit E A) : State

/-- The canonical provider for the newly supplied head environment. -/
@[reducible] def providedEnvironment : Environment.CanProvide (R × Rfiber) R := ⟨Prod.fst⟩

/--
One pure transition matching a sequential branch of `ZCore.runLoop`,
`continueOrComplete`, or `runWithErrorHandler`.

The relation only accepts instructions and frames produced by
`SequentialCore.toZCore`. Therefore it cannot hide an unsupported runtime
case behind an arbitrary `ZCore` value.
-/
inductive Step : State -> State -> Prop where
  | evaluate_done
      (exit : Exit E₁ A₁)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (stack : _root_.Stack E₁ A₁ E A) :
      Step (.evaluate (toZCore (.done exit)) environment validEnv stack)
        (.resume exit stack)
  | evaluate_flatMap
      (effect : Program R E A)
      (next : A -> Program R E B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (stack : _root_.Stack E B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.evaluate (toZCore (.flatMap effect next)) environment validEnv stack)
        (.evaluate (toZCore effect) environment validEnv
          (.more (fun value => toZCore (next value)) none (some (.up rfl))
            stack parentId validEnv environment))
  | evaluate_foldCauseM
      (effect : Program R E A)
      (errorHandler : Cause E -> Program R EHandler B)
      (next : A -> Program R EHandler B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (stack : _root_.Stack EHandler B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.evaluate (toZCore (.foldCauseM effect errorHandler next))
          environment validEnv stack)
        (.evaluate (toZCore effect) environment validEnv
          (.more (fun value => toZCore (next value))
            (some fun cause => toZCore (errorHandler cause)) none
            stack parentId validEnv environment))
  | evaluate_contramap
      (provide : R₀ -> R)
      (effect : Program R E A)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R₀)
      (stack : _root_.Stack E A E₁ A₁) :
      Step (.evaluate (toZCore (.contramap provide effect)) environment validEnv stack)
        (.evaluate (toZCore effect) environment (validEnv.map provide) stack)
  | evaluate_environment
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (stack : _root_.Stack Empty R E₁ A₁) :
      Step (.evaluate (toZCore (.environment : Program R Empty R))
          environment validEnv stack)
        (.resume (.success (validEnv.provide environment)) stack)
  | evaluate_provideEnvironment
      (effect : Program R E A)
      (provided : Environment R)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber Unit)
      (stack : _root_.Stack E A E₁ A₁) :
      Step (.evaluate (toZCore (.provideEnvironment effect provided))
          environment validEnv stack)
        (.evaluate (toZCore effect) (Environment.concat environment provided)
          providedEnvironment stack)
  | resume_done
      (exit : Exit E A)
      (complete : Observer E A) :
      Step (.resume exit (.done complete)) (.halt exit)
  | resume_flatMap_success
      (value : A)
      (next : A -> Program R E B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (tail : _root_.Stack E B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.resume (.success value)
          (.more (fun value => toZCore (next value)) none (some (.up rfl))
            tail parentId validEnv environment))
        (.evaluate (toZCore (next value)) environment validEnv tail)
  | resume_flatMap_failure
      (cause : Cause E)
      (next : A -> Program R E B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (tail : _root_.Stack E B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.resume (.failure cause)
          (.more (fun value => toZCore (next value)) none (some (.up rfl))
            tail parentId validEnv environment))
        (.resume (.failure cause) tail)
  | resume_foldCauseM_success
      (value : A)
      (errorHandler : Cause E -> Program R EHandler B)
      (next : A -> Program R EHandler B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (tail : _root_.Stack EHandler B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.resume (.success value)
          (.more (fun value => toZCore (next value))
            (some fun cause => toZCore (errorHandler cause)) none
            tail parentId validEnv environment))
        (.evaluate (toZCore (next value)) environment validEnv tail)
  | resume_foldCauseM_failure
      (cause : Cause E)
      (errorHandler : Cause E -> Program R EHandler B)
      (next : A -> Program R EHandler B)
      (environment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R)
      (tail : _root_.Stack EHandler B E₁ A₁)
      (parentId : Option NodeId) :
      Step (.resume (.failure cause)
          (.more (fun value => toZCore (next value))
            (some fun cause => toZCore (errorHandler cause)) none
            tail parentId validEnv environment))
        (.evaluate (toZCore (errorHandler cause)) environment validEnv tail)

/-- Zero or more production sequential transitions. -/
inductive Steps : State -> State -> Prop where
  | refl : Steps state state
  | tail : Steps first second -> Step second third -> Steps first third

end Production

/--
The pure machine state corresponds to a production state when the latter
contains the lowered instruction, a stack with corresponding frames, and an
available environment that provides the exact model environment.
-/
inductive Corresponds
    (complete : Observer E A) :
    SequentialMachine.State E A -> Production.State -> Prop where
  | evaluate
      (program : Program R E₁ A₁)
      (environment : R)
      (stack : SequentialMachine.Stack E₁ A₁ E A)
      (runtimeStack : _root_.Stack E₁ A₁ E₂ A₂)
      (runtimeEnvironment : Environment Rfiber)
      (validEnv : Environment.CanProvide Rfiber R) :
      SequentialRuntimeStack.Corresponds complete stack (.pack runtimeStack) ->
      validEnv.provide runtimeEnvironment = environment ->
      Corresponds complete (.evaluate program environment stack)
        (.evaluate (toZCore program) runtimeEnvironment validEnv runtimeStack)
  | resume
      (exit : Exit E₁ A₁)
      (stack : SequentialMachine.Stack E₁ A₁ E A)
      (runtimeStack : _root_.Stack E₁ A₁ E₂ A₂) :
      SequentialRuntimeStack.Corresponds complete stack (.pack runtimeStack) ->
      Corresponds complete (.resume exit stack) (.resume exit runtimeStack)
  | halt (exit : Exit E A) :
      Corresponds complete (.halt exit) (.halt exit)

/-- A production supplied environment provides the model's supplied value. -/
theorem providedEnvironment_matches
    (environment : Environment Rfiber)
    (provided : Environment R) :
    Production.providedEnvironment.provide (Environment.concat environment provided) =
      provided := rfl

/--
Every pure-machine transition has a matching production sequential transition.

This is the first refinement theorem. It covers only states that come from
`SequentialCore.Program`; the relation has no rule for raw `IO`, callbacks,
fibers, interruption, diagrams, or logging.
-/
theorem step_refines
    (machineStep : SequentialMachine.Step first second)
    (correspondence : Corresponds complete first productionFirst) :
    ∃ productionSecond,
      Production.Step productionFirst productionSecond ∧
        Corresponds complete second productionSecond := by
  cases machineStep <;> cases correspondence
  case evaluate_done =>
    rename_i EFinal AFinal ECurrent ACurrent exit R environment stack
      E₂ A₂ Rfiber runtimeEnvironment validEnv runtimeStack environmentMatches stackCorrespondence
    exact ⟨.resume exit runtimeStack,
      .evaluate_done exit runtimeEnvironment validEnv runtimeStack,
      .resume exit stack runtimeStack stackCorrespondence⟩
  case evaluate_flatMap =>
    rename_i EFinal AFinal R ECurrent ACurrent effect B next environment stack
      E₂ A₂ Rfiber runtimeEnvironment validEnv runtimeStack environmentMatches stackCorrespondence
    exact ⟨
      .evaluate (toZCore effect) runtimeEnvironment validEnv
        (.more (fun value => toZCore (next value)) none (some (.up rfl))
          runtimeStack none validEnv runtimeEnvironment),
      .evaluate_flatMap effect next runtimeEnvironment validEnv runtimeStack none,
      .evaluate effect environment (.flatMap next environment stack)
        (.more (fun value => toZCore (next value)) none (some (.up rfl))
          runtimeStack none validEnv runtimeEnvironment)
        runtimeEnvironment validEnv
        (.flatMap next environment stack runtimeStack stackCorrespondence
          none validEnv runtimeEnvironment environmentMatches)
        environmentMatches
    ⟩
  case evaluate_foldCauseM =>
    rename_i EFinal AFinal R ECurrent ACurrent effect EHandler B errorHandler next environment stack
      E₂ A₂ Rfiber runtimeEnvironment validEnv runtimeStack environmentMatches stackCorrespondence
    exact ⟨
      .evaluate (toZCore effect) runtimeEnvironment validEnv
        (.more (fun value => toZCore (next value))
          (some fun cause => toZCore (errorHandler cause)) none
          runtimeStack none validEnv runtimeEnvironment),
      .evaluate_foldCauseM effect errorHandler next runtimeEnvironment validEnv runtimeStack none,
      .evaluate effect environment (.foldCauseM errorHandler next environment stack)
        (.more (fun value => toZCore (next value))
          (some fun cause => toZCore (errorHandler cause)) none
          runtimeStack none validEnv runtimeEnvironment)
        runtimeEnvironment validEnv
        (.foldCauseM errorHandler next environment stack runtimeStack stackCorrespondence
          none validEnv runtimeEnvironment environmentMatches)
        environmentMatches
    ⟩
  case evaluate_contramap =>
    rename_i EFinal AFinal RInput R provide ECurrent ACurrent effect environment stack
      E₂ A₂ Rfiber runtimeEnvironment validEnv runtimeStack environmentMatches stackCorrespondence
    refine ⟨.evaluate (toZCore effect) runtimeEnvironment (validEnv.map provide) runtimeStack,
      .evaluate_contramap provide effect runtimeEnvironment validEnv runtimeStack,
      .evaluate effect (provide environment) stack runtimeStack runtimeEnvironment
        (validEnv.map provide) stackCorrespondence ?_⟩
    change provide (validEnv.provide runtimeEnvironment) = provide environment
    rw [environmentMatches]
  case evaluate_environment =>
    rename_i EFinal AFinal R environment stack E₂ A₂ Rfiber runtimeEnvironment validEnv
      runtimeStack environmentMatches stackCorrespondence
    refine ⟨.resume (.success (validEnv.provide runtimeEnvironment)) runtimeStack,
      .evaluate_environment runtimeEnvironment validEnv runtimeStack, ?_⟩
    simpa only [environmentMatches] using
      (Corresponds.resume (.success environment) stack runtimeStack stackCorrespondence)
  case evaluate_provideEnvironment =>
    rename_i EFinal AFinal R ECurrent ACurrent effect provided stack E₂ A₂ Rfiber
      runtimeEnvironment validEnv runtimeStack environmentMatches stackCorrespondence
    exact ⟨
      .evaluate (toZCore effect) (Environment.concat runtimeEnvironment provided)
        Production.providedEnvironment runtimeStack,
      .evaluate_provideEnvironment effect provided runtimeEnvironment validEnv runtimeStack,
      .evaluate effect provided stack runtimeStack
        (Environment.concat runtimeEnvironment provided) Production.providedEnvironment
        stackCorrespondence (providedEnvironment_matches runtimeEnvironment provided)
    ⟩
  case resume_done =>
    rename_i EFinal AFinal exit E₂ A₂ runtimeStack stackCorrespondence
    cases stackCorrespondence
    exact ⟨.halt exit, .resume_done exit complete, .halt exit⟩
  case resume_flatMap_success =>
    rename_i EFinal AFinal ACurrent value R ECurrent B next environment tail
      E₂ A₂ runtimeStack stackCorrespondence
    cases stackCorrespondence with
    | flatMap next environment tail runtimeTail tailCorrespondence parentId validEnv
        runtimeEnvironment environmentMatches =>
        exact ⟨
          .evaluate (toZCore (next value)) runtimeEnvironment validEnv runtimeTail,
          .resume_flatMap_success value next runtimeEnvironment validEnv runtimeTail parentId,
          .evaluate (next value) environment tail runtimeTail runtimeEnvironment validEnv
            tailCorrespondence environmentMatches
        ⟩
  case resume_flatMap_failure =>
    rename_i EFinal AFinal ECurrent cause R ACurrent B next environment tail
      E₂ A₂ runtimeStack stackCorrespondence
    cases stackCorrespondence with
    | flatMap next environment tail runtimeTail tailCorrespondence parentId validEnv
        runtimeEnvironment environmentMatches =>
        exact ⟨
          .resume (.failure cause) runtimeTail,
          .resume_flatMap_failure cause next runtimeEnvironment validEnv runtimeTail parentId,
          .resume (.failure cause) tail runtimeTail tailCorrespondence
        ⟩
  case resume_foldCauseM_success =>
    rename_i EFinal AFinal ACurrent value ECurrent R EHandler B errorHandler next environment tail
      E₂ A₂ runtimeStack stackCorrespondence
    cases stackCorrespondence with
    | foldCauseM errorHandler next environment tail runtimeTail tailCorrespondence parentId
        validEnv runtimeEnvironment environmentMatches =>
        exact ⟨
          .evaluate (toZCore (next value)) runtimeEnvironment validEnv runtimeTail,
          .resume_foldCauseM_success value errorHandler next runtimeEnvironment validEnv runtimeTail
            parentId,
          .evaluate (next value) environment tail runtimeTail runtimeEnvironment validEnv
            tailCorrespondence environmentMatches
        ⟩
  case resume_foldCauseM_failure =>
    rename_i EFinal AFinal ECurrent cause R EHandler B errorHandler ACurrent next environment tail
      E₂ A₂ runtimeStack stackCorrespondence
    cases stackCorrespondence with
    | foldCauseM errorHandler next environment tail runtimeTail tailCorrespondence parentId
        validEnv runtimeEnvironment environmentMatches =>
        exact ⟨
          .evaluate (toZCore (errorHandler cause)) runtimeEnvironment validEnv runtimeTail,
          .resume_foldCauseM_failure cause errorHandler next runtimeEnvironment validEnv runtimeTail
            parentId,
          .evaluate (errorHandler cause) environment tail runtimeTail runtimeEnvironment validEnv
            tailCorrespondence environmentMatches
        ⟩

/-- A finite pure-machine execution has a matching production transition sequence. -/
theorem steps_refine
    (machineSteps : SequentialMachine.Steps first second)
    (correspondence : Corresponds complete first productionFirst) :
    ∃ productionSecond,
      Production.Steps productionFirst productionSecond ∧
        Corresponds complete second productionSecond := by
  induction machineSteps generalizing productionFirst with
  | refl =>
      exact ⟨productionFirst, .refl, correspondence⟩
  | tail =>
      rename_i final prefixInductionHypothesis
      obtain ⟨productionMiddle, productionPrefix, middleCorrespondence⟩ :=
        prefixInductionHypothesis correspondence
      obtain ⟨productionSecond, productionFinal, finalCorrespondence⟩ :=
        step_refines final middleCorrespondence
      exact ⟨productionSecond, .tail productionPrefix productionFinal, finalCorrespondence⟩

end Zenith.Formalization.SequentialRuntime
