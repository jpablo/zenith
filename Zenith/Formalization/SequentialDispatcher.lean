import Zenith.Formalization.SequentialRuntime
import Z.Runtime.Sequential

/-!
Reduction laws that connect the executable sequential dispatcher to the pure
production transition relation.

`SequentialRuntime.Production.Step` states the runtime model as a relation.
This module checks that `ZCore.Sequential.run`, `success`, and `failure` make
the same choices as the pure production transition relation after model
instructions and continuation frames are lowered with `toZCore`.
-/

namespace Zenith.Formalization.SequentialDispatcher

open SequentialCore
open SequentialMachine
open SequentialRuntime
open SequentialRuntimeStack
open ZCore.Sequential

/-- A state reached by one executable dispatcher action after bookkeeping drops. -/
inductive Target : RunAction -> Production.State -> Prop where
  /-- A routed exit is ready for continuation-stack delivery. -/
  | resume :
      Target (.resume source exit state) (.resume exit state.stack)
  /-- A routed instruction is ready for the next evaluation step. -/
  | evaluate :
      Target (.evaluate self state validEnv edge)
        (.evaluate self state.environment validEnv state.stack)

/-- A state reached by one executable continuation-delivery action. -/
inductive ResumeTarget : ResumeAction -> Production.State -> Prop where
  /-- A saved continuation or handler is ready for evaluation. -/
  | evaluate :
      ResumeTarget (.evaluate self state validEnv edge)
        (.evaluate self state.environment validEnv state.stack)
  /-- A completed production stack halts with its delivered exit. -/
  | finish :
      ResumeTarget (.finish exit fiberInfos complete) (.halt exit)
  /-- A failure with no handler continues unwinding in the stack tail. -/
  | resumeFailure :
      ResumeTarget (.resumeFailure cause state) (.resume (.failure cause) state.stack)

/-- Mapping a cause through a reflexive error-type equality leaves it unchanged. -/
theorem cause_map_cast_rfl (cause : Cause E) :
    cause.map (cast (rfl : E = E)) = cause := by
  induction cause <;> simp [Cause.map, *]

/-- The executable dispatcher delivers a lowered `done` exit unchanged. -/
theorem run_done
    (exit : Exit E A)
    (validEnv : Environment.CanProvide Rfiber R)
    (state : RunState Rfiber E A E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.done exit : Program R E A)) (validEnv := validEnv) state parentId =
      .resume .done exit state := rfl

/-- The executable dispatcher pushes the lowered success continuation. -/
theorem run_flatMap
    (effect : Program R E A)
    (next : A -> Program R E B)
    (validEnv : Environment.CanProvide Rfiber R)
    (state : RunState Rfiber E B E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.flatMap effect next)) (validEnv := validEnv) state parentId =
      .evaluate (toZCore effect)
        { state with
          stack :=
            .more (E₁ := E) (fun value => toZCore (next value)) none
              (some (.up rfl)) state.stack parentId validEnv state.environment
        }
        validEnv
        (.flatMap parentId) := rfl

/-- The executable dispatcher pushes both lowered `foldCauseM` continuations. -/
theorem run_foldCauseM
    (effect : Program R E A)
    (errorHandler : Cause E -> Program R EHandler B)
    (next : A -> Program R EHandler B)
    (validEnv : Environment.CanProvide Rfiber R)
    (state : RunState Rfiber EHandler B E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.foldCauseM effect errorHandler next)) (validEnv := validEnv) state parentId =
      .evaluate (toZCore effect)
        { state with
          stack :=
            .more (fun value => toZCore (next value))
              (some fun cause => toZCore (errorHandler cause)) none
              state.stack parentId validEnv state.environment
        }
        validEnv
        (.foldCauseM parentId) := rfl

/-- The executable dispatcher adapts environment evidence after lowering. -/
theorem run_contramap
    (provide : R₀ -> R)
    (effect : Program R E A)
    (validEnv : Environment.CanProvide Rfiber R₀)
    (state : RunState Rfiber E A E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.contramap provide effect)) (validEnv := validEnv) state parentId =
      .evaluate (toZCore effect) state (validEnv.map provide) .contramap := rfl

/-- The executable dispatcher supplies the available environment on a read. -/
theorem run_environment
    (validEnv : Environment.CanProvide Rfiber R)
    (state : RunState Rfiber Empty (Environment R) E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.environment : Program R Empty R)) (validEnv := validEnv) state parentId =
      .resume .environment (.success (validEnv.provide state.environment)) state := rfl

/-- The executable dispatcher uses the fixed environment as the new head. -/
theorem run_provideEnvironment
    (effect : Program R E A)
    (providedEnvironment : Environment R)
    (validEnv : Environment.CanProvide Rfiber Unit)
    (state : RunState Rfiber E A E₁ A₁)
    (parentId : Option NodeId) :
    run (toZCore (.provideEnvironment effect providedEnvironment))
        (validEnv := validEnv) state parentId =
      .evaluate (toZCore effect)
        { state with
          environment := Environment.concat state.environment providedEnvironment
        }
        (⟨Prod.fst⟩ : Environment.CanProvide (R × Rfiber) R)
        .provideEnvironment := rfl

/-- Successful delivery to a completed production stack finishes the fiber. -/
theorem success_done
    (value : A)
    (complete : Observer E A)
    (state : RunState Rfiber E A Empty Empty) :
    success value { state with stack := .done complete } =
      .finish (.success value) state.fiberInfos complete := rfl

/-- Failed delivery to a completed production stack finishes the fiber. -/
theorem failure_done
    (cause : Cause E)
    (complete : Observer E A)
    (state : RunState Rfiber E A Empty Empty) :
    failure cause { state with stack := .done complete } =
      .finish (.failure cause) state.fiberInfos complete := rfl

/-- Successful delivery selects the saved lowered `flatMap` continuation. -/
theorem success_flatMap
    (value : A)
    (next : A -> Program R E B)
    (validEnv : Environment.CanProvide Rfiber R)
    (savedEnvironment : Environment Rfiber)
    (parentId : Option NodeId)
    (state : RunState Rfiber E B E₁ A₁) :
    success value
        { state with
          stack :=
            .more (fun result => toZCore (next result)) none (some (.up rfl))
              state.stack parentId validEnv savedEnvironment
        } =
      .evaluate (toZCore (next value))
        { state with stack := state.stack, environment := savedEnvironment }
        validEnv
        (.success parentId) := rfl

/-- Failed delivery through a lowered `flatMap` frame preserves its cause. -/
theorem failure_flatMap
    (cause : Cause E)
    (next : A -> Program R E B)
    (validEnv : Environment.CanProvide Rfiber R)
    (savedEnvironment : Environment Rfiber)
    (parentId : Option NodeId)
    (state : RunState Rfiber E B E₁ A₁) :
    failure cause
        { state with
          stack :=
            .more (fun result => toZCore (next result)) none (some (.up rfl))
              state.stack parentId validEnv savedEnvironment
        } =
      .resumeFailure (cause.map (cast (rfl : E = E)))
        { state with stack := state.stack } := rfl

/-- Successful delivery selects the saved lowered `foldCauseM` continuation. -/
theorem success_foldCauseM
    (value : A)
    (errorHandler : Cause E -> Program R EHandler B)
    (next : A -> Program R EHandler B)
    (validEnv : Environment.CanProvide Rfiber R)
    (savedEnvironment : Environment Rfiber)
    (parentId : Option NodeId)
    (state : RunState Rfiber EHandler B E₁ A₁) :
    success value
        { state with
          stack :=
            .more (fun result => toZCore (next result))
              (some fun cause => toZCore (errorHandler cause)) none
              state.stack parentId validEnv savedEnvironment
        } =
      .evaluate (toZCore (next value))
        { state with stack := state.stack, environment := savedEnvironment }
        validEnv
        (.success parentId) := rfl

/-- Failed delivery selects the saved lowered `foldCauseM` handler. -/
theorem failure_foldCauseM
    (cause : Cause E)
    (errorHandler : Cause E -> Program R EHandler B)
    (next : A -> Program R EHandler B)
    (validEnv : Environment.CanProvide Rfiber R)
    (savedEnvironment : Environment Rfiber)
    (parentId : Option NodeId)
    (state : RunState Rfiber EHandler B E₁ A₁) :
    failure cause
        { state with
          stack :=
            .more (fun result => toZCore (next result))
              (some fun failure => toZCore (errorHandler failure)) none
              state.stack parentId validEnv savedEnvironment
        } =
      .evaluate (toZCore (errorHandler cause))
        { state with
          interruption := state.interruption.endUnwind
          stack := state.stack
          environment := savedEnvironment
        }
        validEnv
        (.failure parentId) := rfl

/--
Every lowered pure instruction makes one executable routing step that is a
`Production.Step` of the existing runtime relation.

The target omits logging, diagrams, interruption checks, and all `IO` work.
Those parts remain outside this first proof boundary.
-/
theorem run_models_step
    (program : Program R E A)
    (validEnv : Environment.CanProvide Rfiber R)
    (state : RunState Rfiber E A E₁ A₁)
    (parentId : Option NodeId) :
    ∃ action target,
      run (toZCore program) (validEnv := validEnv) state parentId = action ∧
        Target action target ∧
        Production.Step
          (.evaluate (toZCore program) state.environment validEnv state.stack)
          target := by
  cases program with
  | done exit =>
      exact ⟨
        .resume .done exit state,
        .resume exit state.stack,
        rfl,
        .resume,
        .evaluate_done exit state.environment validEnv state.stack
      ⟩
  | flatMap effect next =>
      exact ⟨
        .evaluate (toZCore effect)
          { state with
            stack :=
              .more (E₁ := _) (fun value => toZCore (next value)) none
                (some (.up rfl)) state.stack parentId validEnv state.environment
          }
          validEnv
          (.flatMap parentId),
        .evaluate (toZCore effect) state.environment validEnv
          (.more (fun value => toZCore (next value)) none (some (.up rfl))
            state.stack parentId validEnv state.environment),
        rfl,
        .evaluate,
        .evaluate_flatMap effect next state.environment validEnv state.stack parentId
      ⟩
  | foldCauseM effect errorHandler next =>
      exact ⟨
        .evaluate (toZCore effect)
          { state with
            stack :=
              .more (fun value => toZCore (next value))
                (some fun cause => toZCore (errorHandler cause)) none
                state.stack parentId validEnv state.environment
          }
          validEnv
          (.foldCauseM parentId),
        .evaluate (toZCore effect) state.environment validEnv
          (.more (fun value => toZCore (next value))
            (some fun cause => toZCore (errorHandler cause)) none
            state.stack parentId validEnv state.environment),
        rfl,
        .evaluate,
        .evaluate_foldCauseM effect errorHandler next state.environment validEnv
          state.stack parentId
      ⟩
  | contramap provide effect =>
      exact ⟨
        .evaluate (toZCore effect) state (validEnv.map provide) .contramap,
        .evaluate (toZCore effect) state.environment (validEnv.map provide)
          state.stack,
        rfl,
        .evaluate,
        .evaluate_contramap provide effect state.environment validEnv state.stack
      ⟩
  | environment =>
      exact ⟨
        .resume .environment (.success (validEnv.provide state.environment)) state,
        .resume (.success (validEnv.provide state.environment)) state.stack,
        rfl,
        .resume,
        .evaluate_environment state.environment validEnv state.stack
      ⟩
  | provideEnvironment effect provided =>
      exact ⟨
        .evaluate (toZCore effect)
          { state with
            environment := Environment.concat state.environment provided
          }
          ⟨Prod.fst⟩
          .provideEnvironment,
        .evaluate (toZCore effect)
          (Environment.concat state.environment provided)
          Production.providedEnvironment
          state.stack,
        rfl,
        .evaluate,
        .evaluate_provideEnvironment effect provided state.environment validEnv state.stack
      ⟩

/--
Successful continuation delivery through a lowered stack is one production
transition.
-/
theorem success_models_step
    (value : A)
    (interruption : Interruption)
    (fiberInfos : IO.Ref (List Fiber.FiberInfo))
    (environment : Environment Rfiber)
    (fiberId : FiberId)
    (initialTime : Nat)
    (loggingEnabled : Bool)
    (runtimeStack : _root_.Stack E A E₁ A₁)
    (machineStack : SequentialMachine.Stack E A EFinal AFinal)
    (complete : Observer EFinal AFinal)
    (correspondence : Corresponds complete machineStack (.pack runtimeStack)) :
    let state : RunState Rfiber E A E₁ A₁ := {
      interruption := interruption
      fiberInfos := fiberInfos
      stack := runtimeStack
      environment := environment
      fiberId := fiberId
      initialTime := initialTime
      loggingEnabled := loggingEnabled
    }
    ∃ action target,
      success value state = action ∧
        ResumeTarget action target ∧
        Production.Step (.resume (.success value) state.stack) target := by
  dsimp
  cases correspondence with
  | done =>
      exact ⟨
        .finish (.success value) fiberInfos complete,
        .halt (.success value),
        rfl,
        .finish,
        .resume_done (.success value) complete
      ⟩
  | flatMap next environment tail runtimeTail tailCorrespondence parentId validEnv
      runtimeEnvironment environmentMatches =>
      exact ⟨
        .evaluate (toZCore (next value))
          {
            interruption := interruption
            fiberInfos := fiberInfos
            stack := runtimeTail
            environment := runtimeEnvironment
            fiberId := fiberId
            initialTime := initialTime
            loggingEnabled := loggingEnabled
          }
          validEnv
          (.success parentId),
        .evaluate (toZCore (next value)) runtimeEnvironment validEnv runtimeTail,
        rfl,
        .evaluate,
        .resume_flatMap_success value next runtimeEnvironment validEnv runtimeTail parentId
      ⟩
  | foldCauseM errorHandler next environment tail runtimeTail tailCorrespondence parentId
      validEnv runtimeEnvironment environmentMatches =>
      exact ⟨
        .evaluate (toZCore (next value))
          {
            interruption := interruption
            fiberInfos := fiberInfos
            stack := runtimeTail
            environment := runtimeEnvironment
            fiberId := fiberId
            initialTime := initialTime
            loggingEnabled := loggingEnabled
          }
          validEnv
          (.success parentId),
        .evaluate (toZCore (next value)) runtimeEnvironment validEnv runtimeTail,
        rfl,
        .evaluate,
        .resume_foldCauseM_success value errorHandler next runtimeEnvironment validEnv
          runtimeTail parentId
      ⟩

/--
Failed continuation delivery through a lowered stack is one production
transition.
-/
theorem failure_models_step
    (cause : Cause E)
    (interruption : Interruption)
    (fiberInfos : IO.Ref (List Fiber.FiberInfo))
    (environment : Environment Rfiber)
    (fiberId : FiberId)
    (initialTime : Nat)
    (loggingEnabled : Bool)
    (runtimeStack : _root_.Stack E A E₁ A₁)
    (machineStack : SequentialMachine.Stack E A EFinal AFinal)
    (complete : Observer EFinal AFinal)
    (correspondence : Corresponds complete machineStack (.pack runtimeStack)) :
    let state : RunState Rfiber E A E₁ A₁ := {
      interruption := interruption
      fiberInfos := fiberInfos
      stack := runtimeStack
      environment := environment
      fiberId := fiberId
      initialTime := initialTime
      loggingEnabled := loggingEnabled
    }
    ∃ action target,
      failure cause state = action ∧
        ResumeTarget action target ∧
        Production.Step (.resume (.failure cause) state.stack) target := by
  dsimp
  cases correspondence with
  | done =>
      exact ⟨
        .finish (.failure cause) fiberInfos complete,
        .halt (.failure cause),
        rfl,
        .finish,
        .resume_done (.failure cause) complete
      ⟩
  | flatMap next savedEnvironment tail runtimeTail tailCorrespondence parentId validEnv
      runtimeEnvironment environmentMatches =>
      let mappedCause : Cause E := cause.map (cast (rfl : E = E))
      refine ⟨
        .resumeFailure mappedCause
          {
            interruption := interruption
            fiberInfos := fiberInfos
            stack := runtimeTail
            environment := environment
            fiberId := fiberId
            initialTime := initialTime
            loggingEnabled := loggingEnabled
          },
        .resume (.failure mappedCause) runtimeTail,
        ?_,
        .resumeFailure,
        ?_
      ⟩
      · rfl
      · simpa [mappedCause, cause_map_cast_rfl] using
          (Production.Step.resume_flatMap_failure cause next runtimeEnvironment validEnv
            runtimeTail parentId)
  | foldCauseM errorHandler next savedEnvironment tail runtimeTail tailCorrespondence parentId
      validEnv runtimeEnvironment environmentMatches =>
      exact ⟨
        .evaluate (toZCore (errorHandler cause))
          {
            interruption := interruption.endUnwind
            fiberInfos := fiberInfos
            stack := runtimeTail
            environment := runtimeEnvironment
            fiberId := fiberId
            initialTime := initialTime
            loggingEnabled := loggingEnabled
          }
          validEnv
          (.failure parentId),
        .evaluate (toZCore (errorHandler cause)) runtimeEnvironment validEnv runtimeTail,
        rfl,
        .evaluate,
        .resume_foldCauseM_failure cause errorHandler next runtimeEnvironment validEnv
          runtimeTail parentId
      ⟩

end Zenith.Formalization.SequentialDispatcher
