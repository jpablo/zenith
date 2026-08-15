import Z.Runtime.Models

/-!
Pure routing decisions for the sequential `ZCore` instructions.

The interpreter owns diagram, logging, interruption delivery, and `IO`
execution. This module only chooses the next executable instruction or final
exit and carries the exact production state required to continue.
-/

namespace ZCore.Sequential

/-- Diagram edge emitted before the next instruction of a sequential node runs. -/
inductive RunEdge where
  /-- A success continuation was pushed. -/
  | flatMap (parentId : Option NodeId)
  /-- A success and failure continuation pair was pushed. -/
  | foldCauseM (parentId : Option NodeId)
  /-- The required environment was adapted. -/
  | contramap
  /-- A fixed environment was provided. -/
  | provideEnvironment

/-- The instruction form that produced a completed exit. -/
inductive ResumeSource where
  /-- A `done'` instruction supplied its stored exit. -/
  | done
  /-- An `environment` instruction supplied the available environment. -/
  | environment

/-- The pure result of routing one instruction in `runLoop`. -/
inductive RunAction : Type 1 where
  /-- Deliver a completed exit to the current continuation stack. -/
  | resume
      (source : ResumeSource)
      (exit : Exit E A)
      (nextState : RunState Rfiber E A E₁ A₁) : RunAction
  /-- Evaluate a next instruction with its exact saved environment evidence. -/
  | evaluate
      (nextSelf : ZCore R E A)
      (nextState : RunState Rfiber E A E₁ A₁)
      (validEnv : Environment.CanProvide Rfiber R)
      (edge : RunEdge) : RunAction
  /-- The instruction belongs to a runtime feature outside this dispatcher. -/
  | unsupported : RunAction

/-- Diagram edge emitted while delivering an exit to a saved frame. -/
inductive ResumeEdge where
  /-- Resume a success continuation. -/
  | success (parentId : Option NodeId)
  /-- Resume a failure handler. -/
  | failure (parentId : Option NodeId)

/-- The pure result of delivering an exit to a production continuation stack. -/
inductive ResumeAction : Type 1 where
  /-- Evaluate the selected continuation or handler. -/
  | evaluate
      (self : ZCore R E A)
      (state : RunState Rfiber E A E₁ A₁)
      (validEnv : Environment.CanProvide Rfiber R)
      (edge : ResumeEdge) : ResumeAction
  /-- Complete the current fiber after its children have been interrupted. -/
  | finish
      (exit : Exit E A)
      (fiberInfos : IO.Ref (List Fiber.FiberInfo))
      (complete : Observer E A) : ResumeAction
  /-- Continue failure unwinding through a frame with no error handler. -/
  | resumeFailure
      (cause : Cause E)
      (state : RunState Rfiber E A E₁ A₁) : ResumeAction
  /-- The production stack violates its required frame invariant. -/
  | invalid : ResumeAction

/--
Route one pure sequential instruction without running it.

`parentId` is already computed by the interpreter because it depends on
diagram configuration. The dispatcher stores it in new continuation frames
but performs no diagram work itself.
-/
@[inline] def run
    (self : ZCore R E A)
    [validEnv : Environment.CanProvide Rfiber R]
    (state : RunState Rfiber E A E₁ A₁)
    (parentId : Option NodeId) : RunAction :=
  /-
  `casesOn` is the dependent eliminator for the private `ZCore` instruction
  representation. It preserves each branch's exact environment, error, and
  success indices without exposing the constructors as public API.
  -/
  self.casesOn
    (motive := fun R E A _ =>
      Environment.CanProvide Rfiber R ->
      RunState Rfiber E A E₁ A₁ ->
      Option NodeId ->
      RunAction)
    (fun exit _ _ state _ => .resume .done exit state)
    (fun _ _ _ _ _ => .unsupported)
    (fun _ _ _ _ _ => .unsupported)
    (fun _ _ _ _ _ => .unsupported)
    (fun effect next _ validEnv state parentId =>
      .evaluate effect
        { state with
          stack :=
            .more (E₁ := _) next none (eq_E_E₁? := some (.up rfl))
              state.stack parentId validEnv state.environment
        }
        validEnv
        (.flatMap parentId))
    (fun effect errorHandler next _ validEnv state parentId =>
      .evaluate effect
        { state with
          stack :=
            .more next errorHandler none state.stack parentId validEnv state.environment
        }
        validEnv
        (.foldCauseM parentId))
    (fun _ _ _ _ _ _ => .unsupported)
    (fun _ _ _ _ _ _ => .unsupported)
    (fun provide effect _ validEnv state _ =>
      .evaluate effect state (validEnv.map provide) .contramap)
    (fun _ validEnv state _ =>
      .resume ResumeSource.environment
        (.success (validEnv.provide state.environment)) state)
    (fun effect providedEnvironment _ _ state _ =>
      .evaluate effect
        { state with
          environment := Environment.concat state.environment providedEnvironment
        }
        ⟨Prod.fst⟩
        .provideEnvironment)
    validEnv state parentId

/-- Route a successful result through one production continuation frame. -/
@[inline] def success
    (value : A)
    (state : RunState Rfiber E A E₁ A₁) : ResumeAction :=
  match state.stack with
  | .done complete =>
      .finish (.success value) state.fiberInfos complete
  | .more next _ _ tail parentId validEnv savedEnvironment =>
      .evaluate (next value)
        { state with stack := tail, environment := savedEnvironment }
        validEnv
        (.success parentId)

/-- Route a failed result through one production continuation frame. -/
@[inline] def failure
    (cause : Cause E)
    (state : RunState Rfiber E A E₁ A₁) : ResumeAction :=
  match state.stack with
  | .more _ (some errorHandler) _ tail parentId validEnv savedEnvironment =>
      .evaluate (errorHandler cause)
        { state with
          interruption := state.interruption.endUnwind
          stack := tail
          environment := savedEnvironment
        }
        validEnv
        (.failure parentId)
  | .more _ none (some (.up errorEquality)) tail .. =>
      let mappedCause : Cause E₁ := cause.map (cast errorEquality)
      .resumeFailure mappedCause { state with stack := tail }
  | .more _ none none .. => .invalid
  | .done complete =>
      .finish (.failure cause) state.fiberInfos complete

end ZCore.Sequential
