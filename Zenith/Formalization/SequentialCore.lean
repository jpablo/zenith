import Z

/-!
A pure sequential model of the executable `ZCore` instruction tree.

This model keeps the control-flow and environment instructions that the
interpreter evaluates by stack manipulation. It excludes raw `IO`, callbacks,
fibers, interruption, diagrams, and runtime bookkeeping. `Evaluates` is a
big-step evaluator relation because an `A -> SequentialCore R E B`
continuation need not have a structurally bounded result tree.
-/

namespace Zenith.Formalization.SequentialCore

/-- The sequential, pure instruction subset used by the first correctness proof. -/
inductive Program : Type -> Type -> Type -> Type 1 where
  /-- Finish with a typed exit. -/
  | done (exit : Exit E A) : Program R E A
  /-- Continue only after a successful result. -/
  | flatMap (effect : Program R E A) (next : A -> Program R E B) : Program R E B
  /-- Select an error or success continuation from an exit. -/
  | foldCauseM
      (effect : Program R E A)
      (errorHandler : Cause E -> Program R E₁ B)
      (next : A -> Program R E₁ B) : Program R E₁ B
  /-- Adapt the available environment before evaluating an effect. -/
  | contramap (provide : R₀ -> R) (effect : Program R E A) : Program R₀ E A
  /-- Read the exact environment required by the program. -/
  | environment : Program R Empty R
  /-- Supply a complete environment and close the external requirement. -/
  | provideEnvironment (effect : Program R E A) (environment : R) : Program Unit E A

/-- Compile the pure model into the matching public `ZCore` constructors. -/
def toZCore : Program R E A -> ZCore R E A
  | .done exit => ZCore.done' exit
  | .flatMap effect next =>
      ZCore.flatMap (toZCore effect) fun value => toZCore (next value)
  | .foldCauseM effect errorHandler next =>
      ZCore.foldCauseM (toZCore effect)
        (fun cause => toZCore (errorHandler cause))
        (fun value => toZCore (next value))
  | .contramap provide effect => ZCore.contramap provide (toZCore effect)
  | .environment => ZCore.environment R
  | .provideEnvironment effect environment =>
      ZCore.provideEnvironment (toZCore effect) environment

/-- The model's completed-exit constructor lowers to a production done node. -/
theorem toZCore_done_showHead (exit : Exit E A) :
    (toZCore (.done exit : Program R E A)).showHead = "done" := rfl

/-- The model's success-continuation constructor lowers to a production on-success node. -/
theorem toZCore_flatMap_showHead (effect : Program R E A) (next : A -> Program R E B) :
    (toZCore (.flatMap effect next)).showHead = "onSuccess" := rfl

/-- The model's exit-handler constructor lowers to a production handler node. -/
theorem toZCore_foldCauseM_showHead
    (effect : Program R E A)
    (errorHandler : Cause E -> Program R E₁ B)
    (next : A -> Program R E₁ B) :
    (toZCore (.foldCauseM effect errorHandler next)).showHead =
      "onSuccessAndFailure" := rfl

/-- The model's environment adaptation lowers to a production contramap node. -/
theorem toZCore_contramap_showHead
    (provide : R₀ -> R)
    (effect : Program R E A) :
    (toZCore (.contramap provide effect)).showHead = "widenEnv" := rfl

/-- The model's environment read lowers to the production environment node. -/
theorem toZCore_environment_showHead :
    (toZCore (.environment : Program R Empty R)).showHead =
      "currentEnvironment" := rfl

/-- The model's provided environment lowers to the production provide node. -/
theorem toZCore_provideEnvironment_showHead
    (effect : Program R E A)
    (environment : R) :
    (toZCore (.provideEnvironment effect environment)).showHead =
      "provideEnvironment" := rfl

/--
The terminating evaluation relation for the pure sequential core.

Each constructor directly mirrors one branch of the production run loop after
logging, diagrams, asynchronous work, and interruption are removed.
-/
inductive Evaluates : Program R E A -> R -> Exit E A -> Prop where
  | done (exit : Exit E A) (environment : R) :
      Evaluates (.done exit) environment exit
  | flatMap_success
      (effect : Program R E A)
      (next : A -> Program R E B)
      (environment : R)
      (value : A)
      (exit : Exit E B) :
      Evaluates effect environment (.success value) ->
      Evaluates (next value) environment exit ->
      Evaluates (.flatMap effect next) environment exit
  | flatMap_failure
      (effect : Program R E A)
      (next : A -> Program R E B)
      (environment : R)
      (cause : Cause E) :
      Evaluates effect environment (.failure cause) ->
      Evaluates (.flatMap effect next) environment (.failure cause)
  | foldCauseM_success
      (effect : Program R E A)
      (errorHandler : Cause E -> Program R E₁ B)
      (next : A -> Program R E₁ B)
      (environment : R)
      (value : A)
      (exit : Exit E₁ B) :
      Evaluates effect environment (.success value) ->
      Evaluates (next value) environment exit ->
      Evaluates (.foldCauseM effect errorHandler next) environment exit
  | foldCauseM_failure
      (effect : Program R E A)
      (errorHandler : Cause E -> Program R E₁ B)
      (next : A -> Program R E₁ B)
      (environment : R)
      (cause : Cause E)
      (exit : Exit E₁ B) :
      Evaluates effect environment (.failure cause) ->
      Evaluates (errorHandler cause) environment exit ->
      Evaluates (.foldCauseM effect errorHandler next) environment exit
  | contramap
      (provide : R₀ -> R)
      (effect : Program R E A)
      (environment : R₀)
      (exit : Exit E A) :
      Evaluates effect (provide environment) exit ->
      Evaluates (.contramap provide effect) environment exit
  | environment (environment : R) :
      Evaluates (.environment : Program R Empty R) environment (.success environment)
  | provideEnvironment
      (effect : Program R E A)
      (provided : R)
      (exit : Exit E A) :
      Evaluates effect provided exit ->
      Evaluates (.provideEnvironment effect provided) () exit

/-- A finished instruction evaluates to exactly its stored exit. -/
theorem evaluates_done_iff
    (exit result : Exit E A)
    (environment : R) :
    Evaluates (.done exit) environment result ↔ result = exit := by
  constructor
  · intro evaluation
    cases evaluation
    rfl
  · intro equality
    subst result
    exact .done exit environment

/-- A successful `flatMap` evaluation runs its selected continuation. -/
theorem evaluates_flatMap_success
    (effect : Program R E A)
    (next : A -> Program R E B)
    (environment : R)
    (value : A)
    (exit : Exit E B)
    (effectEvaluation : Evaluates effect environment (.success value))
    (nextEvaluation : Evaluates (next value) environment exit) :
    Evaluates (.flatMap effect next) environment exit :=
  .flatMap_success effect next environment value exit effectEvaluation nextEvaluation

/-- A failed `flatMap` evaluation propagates its cause without calling `next`. -/
theorem evaluates_flatMap_failure
    (effect : Program R E A)
    (next : A -> Program R E B)
    (environment : R)
    (cause : Cause E)
    (effectEvaluation : Evaluates effect environment (.failure cause)) :
    Evaluates (.flatMap effect next) environment (.failure cause) :=
  .flatMap_failure effect next environment cause effectEvaluation

/-- A `flatMap` evaluation has exactly one of the success and failure shapes. -/
theorem evaluates_flatMap_iff
    (effect : Program R E A)
    (next : A -> Program R E B)
    (environment : R)
    (exit : Exit E B) :
    Evaluates (.flatMap effect next) environment exit ↔
      (∃ value, Evaluates effect environment (.success value) ∧
        Evaluates (next value) environment exit) ∨
      (∃ cause, Evaluates effect environment (.failure cause) ∧
        exit = .failure cause) := by
  constructor
  · intro evaluation
    cases evaluation with
    | flatMap_success _ _ _ value _ effectEvaluation nextEvaluation =>
        exact Or.inl ⟨value, effectEvaluation, nextEvaluation⟩
    | flatMap_failure _ _ _ cause effectEvaluation =>
        exact Or.inr ⟨cause, effectEvaluation, rfl⟩
  · intro evaluation
    cases evaluation with
    | inl success =>
        obtain ⟨value, effectEvaluation, nextEvaluation⟩ := success
        exact .flatMap_success effect next environment value exit effectEvaluation nextEvaluation
    | inr failure =>
        obtain ⟨cause, effectEvaluation, exitEquality⟩ := failure
        subst exit
        exact .flatMap_failure effect next environment cause effectEvaluation

/-- A successful `foldCauseM` evaluation runs its success continuation. -/
theorem evaluates_foldCauseM_success
    (effect : Program R E A)
    (errorHandler : Cause E -> Program R E₁ B)
    (next : A -> Program R E₁ B)
    (environment : R)
    (value : A)
    (exit : Exit E₁ B)
    (effectEvaluation : Evaluates effect environment (.success value))
    (nextEvaluation : Evaluates (next value) environment exit) :
    Evaluates (.foldCauseM effect errorHandler next) environment exit :=
  .foldCauseM_success effect errorHandler next environment value exit
    effectEvaluation nextEvaluation

/-- A failed `foldCauseM` evaluation runs its error handler. -/
theorem evaluates_foldCauseM_failure
    (effect : Program R E A)
    (errorHandler : Cause E -> Program R E₁ B)
    (next : A -> Program R E₁ B)
    (environment : R)
    (cause : Cause E)
    (exit : Exit E₁ B)
    (effectEvaluation : Evaluates effect environment (.failure cause))
    (handlerEvaluation : Evaluates (errorHandler cause) environment exit) :
    Evaluates (.foldCauseM effect errorHandler next) environment exit :=
  .foldCauseM_failure effect errorHandler next environment cause exit
    effectEvaluation handlerEvaluation

/-- A `foldCauseM` evaluation has exactly one success or failure continuation. -/
theorem evaluates_foldCauseM_iff
    (effect : Program R E A)
    (errorHandler : Cause E -> Program R E₁ B)
    (next : A -> Program R E₁ B)
    (environment : R)
    (exit : Exit E₁ B) :
    Evaluates (.foldCauseM effect errorHandler next) environment exit ↔
      (∃ value, Evaluates effect environment (.success value) ∧
        Evaluates (next value) environment exit) ∨
      (∃ cause, Evaluates effect environment (.failure cause) ∧
        Evaluates (errorHandler cause) environment exit) := by
  constructor
  · intro evaluation
    cases evaluation with
    | foldCauseM_success _ _ _ _ value _ effectEvaluation nextEvaluation =>
        exact Or.inl ⟨value, effectEvaluation, nextEvaluation⟩
    | foldCauseM_failure _ _ _ _ cause _ effectEvaluation handlerEvaluation =>
        exact Or.inr ⟨cause, effectEvaluation, handlerEvaluation⟩
  · intro evaluation
    cases evaluation with
    | inl success =>
        obtain ⟨value, effectEvaluation, nextEvaluation⟩ := success
        exact .foldCauseM_success effect errorHandler next environment value exit
          effectEvaluation nextEvaluation
    | inr failure =>
        obtain ⟨cause, effectEvaluation, handlerEvaluation⟩ := failure
        exact .foldCauseM_failure effect errorHandler next environment cause exit
          effectEvaluation handlerEvaluation

/-- Environment adaptation evaluates the inner program with the provided value. -/
theorem evaluates_contramap_iff
    (provide : R₀ -> R)
    (effect : Program R E A)
    (environment : R₀)
    (exit : Exit E A) :
    Evaluates (.contramap provide effect) environment exit ↔
      Evaluates effect (provide environment) exit := by
  constructor
  · intro evaluation
    cases evaluation with
    | contramap _ _ _ _ innerEvaluation => exact innerEvaluation
  · intro evaluation
    exact .contramap provide effect environment exit evaluation

/-- Reading the environment always succeeds with the required environment. -/
theorem evaluates_environment_iff
    (environment : R)
    (exit : Exit Empty R) :
    Evaluates (.environment : Program R Empty R) environment exit ↔
      exit = .success environment := by
  constructor
  · intro evaluation
    cases evaluation
    rfl
  · intro equality
    subst exit
    exact .environment environment

/-- Providing an environment evaluates the inner program with that value. -/
theorem evaluates_provideEnvironment_iff
    (effect : Program R E A)
    (provided : R)
    (exit : Exit E A) :
    Evaluates (.provideEnvironment effect provided) () exit ↔
      Evaluates effect provided exit := by
  constructor
  · intro evaluation
    cases evaluation with
    | provideEnvironment _ _ _ innerEvaluation => exact innerEvaluation
  · intro evaluation
    exact .provideEnvironment effect provided exit evaluation

/-- A terminating sequential program has only one final exit. -/
theorem evaluates_deterministic
    (left : Evaluates effect environment leftExit)
    (right : Evaluates effect environment rightExit) :
    leftExit = rightExit := by
  induction left with
  | done =>
      cases right
      rfl
  | flatMap_success effect next environment value exit effectEvaluation nextEvaluation
      effectInductionHypothesis nextInductionHypothesis =>
      cases right with
      | flatMap_success _ _ _ rightValue _ rightEffectEvaluation rightNextEvaluation =>
          have valueEquality := effectInductionHypothesis rightEffectEvaluation
          cases valueEquality
          exact nextInductionHypothesis rightNextEvaluation
      | flatMap_failure _ _ _ rightCause rightEffectEvaluation =>
          have exitEquality := effectInductionHypothesis rightEffectEvaluation
          cases exitEquality
  | flatMap_failure effect next environment cause effectEvaluation effectInductionHypothesis =>
      cases right with
      | flatMap_success _ _ _ rightValue _ rightEffectEvaluation _ =>
          have exitEquality := effectInductionHypothesis rightEffectEvaluation
          cases exitEquality
      | flatMap_failure _ _ _ rightCause rightEffectEvaluation =>
          have causeEquality := effectInductionHypothesis rightEffectEvaluation
          cases causeEquality
          rfl
  | foldCauseM_success effect errorHandler next environment value exit effectEvaluation nextEvaluation
      effectInductionHypothesis nextInductionHypothesis =>
      cases right with
      | foldCauseM_success _ _ _ _ rightValue _ rightEffectEvaluation rightNextEvaluation =>
          have valueEquality := effectInductionHypothesis rightEffectEvaluation
          cases valueEquality
          exact nextInductionHypothesis rightNextEvaluation
      | foldCauseM_failure _ _ _ _ rightCause _ rightEffectEvaluation _ =>
          have exitEquality := effectInductionHypothesis rightEffectEvaluation
          cases exitEquality
  | foldCauseM_failure effect errorHandler next environment cause exit effectEvaluation
      handlerEvaluation effectInductionHypothesis handlerInductionHypothesis =>
      cases right with
      | foldCauseM_success _ _ _ _ rightValue _ rightEffectEvaluation _ =>
          have exitEquality := effectInductionHypothesis rightEffectEvaluation
          cases exitEquality
      | foldCauseM_failure _ _ _ _ rightCause _ rightEffectEvaluation rightHandlerEvaluation =>
          have causeEquality := effectInductionHypothesis rightEffectEvaluation
          cases causeEquality
          exact handlerInductionHypothesis rightHandlerEvaluation
  | contramap provide effect environment exit innerEvaluation innerInductionHypothesis =>
      cases right with
      | contramap _ _ _ _ rightInnerEvaluation =>
          exact innerInductionHypothesis rightInnerEvaluation
  | environment =>
      cases right
      rfl
  | provideEnvironment effect provided exit innerEvaluation innerInductionHypothesis =>
      cases right with
      | provideEnvironment _ _ _ rightInnerEvaluation =>
          exact innerInductionHypothesis rightInnerEvaluation

/-- The model evaluates a successful continuation as the production run loop does. -/
example :
    Evaluates
      (.flatMap (.done (.success 2)) fun value => .done (.success (value + 1)) :
        Program Unit Empty Nat)
      ()
      (.success 3) := by
  apply evaluates_flatMap_success
  · exact .done _ _
  · exact .done _ _

/-- The model evaluates a handler after the protected program fails. -/
example :
    Evaluates
      (.foldCauseM (.done (.failure (.fail "bad")))
        (fun _ => .done (.success 7))
        (fun value => .done (.success value)) : Program Unit String Nat)
      ()
      (.success 7) := by
  apply evaluates_foldCauseM_failure
  · exact .done _ _
  · exact .done _ _

end Zenith.Formalization.SequentialCore
