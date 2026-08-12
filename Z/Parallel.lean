import Z.Interpreter

/-!
Concurrent effect composition.
-/

namespace Z

private inductive ParallelExit (E A B : Type)
  | left (exit : Exit E A)
  | right (exit : Exit E B)

private def awaitFiberExit (fiber : Fiber E A) : IO (Exit E A) := do
  try
    fiber.await
  catch error =>
    pure (.failure (.die error))

private def interruptAndAwait (fiber : Fiber E A) : IO (Exit E A) := do
  fiber.requestInterrupt
  awaitFiberExit fiber

private def cancelParallel
    (left : Fiber E A)
    (right : Fiber E B) : IO Unit := do
  left.requestInterrupt
  right.requestInterrupt
  let _ ← awaitFiberExit left
  let _ ← awaitFiberExit right
  pure ()

private def awaitParallel
    (left : Fiber E A)
    (right : Fiber E B)
    (combine : A -> B -> C) : IO (Exit E C) := do
  let _ : Nonempty (ParallelExit E A B) :=
    ⟨.left (.failure .interrupt)⟩
  let first ← IO.Promise.new (α := ParallelExit E A B)
  let _ ← IO.asTask do
    first.resolve (.left (← awaitFiberExit left))
  let _ ← IO.asTask do
    first.resolve (.right (← awaitFiberExit right))
  match ← IO.wait first.result? with
  | none =>
      pure (.failure (.die <| IO.userError
        "the parallel effects did not report completion"))
  | some (.left (.success leftValue)) =>
      match ← awaitFiberExit right with
      | .success rightValue => pure (.success (combine leftValue rightValue))
      | .failure cause => pure (.failure cause)
  | some (.right (.success rightValue)) =>
      match ← awaitFiberExit left with
      | .success leftValue => pure (.success (combine leftValue rightValue))
      | .failure cause => pure (.failure cause)
  | some (.left (.failure leftCause)) =>
      match ← interruptAndAwait right with
      | .success _ => pure (.failure leftCause)
      | .failure .interrupt => pure (.failure leftCause)
      | .failure rightCause =>
          pure (.failure (.parallel leftCause rightCause))
  | some (.right (.failure rightCause)) =>
      match ← interruptAndAwait left with
      | .success _ => pure (.failure rightCause)
      | .failure .interrupt => pure (.failure rightCause)
      | .failure leftCause =>
          pure (.failure (.parallel leftCause rightCause))

private def awaitRace
    (left : Fiber E A)
    (right : Fiber E B)
    (leftResult : A -> C)
    (rightResult : B -> C) : IO (Exit E C) := do
  let _ : Nonempty (ParallelExit E A B) :=
    ⟨.left (.failure .interrupt)⟩
  let first ← IO.Promise.new (α := ParallelExit E A B)
  let _ ← IO.asTask do
    first.resolve (.left (← awaitFiberExit left))
  let _ ← IO.asTask do
    first.resolve (.right (← awaitFiberExit right))
  match ← IO.wait first.result? with
  | none =>
      pure (.failure (.die <| IO.userError
        "the raced effects did not report completion"))
  | some (.left (.success value)) =>
      let _ ← interruptAndAwait right
      pure (.success (leftResult value))
  | some (.right (.success value)) =>
      let _ ← interruptAndAwait left
      pure (.success (rightResult value))
  | some (.left (.failure leftCause)) =>
      match ← awaitFiberExit right with
      | .success value => pure (.success (rightResult value))
      | .failure rightCause =>
          pure (.failure (.parallel leftCause rightCause))
  | some (.right (.failure rightCause)) =>
      match ← awaitFiberExit left with
      | .success value => pure (.success (leftResult value))
      | .failure leftCause =>
          pure (.failure (.parallel leftCause rightCause))

/-- Run two effects with one complete environment and error channel. -/
private def zipWithParSame
    (self : Z R E A)
    (other : Z R E B)
    (combine : A -> B -> C) : Z R E C :=
  let forkWithError {X : Type} (effect : Z R E X) (name : String) :
      Z R E (Fiber E X) :=
    (effect.fork name).mapFailure Empty.elim
  (forkWithError self "zipPar-left").flatMap fun left =>
    (forkWithError other "zipPar-right").flatMap fun right =>
      Z.asyncInterrupt fun callback => do
        let _ ← IO.asTask do
          callback (← awaitParallel left right combine)
        pure (cancelParallel left right)

/-- Race two effects with one complete environment and error channel. -/
private def raceMapSame
    (self : Z R E A)
    (other : Z R E B)
    (leftResult : A -> C)
    (rightResult : B -> C) : Z R E C :=
  let forkWithError {X : Type} (effect : Z R E X) (name : String) :
      Z R E (Fiber E X) :=
    (effect.fork name).mapFailure Empty.elim
  (forkWithError self "race-left").flatMap fun left =>
    (forkWithError other "race-right").flatMap fun right =>
      Z.asyncInterrupt fun callback => do
        let _ ← IO.asTask do
          callback (← awaitRace left right leftResult rightResult)
        pure (cancelParallel left right)

/--
Run two effects concurrently. Infer their combined environment and error
requirements, and combine their successful values.
-/
def zipWithPar
    [meet : Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (self : Z R₁ E₁ A)
    (other : Z R₂ E₂ B)
    (combine : A -> B -> C) : Z R E C :=
  let left : Z R E A :=
    (self.contramap meet.left).mapFailure join.left
  let right : Z R E B :=
    (other.contramap meet.right).mapFailure join.right
  zipWithParSame left right combine

/-- Run two effects concurrently and return both successful values. -/
def zipPar
    [meet : Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (self : Z R₁ E₁ A)
    (other : Z R₂ E₂ B) : Z R E (A × B) :=
  self.zipWithPar other (·, ·)

/--
Run two effects concurrently and return the first successful value. If the
first completion is a failure, wait for the other effect. If both effects
fail, preserve both causes in lexical order.
-/
def race
    [meet : Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (self : Z R₁ E₁ A)
    (other : Z R₂ E₂ A) : Z R E A :=
  let left : Z R E A :=
    (self.contramap meet.left).mapFailure join.left
  let right : Z R E A :=
    (other.contramap meet.right).mapFailure join.right
  raceMapSame left right id id

/--
Run two effects concurrently and tag the first successful value with its
branch. If both effects fail, preserve both causes in lexical order.
-/
def raceEither
    [meet : Environment.Meet R₁ R₂ R]
    [join : ErrorChannel.Join E₁ E₂ E]
    (self : Z R₁ E₁ A)
    (other : Z R₂ E₂ B) : Z R E (Sum A B) :=
  let left : Z R E A :=
    (self.contramap meet.left).mapFailure join.left
  let right : Z R E B :=
    (other.contramap meet.right).mapFailure join.right
  raceMapSame left right Sum.inl Sum.inr

end Z
