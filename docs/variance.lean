import Z

namespace Variance

abbrev InferredEnvironment
    {R : Type u} {E A : Type} (_ : Z R E A) : Type u := R

variable {R E A : Type}

example (effect : Z Unit E A) : Z R E A := effect
example (effect : Z R Empty A) : Z R E A := effect
example (effect : Z R E Empty) : Z R E A := effect
example (effect : Z Unit Empty Empty) : Z R E A := effect

example (effect : Z Unit Empty Empty) : Z R E A :=
  effect.adapt (fun _ => ()) impossible impossible

#check_failure (show Unit from (42 : Nat))

example : Z R IO.Error Nat := Z.succeed 1
example : Z R IO.Error (List Nat) := Z.succeed ([] : List Nat)

example (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail cause) pure

example (cause : Cause E) [ToString E] : Z R (Cause E) A :=
  Z.fail cause

example (layer : Layer Unit Empty Empty) : Layer R E A := layer

example (layer : Layer Unit Empty Empty) : Layer R E A :=
  layer.adapt (fun _ => ()) impossible impossible

def environmentPart (A : Type) [A ∣ R] : Z R Empty (Environment A) :=
  (Z.environment A).contramap fun environment : Environment R =>
    environment.get A

example : Z (Nat × String) Empty (Nat × String) := do
  let environment <- Z.environment (Nat × String)
  pure (environment.get Nat, environment.get String)

example : Z (Nat × String) Empty (Nat × String) := do
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

example : Z (Nat × String) Empty (Nat × String) := do
  let nat <- environmentPart Nat
  let string <- environmentPart String
  pure (nat, string)

example : Z (Nat × String) Empty (Nat × String) :=
  Z.flatMapIn (Z.environment Nat) fun nat =>
    (Z.environment String).map fun string =>
      (nat, string)

example : Z (Nat × String) Empty (Nat × String) := zdo
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

def inferredCombined := zdo[Empty]
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

#check inferredCombined
example : Z (Nat × String) Empty (Nat × String) := inferredCombined

def inferredReordered := zdo[Empty]
  let string <- Z.environment String
  let nat <- Z.environment Nat
  pure (string, nat)

#check inferredReordered
example : InferredEnvironment inferredCombined =
    InferredEnvironment inferredReordered := rfl

def inferredGrouped := zdo[Empty]
  Z.environment (String × Nat)

#check inferredGrouped
example : InferredEnvironment inferredCombined =
    InferredEnvironment inferredGrouped := rfl

def inferredRepeated := zdo[Empty]
  let first <- Z.environment Nat
  let second <- Z.environment Nat
  pure (first, second)

#check inferredRepeated
example : Z Nat Empty (Nat × Nat) := inferredRepeated

def inferredThree := zdo[Empty]
  let nat <- Z.environment Nat
  let string <- Z.environment String
  let bool <- Z.environment Bool
  pure (nat, string, bool)

#check inferredThree
example : Z (Bool × Nat × String) Empty (Nat × String × Bool) :=
  inferredThree

def inferredNonAdjacentDuplicate := zdo[Empty]
  let first <- Z.environment Nat
  let string <- Z.environment String
  let second <- Z.environment Nat
  pure (first, string, second)

#check inferredNonAdjacentDuplicate
example : Z (Nat × String) Empty (Nat × String × Nat) :=
  inferredNonAdjacentDuplicate

def inferredError := zdo[IO.Error]
  let nat <- Z.environment Nat
  let value <- Z.attempt (pure 1)
  pure (nat, value)

#check inferredError
example : Z Nat IO.Error (Nat × Nat) := inferredError

def inferredPure := zdo[Empty]
  pure 42

#check inferredPure
example : Z Unit Empty Nat := inferredPure

def inferredIf (selectNat : Bool) := zdo[Empty]
  if selectNat then
    let _ <- Z.environment Nat
    pure "nat"
  else
    Z.environment String

#check inferredIf
example : Bool → Z (Nat × String) Empty String := inferredIf

def inferredMatch (selection : Option Bool) := zdo[Empty]
  match selection with
  | some true =>
      let _ <- Z.environment Nat
      pure "nat"
  | _ => Z.environment String

#check inferredMatch
example : Option Bool → Z (Nat × String) Empty String := inferredMatch

def inferredTry := zdo[IO.Error]
  try
    let _ <- Z.environment Nat
    throw (IO.userError "expected")
  catch _ =>
    Z.environment String

#check inferredTry
example : Z (Nat × String) IO.Error String := inferredTry

def inferredLoop := zdo[Empty]
  for _ in [1, 2] do
    let _ <- Z.environment Nat
    pure ()
  Z.environment String

#check inferredLoop
example : Z (Nat × String) Empty String := inferredLoop

def inferredReturn (stopEarly : Bool) := zdo[Empty]
  if stopEarly then
    return "early"
  let _ <- Z.environment Nat
  Z.environment String

#check inferredReturn
example : Bool → Z (Nat × String) Empty String := inferredReturn

def inferredNestedActions := zdo[Empty]
  pure ((<- Z.environment Nat), (<- Z.environment String))

#check inferredNestedActions
example : Z (Nat × String) Empty (Nat × String) := inferredNestedActions

example : Z Nat Empty (Nat × Nat) := zdo
  let first <- Z.environment Nat
  let second <- Z.environment Nat
  pure (first, second)

example : Z (String × Nat) Empty (Nat × String) := zdo
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

example : Z Nat Empty Nat := zdo
  Z.succeed ()
  Z.environment Nat

example : Z (Nat × String) IO.Error String := zdo
  let _ <- Z.environment Nat
  let _ <- Z.environment String
  pure "ok"

example (selectNat : Bool) : Z (Nat × String) Empty String := zdo
  if selectNat then
    let _ <- Z.environment Nat
    pure "nat"
  else
    Z.environment String

example (selectNat : Bool) : Z (Nat × String) Empty String := do
  if selectNat then
    pure "nat"
  else
    Z.into (R := Nat × String) (E := Empty) (Z.environment String)

example (selection : Option Bool) : Z (Nat × String) Empty String := zdo
  match selection with
  | some true =>
      let _ <- Z.environment Nat
      pure "nat"
  | _ => Z.environment String

example (stopEarly : Bool) : Z (Nat × String) Empty String := zdo
  if stopEarly then
    return "early"
  let _ <- Z.environment Nat
  Z.environment String

example : Z (Nat × String) IO.Error String := zdo
  try
    let _ <- Z.environment Nat
    throw (IO.userError "expected")
  catch _ =>
    Z.environment String

example : Z (Nat × String) Empty String := zdo
  for _ in [1, 2] do
    let _ <- Z.environment Nat
    pure ()
  Z.environment String

example : Z (Nat × String) Empty (Nat × String) := zdo
  pure ((<- Z.environment Nat), (<- Z.environment String))

example : Z Unit IO.Error Nat := zdo
  let io : IO Nat := do
    pure 42
  Z.attempt io

#check_failure (zdo
  let string <- Z.environment String
  pure string : Z Nat Empty String)

def inferredAllPure := zdo
  pure 1

#check inferredAllPure
example : Z Unit Empty Nat := inferredAllPure

def inferredBareThrow := zdo
  let _ : Nat <- throw (IO.userError "defect")
  pure 1

#check inferredBareThrow
example : Z Unit Empty Nat := inferredBareThrow

def stringAction : Z Unit String Nat := Z.succeed 1

def inferredAllErrors := zdo
  let first <- stringAction
  let second <- Z.attempt (pure 2)
  pure (first + second)

#check inferredAllErrors
example : Z Unit (IO.Error ⊕ String) Nat := inferredAllErrors

def inferredAllErrorsReordered := zdo
  let second <- Z.attempt (pure 2)
  let first <- stringAction
  pure (first + second)

#check inferredAllErrorsReordered
example : Z Unit (IO.Error ⊕ String) Nat := inferredAllErrorsReordered

def prejoinedAction : Z Unit (String ⊕ IO.Error) Nat := Z.succeed 1

def inferredNormalizedErrorSum := zdo
  prejoinedAction

#check inferredNormalizedErrorSum
example : Z Unit (IO.Error ⊕ String) Nat := inferredNormalizedErrorSum

def inferredAllBranches (useString : Bool) := zdo
  if useString then
    stringAction
  else
    Z.attempt (pure 2)

#check inferredAllBranches
example : Bool → Z Unit (IO.Error ⊕ String) Nat := inferredAllBranches

def inferredAllParameters := zdo
  let nat <- Z.environment Nat
  let first <- stringAction
  let string <- Z.environment String
  let second <- Z.attempt (pure 2)
  pure (nat + first + string.length + second)

#check inferredAllParameters
example : Z (Nat × String) (IO.Error ⊕ String) Nat :=
  inferredAllParameters

def inferredAllLoop := zdo
  for _ in [1, 2] do
    let _ <- stringAction
    pure ()
  Z.attempt (pure 2)

#check inferredAllLoop
example : Z Unit (IO.Error ⊕ String) Nat := inferredAllLoop

def inferredTypedCatch := zdo
  try
    let _ : Nat <- (Z.fail "expected" : Z Nat String Nat)
    pure 0
  catch _ =>
    (Z.environment String).map String.length

#check inferredTypedCatch
example : Z (Nat × String) Empty Nat := inferredTypedCatch

def inferredDefectCatch := zdo
  try
    let _ : Nat <- throw (IO.userError "expected")
    pure 0
  catch _ =>
    Z.succeed 1

#check inferredDefectCatch
example : Z Unit Empty Nat := inferredDefectCatch

def inferredMultipleCatch := zdo
  try
    let _ : Nat <- (Z.fail "body" : Z Unit String Nat)
    pure 0
  catch _ =>
    let _ : Nat <- (Z.fail 5 : Z Unit Nat Nat)
    pure 1
  catch number =>
    pure (number + 1)

#check inferredMultipleCatch
example : Z Unit Empty Nat := inferredMultipleCatch

def inferredFinally := zdo
  try
    let _ : Nat <- (Z.fail "body" : Z Unit String Nat)
    pure 0
  finally
    Z.attempt (pure ())

#check inferredFinally
example : Z Unit (IO.Error ⊕ String) Nat := inferredFinally

def inferredCatchFinally := zdo
  try
    let _ : Nat <- (Z.fail "handled" : Z Unit String Nat)
    pure 0
  catch _ =>
    pure 1
  finally
    Z.attempt (pure ())

#check inferredCatchFinally
example : Z Unit IO.Error Nat := inferredCatchFinally

def explicitCaughtAction : Z Unit IO.Error Nat := zdo
  try
    throw (IO.userError "expected")
  catch _ =>
    Z.succeed 1

def inferredAroundNestedCatch := zdo
  explicitCaughtAction

#check inferredAroundNestedCatch
example : Z Unit IO.Error Nat := inferredAroundNestedCatch

def handledBody : Z Nat String Nat := Z.fail "handled"

def inferredHandledError := zdo
  handledBody.catchAllMeet fun _ =>
    (Z.environment String).map String.length

#check inferredHandledError
example : Z (Nat × String) Empty Nat := inferredHandledError

structure HighService : Type 1 where
  value : Nat

example : Z (HighService × String) Empty Nat := zdo
  let value <- Z.serviceWith (S := HighService) (·.value)
  let string <- Z.environment String
  pure (value + string.length)

def inferredHighService := zdo[Empty]
  let value <- Z.serviceWith (S := HighService) (·.value)
  let string <- Z.environment String
  pure (value + string.length)

#check inferredHighService
example : Z (String × HighService) Empty Nat := inferredHighService

end Variance
