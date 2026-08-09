import Z

namespace Variance

variable {R E A : Type}

example (effect : Z Unit E A) : Z R E A := effect
example (effect : Z R Empty A) : Z R E A := effect
example (effect : Z R E Empty) : Z R E A := effect
example (effect : Z Unit Empty Empty) : Z R E A := effect

example (effect : Z Unit Empty Empty) : Z R E A :=
  effect.adapt (fun _ => ()) impossible impossible

#check_failure (show Unit from (42 : Nat))

example : Z R IO.Error Nat := Z.succeedNow 1
example : Z R IO.Error (List Nat) := Z.succeedNow ([] : List Nat)

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

#check_failure (do
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string) : Z (Nat × String) Empty (Nat × String))

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

example : Z Nat Empty (Nat × Nat) := zdo
  let first <- Z.environment Nat
  let second <- Z.environment Nat
  pure (first, second)

example : Z (String × Nat) Empty (Nat × String) := zdo
  let nat <- Z.environment Nat
  let string <- Z.environment String
  pure (nat, string)

example : Z Nat Empty Nat := zdo
  Z.succeedNow ()
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

#check_failure (zdo
  pure 1)

structure HighService : Type 1 where
  value : Nat

example : Z (HighService × String) Empty Nat := zdo
  let value <- Z.serviceWith (S := HighService) (·.value)
  let string <- Z.environment String
  pure (value + string.length)

end Variance
