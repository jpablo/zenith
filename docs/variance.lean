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

end Variance
