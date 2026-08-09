import Z

namespace Variance

variable {R E A : Type}

example (effect : Z Unit E A) : Z R E A := effect
example (effect : Z R Empty A) : Z R E A := effect
example (effect : Z R E Empty) : Z R E A := effect

example (self : Z R E A) [ToString E] : Z R (Cause E) A :=
  self.foldCauseZ (fun cause => Z.fail' cause) pure

#check_failure fun (self : Z R E A) [ToString E] =>
  show Z R (Cause E) A from
    self.foldCauseZ (fun cause => Z.fail cause) pure

example (cause : Cause E) [ToString E] : Z R (Cause E) A :=
  let environmentWide : Z R (Cause E) Empty := Z.fail cause
  let resultWide : Z R (Cause E) A := environmentWide
  resultWide

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
