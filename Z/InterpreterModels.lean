import Z.Environment
import Z.Combinators
import Z.GraphvizDiagram
import Z.Fiber

open Fiber

/-- 
Execution stack.  `Stack E A E₁ A₁`.

It stores success/error continuations alongside with extra data needed to resume execution.
-/
inductive Stack: (E: Type) -> (A: Type) -> (E₁: Type) -> (A₁: Type) -> Type 1 where
  | more
      (next          : A -> Z R E₁ A₁) 
      (errorHandler? : Option (Cause E -> Z R E₁ A₁))
      /- Evidence that `errorHandler` does not change the error type `E`. This is used only when *there is no* error handler. -/
      (eq_E_E₁?      : Option (PLift (E = E₁)) := none)
      (tail          : Stack E₁ A₁ E₂ A₂)
      /- Used to link the parent node with this node -/
      (parentId      : Option NodeId)
      /- Evidence that `next` and `errorHandler` can be executed given the current environment -/
      (validEnv      : R ⊂ Rprov)
      /- Environment in the Fiber at the time the stack entry was created  -/
      (env           : Environment Rprov)
    : Stack E A E₁ A₁

  | done (complete: Observer E A) : Stack E A Empty Empty

def Stack.size : Stack E A E₁ A₁ -> Nat
  | Stack.more (tail := tail) .. => 1 + Stack.size tail
  | Stack.done .. => 0

/-- State needed to execute a Fiber  -/
structure RunState (Rprov) (E A E₁ A₁: Type) where
  interruption : Interruption
  fiberInfos   : IO.Ref (List FiberInfo)
  stack        : Stack E A E₁ A₁
  environment  : Environment Rprov
  fiberId      : FiberId
  initialTime  : Nat


/-- Generates a new random id using `fiberId` as prefix  -/
def RunState.newId (self: RunState R E A E₁ A₁) : IO NodeId :=
  GraphViz.newId self.fiberId

