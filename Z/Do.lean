import Lean.Elab.Do
import Z.Combinators

/-!
`zdo` elaborates each action with a fresh environment and error type. It then
widens the action to the environment and error type of the complete block.

The first version requires an expected `Z R E A` type. This keeps environment
selection explicit and lets `IsComponent` verify each requirement.

A terminal action in an `if` or `match` branch must use `let` and end with
`pure`. Lean fixes the complete branch type before `DoOps` can widen a bare
terminal action.
-/

open Lean Meta Elab Term
open Lean.Elab.Do

syntax (name := zdo) "zdo " doSeq : term

namespace Z.Elab

private structure ExpectedZ where
  level : Level
  environment : Expr
  error : Expr

private def expectedZ? (type : Expr) : TermElabM (Option ExpectedZ) := do
  let type ← whnf type
  let .const name levels := type.getAppFn | return none
  unless name == ``Z do return none
  let #[environment, error, _] := type.getAppArgs | return none
  let [level] := levels | return none
  return some { level, environment, error }

private def mkZType
    (level : Level)
    (environment error success : Expr) : Expr :=
  mkApp3 (mkConst ``Z [level]) environment error success

private def mkActionType (success : Expr) : DoElabM Expr := do
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  return mkZType level environment error success

private def zDoOps (expected : ExpectedZ) : DoOps where
  mkPureApp := DoOps.default.mkPureApp
  mkBindApp α β action next := do
    let target (valueType : Expr) : Expr :=
      mkZType expected.level expected.environment expected.error valueType
    if let some actionType ← expectedZ? (← inferType action) then
      if actionType.error.isMVar then
        discard <| isDefEq actionType.error expected.error
    let action ← Term.ensureHasType (target α) action
    let next ← Term.ensureHasType (← mkArrow α (target β)) next
    DoOps.default.mkBindApp α β action next
  isPureApp? := DoOps.default.isPureApp?
  splitMonadApp? := DoOps.default.splitMonadApp?
  mkMonadApp := mkActionType

@[term_elab «zdo»]
def elabZDo : TermElab := fun stx expectedType? => do
  let `(zdo $sequence) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "`zdo` requires an expected type of the form `Z R E A`"
  let result ← elabDoWith (zDoOps expected) sequence expectedType?
  Term.ensureHasType expectedType result

end Z.Elab
