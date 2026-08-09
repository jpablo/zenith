import Lean.Elab.Do
import Z.Combinators

/-!
`zdo` elaborates each action with a fresh environment and error type. It then
widens the action to the environment and error type of the complete block.

`zdo` requires an expected `Z R E A` type. This keeps environment selection
explicit and lets `Environment.CanProvide` verify each requirement.

`zdo[E]` infers the environment of a linear block. The error type `E` stays
explicit because Zenith does not yet have error union inference.

The private `zdo_action%` elaborator adapts terminal actions before Lean fixes
their branch type. This supports bare terminal actions in control-flow blocks.
-/

open Lean Meta Elab Term
open Lean.Elab.Do
open Lean.Parser.Term

syntax (name := zdo) "zdo " doSeq : term
syntax (name := zdoInfer) "zdo[" term "]" doSeq : term

namespace Z.Elab

private structure ExpectedZ where
  level : Level
  environment : Expr
  error : Expr
  success : Expr

private def expectedZ? (type : Expr) : TermElabM (Option ExpectedZ) := do
  let type ← whnf type
  let .const name levels := type.getAppFn | return none
  unless name == ``Z do return none
  let #[environment, error, success] := type.getAppArgs | return none
  let [level] := levels | return none
  return some { level, environment, error, success }

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

private def zDoInferredEnvironmentOps (expectedError : Expr) : DoOps where
  mkPureApp _ value := mkAppM ``Z.succeedNow #[value]
  mkBindApp α β action next := do
    let some actionType ← expectedZ? (← inferType action) | throwError
      "a `zdo` action must have type `Z R E A`"
    let nextType ← whnf (← inferType next)
    let .forallE _ nextArgument nextResult _ := nextType | throwError
      "a `zdo` continuation must be a function"
    unless ← isDefEq nextArgument α do
      throwError "a `zdo` continuation has an invalid argument type"
    let some nextActionType ← expectedZ? nextResult | throwError
      "a `zdo` continuation must return `Z R E A`"
    if ← hasAssignableMVar actionType.environment then
      discard <| isDefEq actionType.environment (mkConst ``Unit)
    if ← hasAssignableMVar nextActionType.environment then
      discard <| isDefEq nextActionType.environment (mkConst ``Unit)
    if ← hasAssignableMVar actionType.error then
      discard <| isDefEq actionType.error expectedError
    if ← hasAssignableMVar nextActionType.error then
      discard <| isDefEq nextActionType.error expectedError
    let actionEnvironment ← instantiateMVars actionType.environment
    let nextEnvironment ← instantiateMVars nextActionType.environment
    let actionTarget :=
      mkZType actionType.level actionEnvironment expectedError α
    let nextTarget ← mkArrow α <|
      mkZType nextActionType.level nextEnvironment expectedError β
    let action ← Term.ensureHasType actionTarget action
    let next ← Term.ensureHasType nextTarget next
    mkAppM ``Z.flatMapMeet #[action, next]
  isPureApp? e :=
    if e.isAppOfArity ``Z.succeedNow 2 then
      some (e.getArg! 1)
    else
      DoOps.default.isPureApp? e
  splitMonadApp? := DoOps.default.splitMonadApp?
  mkMonadApp := mkActionType

private def isPureAction (action : Term) : TermElabM Bool := do
  match action with
  | `(pure $_) => return true
  | _ => return false

syntax (name := zdoAction) "zdo_action%[" term "," term "]" term : term

@[term_elab zdoAction]
private def elabZDoAction : TermElab := fun stx expectedType? => do
  let `(zdo_action%[$targetEnvironmentSyntax, $targetErrorSyntax] $action) := stx |
    throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "internal `zdo` action requires an expected `Z R E A` type"
  let targetEnvironment ← Term.elabType targetEnvironmentSyntax
  let targetError ← Term.elabType targetErrorSyntax
  let targetType :=
    mkZType expected.level targetEnvironment targetError expected.success
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let actionType := mkZType level environment error expected.success
  let action ← Term.elabTerm action actionType
  let some actual ← expectedZ? (← inferType action) | throwErrorAt stx
    "a `zdo` action must have type `Z R E A`"
  if actual.environment.isMVar then
    discard <| isDefEq actual.environment targetEnvironment
  if actual.error.isMVar then
    discard <| isDefEq actual.error targetError
  let sourceEnvironment ← instantiateMVars actual.environment
  let sourceError ← instantiateMVars actual.error
  let success ← instantiateMVars actual.success
  let adapted ← mkAppOptM ``Z.into #[
    some targetEnvironment,
    some sourceEnvironment,
    some sourceError,
    some targetError,
    some success,
    none,
    none,
    some action]
  Term.ensureHasType targetType adapted

private partial def adaptActions
    (node : Syntax)
    (environment error : Term) : TermElabM Syntax := do
  if node.getKind == ``Parser.Term.do then
    return node
  else if node.getKind == ``Parser.Term.doExpr then
    let actionElement : DoElem := ⟨node⟩
    let `(doExpr| $action:term) := actionElement | return node
    if ← isPureAction action then return node
    withRef action do
      let adapted ← `(zdo_action%[$environment, $error] $action)
      let element ← `(doElem| $adapted:term)
      return element.raw
  else
    match node with
    | .node info kind arguments =>
      return .node info kind (← arguments.mapM fun argument =>
          adaptActions argument environment error)
    | _ => return node

@[term_elab «zdo»]
def elabZDo : TermElab := fun stx expectedType? => do
  let `(zdo $sequence) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "`zdo` requires an expected type of the form `Z R E A`"
  let inferEnvironment := expected.environment.isMVar
  let sequence : DoSeq ← if inferEnvironment then
      pure sequence
    else do
      let environment ← Term.exprToSyntax expected.environment
      let error ← Term.exprToSyntax expected.error
      let raw ← adaptActions sequence.raw environment error
      pure (⟨raw⟩ : DoSeq)
  let operations := if inferEnvironment then
      zDoInferredEnvironmentOps expected.error
    else
      zDoOps expected
  let result ← elabDoWith operations sequence expectedType?
  Term.ensureHasType expectedType result

@[term_elab zdoInfer]
def elabZDoInfer : TermElab := fun stx expectedType? => do
  let `(zdo[$errorSyntax] $sequence) := stx | throwUnsupportedSyntax
  let error ← Term.elabType errorSyntax
  let result ← elabDoWith (zDoInferredEnvironmentOps error) sequence none
  if let some resultType ← expectedZ? (← inferType result) then
    if ← hasAssignableMVar resultType.environment then
      discard <| isDefEq resultType.environment (mkConst ``Unit)
    if ← hasAssignableMVar resultType.error then
      discard <| isDefEq resultType.error error
  let result ← instantiateMVars result
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType result
  | none => pure result

end Z.Elab
