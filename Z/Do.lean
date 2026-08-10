import Lean.Elab.Do
import Z.Combinators

/-!
`zdo` elaborates each action with a fresh environment and error type. It then
widens the action to the environment and error type of the complete block.

`zdo` requires an expected `Z R E A` type. This keeps environment selection
explicit and lets `Environment.CanProvide` verify each requirement.

`zdo[E]` collects action requirements before it infers the complete
environment. The error type `E` stays explicit because Zenith does not yet
have error union inference.

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

private def isPureAction (action : Term) : TermElabM Bool := do
  match action with
  | `(pure $_) => return true
  | _ => return false

syntax (name := zdoAction) "zdo_action%[" term "," term "]" term : term
syntax (name := zdoCollectAction)
  "zdo_collect%[" term "," term "," term "]" term : term

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

@[term_elab zdoCollectAction]
private def elabZDoCollectAction : TermElab := fun stx expectedType? => do
  let `(zdo_collect%[$targetEnvironmentSyntax, $targetErrorSyntax,
      $requirementSyntax] $action) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "internal `zdo` action requires an expected `Z R E A` type"
  let targetEnvironment ← Term.elabType targetEnvironmentSyntax
  let targetError ← Term.elabType targetErrorSyntax
  let requirement ← Term.elabType requirementSyntax
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let actionType := mkZType level environment error expected.success
  let action ← Term.elabTerm action actionType
  let some actual ← expectedZ? (← inferType action) | throwErrorAt stx
    "a `zdo` action must have type `Z R E A`"
  if ← hasAssignableMVar actual.environment then
    discard <| isDefEq actual.environment (mkConst ``Unit)
  if ← hasAssignableMVar actual.error then
    discard <| isDefEq actual.error targetError
  let sourceEnvironment ← instantiateMVars actual.environment
  let sourceError ← instantiateMVars actual.error
  let success ← instantiateMVars actual.success
  unless ← isDefEq requirement sourceEnvironment do
    throwErrorAt stx "failed to collect the `zdo` environment requirement"
  let targetLevel ← getDecLevel targetEnvironment
  let sourceLevel ← getDecLevel sourceEnvironment
  let environmentInstanceType := mkApp2
    (mkConst ``Environment.CanProvide [targetLevel, sourceLevel])
    targetEnvironment sourceEnvironment
  let errorInstanceType := mkApp2
    (mkConst ``CanConvert [.zero, .zero]) sourceError targetError
  let environmentInstance ← Term.mkInstMVar environmentInstanceType
  let errorInstance ← Term.mkInstMVar errorInstanceType
  let adapted := mkAppN (mkConst ``Z.into [targetLevel, sourceLevel]) #[
    targetEnvironment,
    sourceEnvironment,
    sourceError,
    targetError,
    success,
    environmentInstance,
    errorInstance,
    action]
  let targetType :=
    mkZType targetLevel targetEnvironment targetError expected.success
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

private structure CollectedActions where
  raw : Syntax
  requirements : Array Expr := #[]

private partial def collectActions
    (node : Syntax)
    (environment error : Term) : TermElabM CollectedActions := do
  if node.getKind == ``Parser.Term.do then
    return { raw := node }
  else if node.getKind == ``Parser.Term.doExpr then
    let actionElement : DoElem := ⟨node⟩
    let `(doExpr| $action:term) := actionElement |
      return { raw := node }
    let (action, nestedRequirements) ← match action with
      | `(pure $value) => do
          let collected ← collectActions value.raw environment error
          let value : Term := ⟨collected.raw⟩
          let action ← `(Z.succeedNow $value)
          pure (action, collected.requirements)
      | _ => pure (action, #[])
    withRef action do
      let level ← mkFreshLevelMVar
      let requirement ← mkFreshExprMVar (mkSort level.succ)
      let requirementSyntax ← Term.exprToSyntax requirement
      let adapted ← `(zdo_collect%[$environment, $error,
        $requirementSyntax] $action)
      let element ← `(doElem| $adapted:term)
      return {
        raw := element.raw
        requirements := nestedRequirements.push requirement
      }
  else
    match node with
    | .node info kind arguments =>
      let (arguments, requirements) ← arguments.foldlM
          (init := (#[], #[])) fun (arguments, requirements) argument => do
        let collected ← collectActions argument environment error
        pure (arguments.push collected.raw,
          requirements ++ collected.requirements)
      return { raw := .node info kind arguments, requirements }
    | _ => return { raw := node }

private def meetEnvironments (requirements : Array Expr) : TermElabM Expr := do
  requirements.reverse.foldlM (init := mkConst ``Unit) fun right left => do
    let left ← instantiateMVars left
    let right ← instantiateMVars right
    let leftLevel ← getDecLevel left
    let rightLevel ← getDecLevel right
    let resultLevel ← mkFreshLevelMVar
    let result ← mkFreshExprMVar (mkSort resultLevel.succ)
    let meetType := mkApp3
      (mkConst ``Environment.Meet [leftLevel, rightLevel, resultLevel])
      left right result
    let _ ← synthInstance meetType
    instantiateMVars result

@[term_elab «zdo»]
def elabZDo : TermElab := fun stx expectedType? => do
  let `(zdo $sequence) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "`zdo` requires an expected type of the form `Z R E A`"
  if ← hasAssignableMVar expected.environment then
    throwErrorAt stx
      "`zdo` cannot infer an environment from an expected type; use `zdo[E]`"
  let environment ← Term.exprToSyntax expected.environment
  let error ← Term.exprToSyntax expected.error
  let sequence : DoSeq :=
    ⟨← adaptActions sequence.raw environment error⟩
  let result ← elabDoWith (zDoOps expected) sequence expectedType?
  Term.ensureHasType expectedType result

@[term_elab zdoInfer]
def elabZDoInfer : TermElab := fun stx expectedType? => do
  let `(zdo[$errorSyntax] $sequence) := stx | throwUnsupportedSyntax
  let error ← Term.elabType errorSyntax
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let success ← match expectedType? with
    | some expectedType =>
        match ← expectedZ? expectedType with
        | some expected => pure expected.success
        | none => mkFreshExprMVar (mkSort (.succ .zero))
    | none => mkFreshExprMVar (mkSort (.succ .zero))
  let expected : ExpectedZ := { level, environment, error, success }
  let environmentSyntax ← Term.exprToSyntax environment
  let errorSyntax ← Term.exprToSyntax error
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax
  let sequence : DoSeq := ⟨collected.raw⟩
  let internalExpectedType := mkZType level environment error success
  let result ← elabDoWith (zDoOps expected) sequence internalExpectedType
  let result ← Term.ensureHasType internalExpectedType result
  let inferredEnvironment ← meetEnvironments collected.requirements
  unless ← isDefEq environment inferredEnvironment do
    throwErrorAt stx "failed to infer the complete `zdo` environment"
  Term.synthesizeSyntheticMVarsNoPostponing
  let result ← instantiateMVars result
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType result
  | none => pure result

end Z.Elab
