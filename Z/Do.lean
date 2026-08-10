import Lean.Elab.Do
import Lean.Util.SortExprs
import Z.Combinators

/-!
`zdo` elaborates each action with a fresh environment and error type. It then
widens the action to the environment and error type of the complete block.

With a complete expected `Z R E A` type, `zdo` verifies that each action can
use `R` and can convert its error to `E`.

`zdo[E]` collects action requirements before it infers the complete
environment. It flattens and sorts those requirements before it applies
`Environment.Meet`. The error type `E` stays explicit.

Without a complete expected type, plain `zdo` also collects, normalizes, and
joins action errors. Native `catch` needs a scoped error elaborator and is
therefore rejected in this mode. Use `zdo[E]` or `Z.catchAllMeet` for caught
errors.

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
  "zdo_collect%[" term "," term "," term "," term "," term "]" term : term

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
      $defaultErrorSyntax, $environmentRequirementSyntax,
      $errorRequirementSyntax] $action) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | unreachable!
  let expectedType ← instantiateMVars expectedType
  let some expected ← expectedZ? expectedType | throwErrorAt stx
    "internal `zdo` action requires an expected `Z R E A` type"
  let targetEnvironment ← Term.elabType targetEnvironmentSyntax
  let targetError ← Term.elabType targetErrorSyntax
  let defaultError ← Term.elabType defaultErrorSyntax
  let environmentRequirement ← Term.elabType environmentRequirementSyntax
  let errorRequirement ← Term.elabType errorRequirementSyntax
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
    discard <| isDefEq actual.error defaultError
  let sourceEnvironment ← instantiateMVars actual.environment
  let sourceError ← instantiateMVars actual.error
  let success ← instantiateMVars actual.success
  unless ← isDefEq environmentRequirement sourceEnvironment do
    throwErrorAt stx "failed to collect the `zdo` environment requirement"
  unless ← isDefEq errorRequirement sourceError do
    throwErrorAt stx "failed to collect the `zdo` error requirement"
  let targetLevel ← getDecLevel targetEnvironment
  let sourceLevel ← getDecLevel sourceEnvironment
  let environmentInstanceType := mkApp2
    (mkConst ``Environment.CanProvide [targetLevel, sourceLevel])
    targetEnvironment sourceEnvironment
  let errorInstanceType := mkApp2
    (mkConst ``ErrorChannel.CanInject [.zero, .zero]) sourceError targetError
  let environmentInstance ← Term.mkInstMVar environmentInstanceType
  let errorInstance ← Term.mkInstMVar errorInstanceType
  let adapted := mkAppN (mkConst ``Z.intoJoined [targetLevel, sourceLevel]) #[
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
  environmentRequirements : Array Expr := #[]
  errorRequirements : Array Expr := #[]

private partial def collectActions
    (node : Syntax)
    (environment error defaultError : Term) : TermElabM CollectedActions := do
  if node.getKind == ``Parser.Term.do then
    return { raw := node }
  else if node.getKind == ``Parser.Term.doExpr then
    let actionElement : DoElem := ⟨node⟩
    let `(doExpr| $action:term) := actionElement |
      return { raw := node }
    let (action, nestedEnvironments, nestedErrors) ← match action with
      | `(pure $value) => do
          let collected ← collectActions value.raw environment error defaultError
          let value : Term := ⟨collected.raw⟩
          let action ← `(Z.succeedNow $value)
          pure (action, collected.environmentRequirements,
            collected.errorRequirements)
      | _ => pure (action, #[], #[])
    withRef action do
      let level ← mkFreshLevelMVar
      let environmentRequirement ← mkFreshExprMVar (mkSort level.succ)
      let errorRequirement ← mkFreshExprMVar (mkSort (.succ .zero))
      let environmentRequirementSyntax ←
        Term.exprToSyntax environmentRequirement
      let errorRequirementSyntax ← Term.exprToSyntax errorRequirement
      let adapted ← `(zdo_collect%[$environment, $error, $defaultError,
        $environmentRequirementSyntax, $errorRequirementSyntax] $action)
      let element ← `(doElem| $adapted:term)
      return {
        raw := element.raw
        environmentRequirements :=
          nestedEnvironments.push environmentRequirement
        errorRequirements := nestedErrors.push errorRequirement
      }
  else
    match node with
    | .node info kind arguments =>
      let (arguments, environments, errors) ← arguments.foldlM
          (init := (#[], #[], #[]))
          fun (arguments, environments, errors) argument => do
        let collected ← collectActions argument environment error defaultError
        pure (arguments.push collected.raw,
          environments ++ collected.environmentRequirements,
          errors ++ collected.errorRequirements)
      return {
        raw := .node info kind arguments
        environmentRequirements := environments
        errorRequirements := errors
      }
    | _ => return { raw := node }

private partial def flattenEnvironmentRequirement
    (requirement : Expr) : TermElabM (Array Expr) := do
  let requirement ← whnf (← instantiateMVars requirement)
  if requirement.isConstOf ``Unit || requirement.isConstOf ``PUnit then
    return #[]
  if requirement.isAppOfArity ``Prod 2 then
    let left ← flattenEnvironmentRequirement requirement.getAppArgs[0]!
    let right ← flattenEnvironmentRequirement requirement.getAppArgs[1]!
    return left ++ right
  return #[requirement]

private def normalizeEnvironmentRequirements
    (requirements : Array Expr) : TermElabM (Array Expr) := do
  let flattened ← requirements.foldlM (init := #[]) fun result requirement => do
    return result ++ (← flattenEnvironmentRequirement requirement)
  return Lean.sortExprs flattened |>.1

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

private def inferEnvironment (requirements : Array Expr) : TermElabM Expr := do
  meetEnvironments (← normalizeEnvironmentRequirements requirements)

private partial def flattenErrorRequirement
    (requirement : Expr) : TermElabM (Array Expr) := do
  let requirement ← whnf (← instantiateMVars requirement)
  if requirement.isConstOf ``Empty then
    return #[]
  if requirement.isAppOfArity ``Sum 2 then
    let left ← flattenErrorRequirement requirement.getAppArgs[0]!
    let right ← flattenErrorRequirement requirement.getAppArgs[1]!
    return left ++ right
  return #[requirement]

private def normalizeErrorRequirements
    (requirements : Array Expr) : TermElabM (Array Expr) := do
  let flattened ← requirements.foldlM (init := #[]) fun result requirement => do
    return result ++ (← flattenErrorRequirement requirement)
  return Lean.sortExprs flattened |>.1

private def joinErrors (requirements : Array Expr) : TermElabM Expr := do
  requirements.reverse.foldlM (init := mkConst ``Empty) fun right left => do
    let left ← instantiateMVars left
    let right ← instantiateMVars right
    let leftLevel ← getDecLevel left
    let rightLevel ← getDecLevel right
    let resultLevel ← mkFreshLevelMVar
    let result ← mkFreshExprMVar (mkSort resultLevel.succ)
    let joinType := mkApp3
      (mkConst ``ErrorChannel.Join [leftLevel, rightLevel, resultLevel])
      left right result
    let _ ← synthInstance joinType
    instantiateMVars result

private def inferError (requirements : Array Expr) : TermElabM Expr := do
  joinErrors (← normalizeErrorRequirements requirements)

private partial def findCatch? (node : Syntax) : Option Syntax :=
  if node.getKind == ``Parser.Term.do ||
      node.getKind == ``Parser.Term.doExpr then
    none
  else if node.getKind == ``Parser.Term.doCatch ||
      node.getKind == ``Parser.Term.doCatchMatch then
    some node
  else
    match node with
    | .node _ _ arguments => arguments.findSome? findCatch?
    | _ => none

private def successTypeFromExpected? (expectedType? : Option Expr) : TermElabM Expr := do
  if let some expectedType := expectedType? then
    if let some expected ← expectedZ? expectedType then
      return expected.success
  mkFreshExprMVar (mkSort (.succ .zero))

private def elabZDoFixed
    (sequence : DoSeq)
    (expectedType : Expr)
    (expected : ExpectedZ) : TermElabM Expr := do
  let environment ← Term.exprToSyntax expected.environment
  let error ← Term.exprToSyntax expected.error
  let sequence : DoSeq :=
    ⟨← adaptActions sequence.raw environment error⟩
  let result ← elabDoWith (zDoOps expected) sequence (some expectedType)
  Term.ensureHasType expectedType result

private def elabZDoInferAll
    (stx : Syntax)
    (sequence : DoSeq)
    (expectedType? : Option Expr) : TermElabM Expr := do
  if let some catchSyntax := findCatch? sequence.raw then
    throwErrorAt catchSyntax
      "plain `zdo` cannot infer errors across `catch`; use `zdo[E]`"
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let success ← successTypeFromExpected? expectedType?
  let expected : ExpectedZ := { level, environment, error, success }
  let environmentSyntax ← Term.exprToSyntax environment
  let errorSyntax ← Term.exprToSyntax error
  let emptyErrorSyntax ← Term.exprToSyntax (mkConst ``Empty)
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax
    emptyErrorSyntax
  let sequence : DoSeq := ⟨collected.raw⟩
  let internalExpectedType := mkZType level environment error success
  let result ← elabDoWith (zDoOps expected) sequence internalExpectedType
  let result ← Term.ensureHasType internalExpectedType result
  let inferredEnvironment ← inferEnvironment collected.environmentRequirements
  unless ← isDefEq environment inferredEnvironment do
    throwErrorAt stx "failed to infer the complete `zdo` environment"
  let inferredError ← inferError collected.errorRequirements
  unless ← isDefEq error inferredError do
    throwErrorAt stx "failed to infer the complete `zdo` error type"
  Term.synthesizeSyntheticMVarsNoPostponing
  let result ← instantiateMVars result
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType result
  | none => pure result

@[term_elab «zdo»]
def elabZDo : TermElab := fun stx expectedType? => do
  let `(zdo $sequence) := stx | throwUnsupportedSyntax
  match expectedType? with
  | none => elabZDoInferAll stx sequence none
  | some expectedType =>
      let expectedType ← instantiateMVars expectedType
      if ← hasAssignableMVar expectedType then
        elabZDoInferAll stx sequence (some expectedType)
      else
        let some expected ← expectedZ? expectedType | throwErrorAt stx
          "`zdo` requires an expected type of the form `Z R E A`"
        if ← hasAssignableMVar expected.environment <||>
            hasAssignableMVar expected.error then
          elabZDoInferAll stx sequence (some expectedType)
        else
          elabZDoFixed sequence expectedType expected

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
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax errorSyntax
  let sequence : DoSeq := ⟨collected.raw⟩
  let internalExpectedType := mkZType level environment error success
  let result ← elabDoWith (zDoOps expected) sequence internalExpectedType
  let result ← Term.ensureHasType internalExpectedType result
  let inferredEnvironment ← inferEnvironment collected.environmentRequirements
  unless ← isDefEq environment inferredEnvironment do
    throwErrorAt stx "failed to infer the complete `zdo` environment"
  Term.synthesizeSyntheticMVarsNoPostponing
  let result ← instantiateMVars result
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType result
  | none => pure result

end Z.Elab
