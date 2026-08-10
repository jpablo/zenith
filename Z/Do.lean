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
joins action errors. A native `catch` gets private environment and error
inference scopes for its body and handler. The body error is handled, so only
the handler error contributes to the enclosing block.

The private `zdo_action%` elaborator adapts terminal actions before Lean fixes
their branch type. This supports bare terminal actions in control-flow blocks.
-/

open Lean Meta Elab Term
open Lean.Elab.Do
open Lean.Parser.Term

syntax (name := zdo) "zdo " doSeq : term
syntax (name := zdoInfer) "zdo[" term "]" doSeq : term
syntax (name := zdoScopedTry)
  "zdo_scoped_try%[" term "," term "," term "," term "] " doElem : doElem

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
    (environment error defaultError : Term)
    (scopeCatches : Bool) : TermElabM CollectedActions := do
  if node.getKind == ``Parser.Term.do then
    return { raw := node }
  else if scopeCatches && node.getKind == ``Parser.Term.doTry then
    let original : DoElem := ⟨node⟩
    let environmentLevel ← mkFreshLevelMVar
    let environmentRequirement ←
      mkFreshExprMVar (mkSort environmentLevel.succ)
    let errorRequirement ← mkFreshExprMVar (mkSort (.succ .zero))
    let environmentRequirementSyntax ←
      Term.exprToSyntax environmentRequirement
    let errorRequirementSyntax ← Term.exprToSyntax errorRequirement
    let wrapped ← `(doElem| zdo_scoped_try%[
      $environment, $error, $environmentRequirementSyntax,
      $errorRequirementSyntax] $original:doElem)
    return {
      raw := wrapped.raw
      environmentRequirements := #[environmentRequirement]
      errorRequirements := #[errorRequirement]
    }
  else if node.getKind == ``Parser.Term.doExpr then
    let actionElement : DoElem := ⟨node⟩
    let `(doExpr| $action:term) := actionElement |
      return { raw := node }
    let (action, nestedEnvironments, nestedErrors) ← match action with
      | `(pure $value) => do
          let collected ← collectActions value.raw environment error defaultError
            scopeCatches
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
          scopeCatches
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

private def successTypeFromExpected? (expectedType? : Option Expr) : TermElabM Expr := do
  if let some expectedType := expectedType? then
    if let some expected ← expectedZ? expectedType then
      return expected.success
  mkFreshExprMVar (mkSort (.succ .zero))

/-- Run a nested `do` region with one fixed Zenith monad. -/
private def withZMonad
    (expected : ExpectedZ)
    (action : DoElabM α) : DoElabM α := do
  let expectedType := mkZType expected.level expected.environment
    expected.error expected.success
  let ops := { zDoOps expected with
    mkMonadApp := fun success =>
      pure (mkZType expected.level expected.environment expected.error success)
  }
  let localContext ← mkContext (some expectedType) ops
  withReader (fun context => { context with
    monadInfo := localContext.monadInfo
    ops := localContext.ops
  }) action

private structure ScopedSequence where
  effect : Expr
  environment : Expr
  error : Expr
  lifter : EffectForwarder

/-
Lean's standard return forwarder leaves the normal result of `Except` fresh.
The body and handler use separate monads, so use the concrete packed result
that both scopes share.
-/
private def mkScopedReturn
    (base : ControlStack)
    (liftedResultType returnValue : Expr) : DoElabM Expr := do
  let monadInfo := { (← read).monadInfo with m := (← base.m) }
  let monadInstance ← Term.mkInstMVar <|
    mkApp (mkConst ``Monad [monadInfo.u, monadInfo.v]) monadInfo.m
  let returnType ← inferType returnValue
  let liftedResultType ← whnf liftedResultType
  unless liftedResultType.isAppOfArity ``Except 2 do
    throwError "an early-return scope must have an `Except` control result"
  let continuationType := liftedResultType.getAppArgs[1]!
  let expected ← mkMonadApp (← read).doBlockResultType
  let actual := mkApp monadInfo.m <| mkApp2
    (mkConst ``Except [monadInfo.u, monadInfo.u])
    returnType continuationType
  unless ← isDefEq expected actual do
    throwError "failed to build a scoped early return"
  base.runInBase <| mkApp5
    (mkConst ``EarlyReturnT.return [monadInfo.u, monadInfo.v])
    returnType monadInfo.m continuationType monadInstance returnValue

/-- Forward `return`, mutable state, `break`, and `continue` into a scoped monad. -/
private def liftScopedEffects
    (lifter : EffectForwarder)
    (elaborate : DoElemCont → DoElabM Expr) : DoElabM Expr := do
  let oldBreakCont ← getBreakCont
  let oldContinueCont ← getContinueCont
  let oldReturnCont ← getReturnCont
  let breakCont := match oldBreakCont, lifter.breakBase? with
    | some _, some breakBase =>
        some <| breakBase.mkBreak (lifter.continueBase?.isSome)
    | _, _ => oldBreakCont
  let continueCont := match oldContinueCont, lifter.continueBase? with
    | some _, some continueBase => some <| continueBase.mkContinue
    | _, _ => oldContinueCont
  let returnCont := match lifter.returnBase? with
    | some returnBase => { oldReturnCont with
        k := mkScopedReturn returnBase lifter.liftedDoBlockResultType }
    | none => oldReturnCont
  let contInfo := ContInfo.toContInfoRef {
    breakCont
    continueCont
    returnCont
  }
  let pureCont := { lifter.origCont with
    k := lifter.liftedStack.mkPure lifter.origCont.resultName
    kind := .duplicable
  }
  withReader (fun context => { context with
    contInfo
    doBlockResultType := lifter.liftedDoBlockResultType
  }) <| elaborate pureCont

private def elabScopedSequence
    (sequence : DoSeq)
    (continuation : DoElemCont)
    (controlInfo : ControlInfo) : DoElabM ScopedSequence := do
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let environmentSyntax ← Term.exprToSyntax environment
  let errorSyntax ← Term.exprToSyntax error
  let emptyErrorSyntax ← Term.exprToSyntax (mkConst ``Empty)
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax
    emptyErrorSyntax true
  let sequence : DoSeq := ⟨collected.raw⟩
  let expected : ExpectedZ := {
    level
    environment
    error
    success := continuation.resultType
  }
  let lifter ← withZMonad expected <|
    EffectForwarder.ofCont controlInfo continuation
  let effect ← withZMonad expected <|
    liftScopedEffects lifter (elabDoSeq sequence)
  let inferredEnvironment ←
    inferEnvironment collected.environmentRequirements
  unless ← isDefEq environment inferredEnvironment do
    throwErrorAt sequence "failed to infer a scoped `zdo` environment"
  let inferredError ← inferError collected.errorRequirements
  unless ← isDefEq error inferredError do
    throwErrorAt sequence "failed to infer a scoped `zdo` error type"
  return {
    effect
    environment := ← instantiateMVars environment
    error := ← instantiateMVars error
    lifter
  }

private structure ScopedHandler where
  function : Expr
  environment : Expr
  error : Expr
  lifter : EffectForwarder
  catchesIOError : Bool

private def elabScopedHandler
    (catchClause : TSyntax ``doCatch)
    (bodyError : Expr)
    (continuation : DoElemCont)
    (controlInfo : ControlInfo) : DoElabM ScopedHandler := do
  let `(doCatch| catch $name $[: $errorType?]? => $sequence) := catchClause |
    throwUnsupportedSyntax
  let bodyError ← whnf (← instantiateMVars bodyError)
  let annotatedError? ← match errorType? with
    | some errorType => pure (some (← Term.elabType errorType))
    | none => pure none
  let catchesIOError ← match annotatedError? with
    | some annotatedError =>
        if ← isDefEq annotatedError bodyError then
          pure false
        else if ← isDefEq annotatedError (mkConst ``IO.Error) then
          unless bodyError.isConstOf ``Empty do
            throwErrorAt catchClause
              "an `IO.Error` catch requires an `Empty` typed error channel"
          pure true
        else
          throwErrorAt catchClause
            "the catch type must match the protected `zdo` error type"
    | none => pure (bodyError.isConstOf ``Empty)
  let caughtError :=
    if catchesIOError then Lean.mkConst ``IO.Error else bodyError
  let caughtErrorSyntax ← Term.exprToSyntax caughtError
  let binder := Term.mkExplicitBinder name caughtErrorSyntax
  controlAtTermElabM fun runInBase => do
    Term.elabBinder binder fun error => runInBase do
      let handler ←
        elabScopedSequence sequence continuation controlInfo
      let function ← mkLambdaFVars #[error] handler.effect
      return {
        function
        environment := handler.environment
        error := handler.error
        lifter := handler.lifter
        catchesIOError
      }

private def combineScopedCatch
    (body : ScopedSequence)
    (handler : ScopedHandler) : DoElabM (Expr × Expr) := do
  let bodyLevel ← getDecLevel body.environment
  let handlerLevel ← getDecLevel handler.environment
  let combinedLevel ← mkFreshLevelMVar
  let combinedEnvironment ← mkFreshExprMVar (mkSort combinedLevel.succ)
  let meetType := mkApp3
    (mkConst ``Environment.Meet [bodyLevel, handlerLevel, combinedLevel])
    body.environment handler.environment combinedEnvironment
  let meet ← Term.mkInstMVar meetType
  let packedType := body.lifter.liftedDoBlockResultType
  unless ← isDefEq packedType handler.lifter.liftedDoBlockResultType do
    throwError "the protected body and catch handler use different control types"
  let conversionType := mkApp2 (mkConst ``CanConvert [.zero, .zero])
    packedType packedType
  let conversion ← Term.mkInstMVar conversionType
  let effect := if handler.catchesIOError then
      mkAppN (mkConst ``Z.catchIOErrorMeet
          [bodyLevel, handlerLevel, combinedLevel]) #[
        body.environment,
        packedType,
        handler.environment,
        combinedEnvironment,
        packedType,
        handler.error,
        body.effect,
        meet,
        conversion,
        handler.function]
    else
      mkAppN (mkConst ``Z.catchAllMeet
          [bodyLevel, handlerLevel, combinedLevel]) #[
        body.environment,
        body.error,
        packedType,
        body.effect,
        handler.environment,
        combinedEnvironment,
        packedType,
        handler.error,
        meet,
        conversion,
        handler.function]
  return (effect, combinedEnvironment)

@[doElem_control_info zdoScopedTry]
private def inferScopedTryControlInfo : ControlInfoHandler := fun stx => do
  let `(doElem| zdo_scoped_try%[$_, $_, $_, $_] $original:doElem) := stx |
    throwUnsupportedSyntax
  inferControlInfoElem original

@[doElem_elab zdoScopedTry]
private def elabZDoScopedTry : DoElab := fun stx continuation => do
  let `(doElem| zdo_scoped_try%[
      $targetEnvironmentSyntax, $targetErrorSyntax,
      $environmentRequirementSyntax, $errorRequirementSyntax]
      $original:doElem) := stx | throwUnsupportedSyntax
  let `(doTry| try $bodySequence:doSeq $[$catches]*
      $[finally $finallySequence?]?) := original | throwUnsupportedSyntax
  if finallySequence?.isSome then
    throwErrorAt original
      "plain inferred `zdo` does not yet support a native `finally` block"
  unless catches.size == 1 do
    throwErrorAt original
      "plain inferred `zdo` requires exactly one catch clause"
  checkMutVarsForShadowing <| catches.filterMap (fun
    | `(doCatch| catch $name:ident $[: $_]? => $_) => some name
    | _ => none)
  let controlInfo ← inferControlInfoElem original
  let body ← elabScopedSequence bodySequence continuation controlInfo
  let catchClause : TSyntax ``doCatch ← match catches[0]! with
    | `(doCatchMatch| catch $alternatives) =>
        `(doCatch| catch error => match error with $alternatives)
    | `(doCatch| $catchClause) => pure catchClause
  let handler ← elabScopedHandler catchClause body.error
    continuation controlInfo
  let (caught, combinedEnvironment) ← combineScopedCatch body handler
  let environmentRequirement ←
    Term.elabType environmentRequirementSyntax
  unless ← isDefEq environmentRequirement combinedEnvironment do
    throwErrorAt stx "failed to collect a scoped `zdo` environment"
  let errorRequirement ← Term.elabType errorRequirementSyntax
  unless ← isDefEq errorRequirement handler.error do
    throwErrorAt stx "failed to collect a scoped `zdo` error type"
  let packedType := body.lifter.liftedDoBlockResultType
  let targetEnvironment ← Term.elabType targetEnvironmentSyntax
  let targetError ← Term.elabType targetErrorSyntax
  let targetLevel ← getDecLevel targetEnvironment
  let combinedEnvironment ← instantiateMVars combinedEnvironment
  let handlerError ← instantiateMVars handler.error
  let combinedLevel ← getDecLevel combinedEnvironment
  let environmentInstanceType := mkApp2
    (mkConst ``Environment.CanProvide [targetLevel, combinedLevel])
    targetEnvironment combinedEnvironment
  let errorInstanceType := mkApp2
    (mkConst ``ErrorChannel.CanInject [.zero, .zero])
    handlerError targetError
  let environmentInstance ← Term.mkInstMVar environmentInstanceType
  let errorInstance ← Term.mkInstMVar errorInstanceType
  let adapted := mkAppN
    (mkConst ``Z.intoJoined [targetLevel, combinedLevel]) #[
      targetEnvironment,
      combinedEnvironment,
      handlerError,
      targetError,
      packedType,
      environmentInstance,
      errorInstance,
      caught]
  let currentType := mkApp (← read).monadInfo.m packedType
  let adapted ← Term.ensureHasType currentType adapted
  let restoreExpected : ExpectedZ := {
    level := targetLevel
    environment := targetEnvironment
    error := targetError
    success := (← read).doBlockResultType
  }
  withZMonad restoreExpected do
    let restored ← body.lifter.restoreCont
    (restored.withDeadCodeFromInfo controlInfo).mkBindUnlessPure adapted

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
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let success ← successTypeFromExpected? expectedType?
  let expected : ExpectedZ := { level, environment, error, success }
  let environmentSyntax ← Term.exprToSyntax environment
  let errorSyntax ← Term.exprToSyntax error
  let emptyErrorSyntax ← Term.exprToSyntax (mkConst ``Empty)
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax
    emptyErrorSyntax true
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
    false
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
