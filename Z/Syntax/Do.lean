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

An environment type can use `[zdo_row_environment normalize]` to register one
explicit list argument as a row. The collector combines and normalizes all
requirements for that environment type before it applies `Environment.Meet`.
The package that owns the environment supplies its normalizer and projection
instances. `Z.Syntax.Do` does not depend on that package.

Without a complete expected type, plain `zdo` also collects, normalizes, and
joins action errors.

Every spelling gives a native `catch` private environment and error inference
scopes for its body and handler. The body error is handled, so only the handler
error contributes to the enclosing block. A native `finally` gets another
private scope. Its environment and error are combined with the protected
effect, and it runs before forwarded control resumes.

The private `zdo_collect%` elaborator adapts terminal actions before Lean fixes
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

/--
Register a unary row-environment type and the function that normalizes its
row argument. The `zdo` collector combines requirements that use the same
registered environment before it applies `Environment.Meet`.
-/
initialize rowEnvironmentAttr : ParametricAttribute Name ←
  registerParametricAttribute {
    name := `zdo_row_environment
    descr := "row normalizer used by the `zdo` environment collector"
    getParam := fun _ stx => do
      let normalizerSyntax ← Attribute.Builtin.getIdent stx
      Elab.realizeGlobalConstNoOverloadWithInfo normalizerSyntax
  }

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

syntax (name := zdoCollectAction)
  "zdo_collect%[" term "," term "," term "," term "," term "]" term : term

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
  let adapted := mkAppN (mkConst ``Z.widenWithErrorInjection [targetLevel, sourceLevel]) #[
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

private structure CollectedActions where
  raw : Syntax
  environmentRequirements : Array Expr := #[]
  errorRequirements : Array Expr := #[]

private partial def collectActions
    (node : Syntax)
    (environment error defaultError : Term) : TermElabM CollectedActions := do
  if node.getKind == ``Parser.Term.do then
    return { raw := node }
  else if node.getKind == ``Parser.Term.doTry then
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
          let value : Term := ⟨collected.raw⟩
          let action ← `(Z.succeed $value)
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

private structure RowEnvironmentGroup where
  wrapper : Name
  normalizer : Name
  elementType : Expr
  entries : Array Expr := #[]
  deriving Inhabited

private partial def rowEntries
    (wrapper : Name)
    (row : Expr) : TermElabM (Expr × Array Expr) := do
  let rowType ← whnf (← inferType row)
  unless rowType.isAppOfArity ``List 1 do
    throwError
      "`[{rowEnvironmentAttr.attr.name}]` requires a list row argument on `{wrapper}`"
  let elementType := rowType.getAppArgs[0]!
  let row ← whnf (← instantiateMVars row)
  if row.getAppFn.isConstOf ``List.nil then
    return (elementType, #[])
  if row.getAppFn.isConstOf ``List.cons then
    let arguments := row.getAppArgs
    unless arguments.size == 3 do
      throwError "invalid list row in registered environment `{wrapper}`"
    let (tailElementType, tail) ← rowEntries wrapper arguments[2]!
    unless ← isDefEq elementType tailElementType do
      throwError "inconsistent list row in registered environment `{wrapper}`"
    return (elementType, #[arguments[1]!] ++ tail)
  throwError
    "the row argument of registered environment `{wrapper}` must reduce to a list"

private def rowEnvironment? (requirement : Expr) :
    TermElabM (Option (Name × Name × Expr)) := do
  let requirement ← whnf (← instantiateMVars requirement)
  let .const wrapper _ := requirement.getAppFn | return none
  let some normalizer := rowEnvironmentAttr.getParam? (← getEnv) wrapper |
    return none
  let arguments := requirement.getAppArgs
  unless arguments.size == 1 do
    throwError
      "registered row environment `{wrapper}` must have one explicit row argument"
  return some (wrapper, normalizer, arguments[0]!)

private def normalizeRowEnvironments
    (requirements : Array Expr) : TermElabM (Array Expr) := do
  let mut ordinary := #[]
  let mut groups : Array RowEnvironmentGroup := #[]
  for requirement in requirements do
    match ← rowEnvironment? requirement with
    | none => ordinary := ordinary.push requirement
    | some (wrapper, normalizer, row) =>
        let (elementType, entries) ← rowEntries wrapper row
        let mut groupIndex? : Option Nat := none
        for index in [:groups.size] do
          let group := groups[index]!
          if group.wrapper == wrapper && group.normalizer == normalizer &&
              (← isDefEq group.elementType elementType) then
            groupIndex? := some index
            break
        match groupIndex? with
        | some index =>
            let group := groups[index]!
            groups := groups.set! index {
              group with entries := group.entries ++ entries
            }
        | none =>
            groups := groups.push {
              wrapper
              normalizer
              elementType
              entries
            }
  for group in groups do
    let row ← mkListLit group.elementType group.entries.toList
    let normalized ← mkAppM group.normalizer #[row]
    let (_, normalizedEntries) ← rowEntries group.wrapper normalized
    let normalizedRow ←
      mkListLit group.elementType normalizedEntries.toList
    ordinary := ordinary.push (← mkAppM group.wrapper #[normalizedRow])
  return ordinary

private def normalizeEnvironmentRequirements
    (requirements : Array Expr) : TermElabM (Array Expr) := do
  let flattened ← requirements.foldlM (init := #[]) fun result requirement => do
    return result ++ (← flattenEnvironmentRequirement requirement)
  return Lean.sortExprs (← normalizeRowEnvironments flattened) |>.1

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

private def inferEnvironmentMeet
    (left right : Expr) : TermElabM (Expr × Expr) := do
  let left ← instantiateMVars left
  let right ← instantiateMVars right
  let result ← inferEnvironment #[left, right]
  let leftLevel ← getDecLevel left
  let rightLevel ← getDecLevel right
  let resultLevel ← getDecLevel result
  let leftProviderType := mkApp2
    (mkConst ``Environment.CanProvide [resultLevel, leftLevel]) result left
  let rightProviderType := mkApp2
    (mkConst ``Environment.CanProvide [resultLevel, rightLevel]) result right
  let leftProvider ← synthInstance leftProviderType
  let rightProvider ← synthInstance rightProviderType
  let meet := mkAppN
    (mkConst ``Environment.Meet.ofCanProvide
      [leftLevel, rightLevel, resultLevel]) #[
      left,
      right,
      result,
      leftProvider,
      rightProvider]
  return (result, meet)

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
    emptyErrorSyntax
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

private structure ScopedFinalizer where
  effect : Expr
  environment : Expr
  error : Expr

private def elabScopedFinalizer
    (sequence : DoSeq) : DoElabM ScopedFinalizer := do
  let level ← mkFreshLevelMVar
  let environment ← mkFreshExprMVar (mkSort level.succ)
  let error ← mkFreshExprMVar (mkSort (.succ .zero))
  let success ← mkFreshExprMVar (mkSort (.succ .zero))
  let environmentSyntax ← Term.exprToSyntax environment
  let errorSyntax ← Term.exprToSyntax error
  let emptyErrorSyntax ← Term.exprToSyntax (mkConst ``Empty)
  let collected ← collectActions sequence.raw environmentSyntax errorSyntax
    emptyErrorSyntax
  let sequence : DoSeq := ⟨collected.raw⟩
  let expected : ExpectedZ := { level, environment, error, success }
  let continuation ← DoElemCont.mkPure success
  let effect ← withZMonad expected <|
    enterFinally success <| elabDoSeq sequence continuation
  let inferredEnvironment ←
    inferEnvironment collected.environmentRequirements
  unless ← isDefEq environment inferredEnvironment do
    throwErrorAt sequence "failed to infer a `zdo` finalizer environment"
  let inferredError ← inferError collected.errorRequirements
  unless ← isDefEq error inferredError do
    throwErrorAt sequence "failed to infer a `zdo` finalizer error type"
  return {
    effect
    environment := ← instantiateMVars environment
    error := ← instantiateMVars error
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
    (handler : ScopedHandler) : DoElabM ScopedSequence := do
  let bodyLevel ← getDecLevel body.environment
  let handlerLevel ← getDecLevel handler.environment
  let (combinedEnvironment, meet) ←
    inferEnvironmentMeet body.environment handler.environment
  let combinedLevel ← getDecLevel combinedEnvironment
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
  return {
    effect
    environment := combinedEnvironment
    error := handler.error
    lifter := body.lifter
  }

private def combineScopedFinally
    (body : ScopedSequence)
    (finalizer : ScopedFinalizer) : DoElabM ScopedSequence := do
  let bodyLevel ← getDecLevel body.environment
  let finalizerLevel ← getDecLevel finalizer.environment
  let (combinedEnvironment, meet) ←
    inferEnvironmentMeet body.environment finalizer.environment
  let combinedLevel ← getDecLevel combinedEnvironment
  let combinedErrorLevel ← mkFreshLevelMVar
  let combinedError ← mkFreshExprMVar (mkSort combinedErrorLevel.succ)
  let joinType := mkApp3
    (mkConst ``ErrorChannel.Join [.zero, .zero, combinedErrorLevel])
    body.error finalizer.error combinedError
  let join ← Term.mkInstMVar joinType
  let packedType := body.lifter.liftedDoBlockResultType
  let finalizerSuccess ← match ← expectedZ? (← inferType finalizer.effect) with
    | some expected => pure expected.success
    | none => throwError "a scoped finalizer must be a `Z` effect"
  let effect := mkAppN
    (mkConst ``Z.ensuringMeetJoin
      [bodyLevel, finalizerLevel, combinedLevel]) #[
      body.environment,
      body.error,
      packedType,
      body.effect,
      finalizer.environment,
      combinedEnvironment,
      finalizer.error,
      combinedError,
      finalizerSuccess,
      meet,
      join,
      finalizer.effect]
  return {
    effect
    environment := combinedEnvironment
    error := combinedError
    lifter := body.lifter
  }

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
  if catches.isEmpty && finallySequence?.isNone then
    throwErrorAt original
      "a native `try` requires a catch or finally clause"
  checkMutVarsForShadowing <| catches.filterMap (fun
    | `(doCatch| catch $name:ident $[: $_]? => $_) => some name
    | _ => none)
  let controlInfo ← inferControlInfoElem original
  let body ← elabScopedSequence bodySequence continuation controlInfo
  let caught ← catches.foldlM (init := body) fun current clause => do
      let catchClause : TSyntax ``doCatch ← match clause with
        | `(doCatchMatch| catch $alternatives) =>
            `(doCatch| catch error => match error with $alternatives)
        | `(doCatch| $catchClause) => pure catchClause
      let handler ← elabScopedHandler catchClause current.error
        continuation controlInfo
      combineScopedCatch current handler
  let completed ← match finallySequence? with
    | none => pure caught
    | some finallySequence => do
        let finalizer ← elabScopedFinalizer finallySequence
        combineScopedFinally caught finalizer
  let environmentRequirement ←
    Term.elabType environmentRequirementSyntax
  unless ← isDefEq environmentRequirement completed.environment do
    throwErrorAt stx "failed to collect a scoped `zdo` environment"
  let errorRequirement ← Term.elabType errorRequirementSyntax
  unless ← isDefEq errorRequirement completed.error do
    throwErrorAt stx "failed to collect a scoped `zdo` error type"
  let packedType := completed.lifter.liftedDoBlockResultType
  let targetEnvironment ← Term.elabType targetEnvironmentSyntax
  let targetError ← Term.elabType targetErrorSyntax
  let targetLevel ← getDecLevel targetEnvironment
  let combinedEnvironment ← instantiateMVars completed.environment
  let handlerError ← instantiateMVars completed.error
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
    (mkConst ``Z.widenWithErrorInjection [targetLevel, combinedLevel]) #[
      targetEnvironment,
      combinedEnvironment,
      handlerError,
      targetError,
      packedType,
      environmentInstance,
      errorInstance,
      completed.effect]
  let currentType := mkApp (← read).monadInfo.m packedType
  let adapted ← Term.ensureHasType currentType adapted
  let restoreExpected : ExpectedZ := {
    level := targetLevel
    environment := targetEnvironment
    error := targetError
    success := (← read).doBlockResultType
  }
  withZMonad restoreExpected do
    let restored ← completed.lifter.restoreCont
    (restored.withDeadCodeFromInfo controlInfo).mkBindUnlessPure adapted

private def elabZDoFixed
    (sequence : DoSeq)
    (expectedType : Expr)
    (expected : ExpectedZ) : TermElabM Expr := do
  let environment ← Term.exprToSyntax expected.environment
  let error ← Term.exprToSyntax expected.error
  let collected ← collectActions sequence.raw environment error error
  let sequence : DoSeq := ⟨collected.raw⟩
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

/-- Elaborate `zdo`, inferring its environment and error channel when needed. -/
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

/-- Elaborate `zdo[E]` with an explicit error channel and inferred environment. -/
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
    errorSyntax
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
