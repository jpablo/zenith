
import Lean
import Zenith.Services.Layer

/-!
`service_key entryName : ServiceType` resolves `ServiceType` and uses the full
Lean declaration names of its constructor and type arguments. An abstract type
argument uses its `ServiceKey` witness. A value argument uses the
`ServiceValueKey` function for its type. Normal code does not write an owner
string or construct a key.
-/

namespace Z

open Lean Meta Elab Command Term
open Parser.Term

syntax (name := serviceKeyDecl)
  "service_key " ident " : " term : command

syntax (name := serviceRowType)
  "ServiceRow[" term,* "]" : term

syntax (name := servicesType)
  "Services[" term,* "]" : term

syntax (name := serviceKeyTerm)
  "serviceKey[" term "]" : term

syntax (name := keyedLayerFromLayerType)
  "KeyedLayer.fromLayer" term:arg : term

syntax (name := keyedLayerSucceedType)
  "KeyedLayer.succeed" term:arg : term

syntax (name := keyedLayerDeriveConstructor)
  "KeyedLayer.derive" term:arg : term

syntax (name := keyedServiceWithType)
  "Z.serviceWithType" "(" term ")" term:arg : term

syntax (name := keyedServiceWithMType)
  "Z.serviceWithMType" "(" term ")" term:arg : term

syntax (name := servicesGetType)
  "Services.get" "[" term "]" term:arg : term

macro_rules
  | `(Z.serviceWith[$serviceType] $operation) =>
      `(Z.serviceWithType ($serviceType) $operation)
  | `(Z.serviceWithM[$serviceType] $operation) =>
      `(Z.serviceWithMType ($serviceType) $operation)

private partial def keySyntaxForType
    (reference : Syntax)
    (type : Expr) : TermElabM (TSyntax `term) := do
  let type ← whnf (← instantiateMVars type)
  let typeFunction := type.getAppFn
  if typeFunction.isFVar then
    let typeSyntax ← Term.exprToSyntax type
    return ← `(ServiceKey.key (Service := $typeSyntax))
  let .const typeName _ := typeFunction |
    throwErrorAt reference
      "a service key requires a named type constructor or a `ServiceKey` witness"
  let .str owner localName := typeName |
    throwErrorAt reference
      "a service key requires a named Lean declaration"
  let mut argumentKeys : Array (TSyntax `term) := #[]
  for argument in type.getAppArgs do
    let argumentType ← whnf (← inferType argument)
    if argumentType.isSort then
      argumentKeys := argumentKeys.push
        (← keySyntaxForType reference argument)
    else
      unless ← isType argumentType do
        throwErrorAt reference
          "a service value argument must have a type"
      let argumentSyntax ← PrettyPrinter.delab argument
      let argumentTypeSyntax ← PrettyPrinter.delab argumentType
      let typeKey ← keySyntaxForType reference argumentType
      let payload ← `(ServiceValueKey.key
        (Value := $argumentTypeSyntax) $argumentSyntax)
      argumentKeys := argumentKeys.push
        (← `(Key.value $typeKey $payload))
  let owner := Syntax.mkStrLit <| match owner with
    | .anonymous => ""
    | owner => owner.toString
  let localName := Syntax.mkStrLit localName
  `(Key.named $owner $localName [$argumentKeys,*])

@[term_elab serviceKeyTerm]
def elabServiceKeyTerm : TermElab := fun stx expectedType? => do
  let `(serviceKey[$serviceType]) := stx | throwUnsupportedSyntax
  let serviceTypeExpr ← Term.elabType serviceType
  let key ← keySyntaxForType serviceType
    (← instantiateMVars serviceTypeExpr)
  let generated ←
    `(ServiceKey.create (Service := $serviceType) $key)
  Term.elabTerm generated expectedType?

private def deriveServiceKey
    (declarationNames : Array Name) : CommandElabM Bool := do
  let #[typeName] := declarationNames | return false
  let inductiveValue ← getConstInfoInduct typeName
  if inductiveValue.all.length != 1 then
    throwError "mutually inductive service-key derivation is not supported"
  elabCommand <| ← liftTermElabM do
    forallTelescopeReducing inductiveValue.type fun parameters _ => do
      let mut localContext ← getLCtx
      for index in [0:parameters.size] do
        let parameter := parameters[index]!
        let userName := Name.mkSimple s!"serviceKeyParameter{index}"
        localContext := localContext.setUserName
          parameter.fvarId! userName
      withLCtx' localContext do
        let mut instanceBinders := #[]
        let mut valueTypes : Array Expr := #[]
        for parameter in parameters do
          if ← isType parameter then
            let parameterName :=
              mkIdent (← getFVarLocalDecl parameter).userName
            let binder ←
              `(bracketedBinderF| [ServiceKey $parameterName:ident])
            instanceBinders := instanceBinders.push binder
          else
            let parameterType ← whnf (← inferType parameter)
            unless ← isType parameterType do
              throwError
                "a service value parameter must have a type"
            let mut alreadyRequired := false
            for existing in valueTypes do
              if ← isDefEq existing parameterType then
                alreadyRequired := true
            unless alreadyRequired do
              let parameterTypeSyntax ← PrettyPrinter.delab parameterType
              let binder ←
                `(bracketedBinderF|
                  [ServiceValueKey $parameterTypeSyntax])
              instanceBinders := instanceBinders.push binder
              valueTypes := valueTypes.push parameterType
        let parameterNames ← parameters.mapM fun parameter =>
          return mkIdent (← getFVarLocalDecl parameter).userName
        let serviceType ←
          `(@$(mkCIdent typeName) $parameterNames*)
        `(variable $instanceBinders* in
          instance : ServiceKey $serviceType := serviceKey[$serviceType])
  return true

initialize
  registerDerivingHandler ``ServiceKey deriveServiceKey

private def entrySyntaxForType
    (reference : Syntax)
    (serviceType : Term) : TermElabM Term := do
  let serviceTypeExpr ← Term.elabType serviceType
  let key ← keySyntaxForType reference
    (← instantiateMVars serviceTypeExpr)
  `(Entry.create $key $serviceType)

private def entrySyntaxForTypeExpr
    (reference : Syntax)
    (serviceType : Expr) : TermElabM Term := do
  let serviceType ← instantiateMVars serviceType
  let serviceTypeSyntax ← Term.exprToSyntax serviceType
  let key ← keySyntaxForType reference
    serviceType
  `(Entry.create $key $serviceTypeSyntax)

private partial def serviceRowEntries
    (reference : Syntax)
    (row : Expr) : TermElabM (Array Expr) := do
  let row ← whnf row
  if row.getAppFn.isConstOf ``List.nil then
    return #[]
  if row.getAppFn.isConstOf ``List.cons then
    let arguments := row.getAppArgs
    unless arguments.size == 3 do
      throwErrorAt reference "invalid service row"
    return #[arguments[1]!] ++
      (← serviceRowEntries reference arguments[2]!)
  throwErrorAt reference "a service row must reduce to a list"

/-- Reject a stable-key collision before normalization can hide it. -/
private def ensureCompatibleEntries
    (reference : Syntax)
    (entries : Array Expr) : TermElabM Unit := do
  for leftPosition in [0:entries.size] do
    for rightPosition in [leftPosition + 1:entries.size] do
      let left := entries[leftPosition]!
      let right := entries[rightPosition]!
      let leftKey ← mkAppM ``Entry.key #[left]
      let rightKey ← mkAppM ``Entry.key #[right]
      if ← isDefEq leftKey rightKey then
        unless ← isDefEq left right do
          let leftService ← whnf (← mkAppM ``Entry.Service #[left])
          let rightService ← whnf (← mkAppM ``Entry.Service #[right])
          throwErrorAt reference m!
            "service types {leftService} and {rightService} have the same stable key"

private def elaborateServiceRow
    (reference : Syntax)
    (serviceTypes : Array Term) : TermElabM Expr := do
  let mut entries := #[]
  for serviceType in serviceTypes do
    let entrySyntax ← entrySyntaxForType serviceType serviceType
    let entry ← Term.elabTerm entrySyntax none
    entries := entries.push (← instantiateMVars entry)
  let entryType ←
    if entries.isEmpty then
      pure <| Lean.mkConst ``Entry [← mkFreshLevelMVar]
    else
      inferType entries[0]!
  for entry in entries do
    unless ← isDefEq (← inferType entry) entryType do
      throwErrorAt reference
        "all services in one row must use the same universe"
  ensureCompatibleEntries reference entries
  let row ← mkListLit entryType entries.toList
  let normalized ← mkAppM ``Row.normalize #[row]
  let normalizedEntries ← serviceRowEntries reference normalized
  mkListLit entryType normalizedEntries.toList

private def ensureExpectedType
    (expectedType? : Option Expr)
    (expression : Expr) : TermElabM Expr :=
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType expression
  | none => pure expression

private structure DerivedConstructor where
  constructor : Term
  dependencies : Array Term
  output : Term

private def analyzeDerivedConstructor
    (reference : Syntax)
    (constructor : Expr) : TermElabM DerivedConstructor := do
  let constructorType ← inferType constructor
  let (arguments, binderInfos, result) ←
    forallMetaTelescopeReducing constructorType
  let mut dependencies : Array Term := #[]
  for index in [0:arguments.size] do
    unless binderInfos[index]!.isExplicit do continue
    let dependency ← instantiateMVars (← inferType arguments[index]!)
    unless ← isType dependency do
      throwErrorAt reference m!
        "constructor parameter {dependency} is not a service type"
    if dependency.hasExprMVar then
      throwErrorAt reference
        "could not infer a constructor parameter type; apply all type parameters before `KeyedLayer.derive`"
    dependencies := dependencies.push
      (← Term.exprToSyntax dependency)
  let output ← instantiateMVars result
  unless ← isType output do
    throwErrorAt reference m!
      "the constructor result {output} is not a service type"
  if output.hasExprMVar then
    throwErrorAt reference
      "could not infer the constructed service type; apply all type parameters before `KeyedLayer.derive`"
  return {
    constructor := ← Term.exprToSyntax constructor
    dependencies
    output := ← Term.exprToSyntax output
  }

private def elaborateAnalyzedConstructor
    (reference : Syntax)
    (analyzed : DerivedConstructor)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let environment :=
    mkIdentFrom reference `_keyedLayerDeriveEnvironment
  let mut constructed := analyzed.constructor
  for dependency in analyzed.dependencies do
    let argument ← `(Services.get[$dependency] $environment)
    constructed ← `($constructed $argument)
  let generated ←
    `(KeyedLayer.fromLayer
      (show Layer
          (Services[$(analyzed.dependencies),*])
          Empty
          $(analyzed.output) from
        Layer.fromFunction fun $environment => $constructed))
  Term.elabTerm generated expectedType?

private def elaborateDerivedConstructor
    (reference : Syntax)
    (constructor : Expr)
    (expectedType? : Option Expr) : TermElabM Expr := do
  elaborateAnalyzedConstructor reference
    (← analyzeDerivedConstructor reference constructor)
    expectedType?

private def elaborateDerivedStructure
    (reference : Syntax)
    (serviceTypeSyntax : Term)
    (expectedType? : Option Expr) : TermElabM Expr := do
  let serviceType ← whnf (← Term.elabType serviceTypeSyntax)
  let .const serviceName levels := serviceType.getAppFn |
    throwErrorAt reference
      "`KeyedLayer.derive[Service]` requires a structure type"
  unless isStructure (← getEnv) serviceName do
    throwErrorAt reference m!
      "{serviceType} is not a structure type"
  let inductiveValue ← getConstInfoInduct serviceName
  let constructorName := inductiveValue.ctors[0]!
  let parameters :=
    serviceType.getAppArgs.extract 0 inductiveValue.numParams
  let constructor := mkAppN (mkConst constructorName levels) parameters
  let constructorResult ← analyzeDerivedConstructor reference constructor
  let resultType ← Term.elabType constructorResult.output
  unless ← isDefEq resultType serviceType do
    throwErrorAt reference m!
      "the structure constructor produces {resultType}, not {serviceType}"
  elaborateAnalyzedConstructor reference constructorResult expectedType?

private structure ExpectedSingleServiceLayer where
  input : Expr
  errorType : Expr
  service : Expr

private def expectedSingleServiceLayer?
    (expectedType? : Option Expr) :
    TermElabM (Option ExpectedSingleServiceLayer) := do
  let some expectedType := expectedType? | return none
  let expectedType ← instantiateMVars expectedType
  if ← hasAssignableMVar expectedType then return none
  let expectedType ← whnf expectedType
  unless expectedType.isAppOfArity ``KeyedLayer 3 do return none
  let arguments := expectedType.getAppArgs
  let row ← whnf arguments[2]!
  unless row.getAppFn.isConstOf ``List.cons do return none
  let rowArguments := row.getAppArgs
  unless rowArguments.size == 3 do return none
  let tail ← whnf rowArguments[2]!
  unless tail.getAppFn.isConstOf ``List.nil do return none
  let service ← mkAppM ``Entry.Service #[rowArguments[1]!]
  return some {
    input := arguments[0]!
    errorType := arguments[1]!
    service
  }

@[term_elab serviceRowType]
def elabServiceRowType : TermElab := fun stx expectedType? => do
  let `(ServiceRow[$serviceTypes,*]) := stx | throwUnsupportedSyntax
  let row ← elaborateServiceRow stx serviceTypes.getElems
  ensureExpectedType expectedType? row

@[term_elab servicesType]
def elabServicesType : TermElab := fun stx expectedType? => do
  let `(Services[$serviceTypes,*]) := stx | throwUnsupportedSyntax
  let row ← elaborateServiceRow stx serviceTypes.getElems
  let environment ← mkAppM ``Environment #[row]
  ensureExpectedType expectedType? environment

@[term_elab keyedLayerFromLayerType]
def elabKeyedLayerFromLayerType : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.fromLayer $layer) := stx |
    throwUnsupportedSyntax
  let expectedLayer? ← expectedSingleServiceLayer? expectedType?
  let layerExpectedType? ← expectedLayer?.mapM fun expected =>
    mkAppM ``Layer #[expected.input, expected.errorType, expected.service]
  let layer ← Term.elabTerm layer layerExpectedType?
  let layerType ← whnf (← inferType layer)
  unless layerType.isAppOfArity ``Layer 3 do
    throwErrorAt stx
      "`KeyedLayer.fromLayer` requires an ordinary `Layer` value"
  let serviceType := layerType.getAppArgs[2]!
  let entrySyntax ← entrySyntaxForTypeExpr stx serviceType
  let entry ← Term.elabTerm entrySyntax none
  let result ← mkAppM ``KeyedLayer.singleton #[entry, layer]
  ensureExpectedType expectedType? result

@[term_elab keyedLayerSucceedType]
def elabKeyedLayerSucceedType : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.succeed $value) := stx |
    throwUnsupportedSyntax
  let expectedLayer? ← expectedSingleServiceLayer? expectedType?
  let value ← Term.elabTerm value (expectedLayer?.map (·.service))
  let serviceType ← inferType value
  let entrySyntax ← entrySyntaxForTypeExpr stx serviceType
  let entry ← Term.elabTerm entrySyntax none
  let result ← mkAppM ``KeyedLayer.succeedEntry #[entry, value]
  ensureExpectedType expectedType? result

@[term_elab keyedLayerDeriveConstructor]
def elabKeyedLayerDeriveConstructor : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.derive $constructorSyntax) := stx |
    throwUnsupportedSyntax
  -- The argument parser represents `[Service]` as a singleton bracketed term.
  -- Handle this form before it can elaborate as a list value.
  match constructorSyntax with
  | `([$serviceType]) =>
      elaborateDerivedStructure stx serviceType expectedType?
  | _ =>
      let constructor ← Term.elabTerm constructorSyntax none
      Term.synthesizeSyntheticMVarsNoPostponing
      elaborateDerivedConstructor stx constructor expectedType?

@[term_elab keyedServiceWithType]
def elabKeyedServiceWithType : TermElab := fun stx expectedType? => do
  let `(Z.serviceWithType ($serviceType) $operation) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ← `(withServiceEntry $entry $operation)
  match expectedType? with
  | some expectedType =>
      if ← hasAssignableMVar expectedType then
        Term.elabTerm generated none
      else
        Term.elabTerm generated expectedType?
  | none => Term.elabTerm generated none

@[term_elab keyedServiceWithMType]
def elabKeyedServiceWithMType : TermElab := fun stx expectedType? => do
  let `(Z.serviceWithMType ($serviceType) $operation) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ← `(withServiceMEntry $entry $operation)
  match expectedType? with
  | some expectedType =>
      if ← hasAssignableMVar expectedType then
        Term.elabTerm generated none
      else
        Term.elabTerm generated expectedType?
  | none => Term.elabTerm generated none

@[term_elab servicesGetType]
def elabServicesGetType : TermElab := fun stx expectedType? => do
  let `(Services.get[$serviceType] $environment) := stx |
    throwUnsupportedSyntax
  let entry ← entrySyntaxForType serviceType serviceType
  let generated ←
    `(Contains.get (target := $entry) $environment)
  Term.elabTerm generated expectedType?

@[command_elab serviceKeyDecl]
meta def elabServiceKeyDecl : CommandElab
  | `(service_key $entryName:ident : $serviceType:term) => do
      let entry ← liftTermElabM <|
        entrySyntaxForType serviceType serviceType
      elabCommand <| ← `(abbrev $entryName : Entry := $entry)
  | _ => throwUnsupportedSyntax

end Z
