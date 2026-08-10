import Z.Experimental.StableServiceKeys

/-!
Automatic construction for keyed layers.

`KeyedLayer.make` reads the expected `KeyedLayer` type, finds one provider for
each requested service, and emits ordinary `KeyedLayer` compositions. The
dependency graph exists only while Lean elaborates the term.

`Z.provide` reads the program's keyed environment, constructs that environment
with `KeyedLayer.make`, and scopes the generated layer around the program.
-/

open Lean Meta Elab Term

namespace StableServiceKeys

syntax (name := keyedLayerMake)
  "KeyedLayer.make" "[" term,* "]" : term

syntax (name := zProvide)
  "Z.provide" term:max "[" term,* "]" : term

namespace KeyedLayerMake

private structure KeyedLayerType where
  input : Expr
  errorType : Expr
  output : Expr

private structure ZType where
  environment : Expr
  errorType : Expr
  success : Expr

private structure Candidate where
  index : Nat
  stx : Term
  inputs : Array Expr
  outputRow : Expr
  outputs : Array Expr
  deriving Inhabited

private structure Plan where
  candidate : Nat
  dependencies : Array Nat

private def keyedLayerType? (type : Expr) : MetaM (Option KeyedLayerType) := do
  let type ← whnf type
  let .const name _ := type.getAppFn | return none
  unless name == ``KeyedLayer do return none
  let #[input, errorType, output] := type.getAppArgs | return none
  return some { input, errorType, output }

private def zType? (type : Expr) : MetaM (Option ZType) := do
  let type ← whnf type
  let .const name _ := type.getAppFn | return none
  unless name == ``Z do return none
  let #[environment, errorType, success] := type.getAppArgs | return none
  return some { environment, errorType, success }

private def environmentRow? (type : Expr) : MetaM (Option Expr) := do
  let type ← whnf type
  let .const name _ := type.getAppFn | return none
  unless name == ``Environment do return none
  let #[entries] := type.getAppArgs | return none
  return some entries

private partial def rowEntries (row : Expr) : MetaM (Array Expr) := do
  let row ← whnf row
  let fn := row.getAppFn
  let args := row.getAppArgs
  if fn.isConstOf ``List.nil then
    unless args.size == 1 do
      throwError "invalid empty service row"
    return #[]
  if fn.isConstOf ``List.cons then
    unless args.size == 3 do
      throwError "invalid service row entry"
    return #[args[1]!] ++ (← rowEntries args[2]!)
  throwError m!"a keyed service row must reduce to a list, but found {row}"

private def sameKey (left right : Expr) : MetaM Bool := do
  let leftKey ← mkAppM ``Entry.key #[left]
  let rightKey ← mkAppM ``Entry.key #[right]
  isDefEq leftKey rightKey

private def sameEntry (left right : Expr) : MetaM Bool :=
  isDefEq left right

private def findByKey
    (entries : Array Expr)
    (target : Expr) : MetaM (Option Expr) := do
  for entry in entries do
    if ← sameKey entry target then
      return some entry
  return none

private def providersFor
    (candidates : Array Candidate)
    (target : Expr) : MetaM (Array (Nat × Expr)) := do
  let mut providers := #[]
  for candidate in candidates do
    for output in candidate.outputs do
      if ← sameKey output target then
        providers := providers.push (candidate.index, output)
  return providers

private def entryMessage (entry : Expr) : MetaM MessageData := do
  return m!"{entry}"

private def candidateAt
    (candidates : Array Candidate)
    (index : Nat) : Candidate :=
  candidates[index]!

private def planContains (plans : Array Plan) (index : Nat) : Bool :=
  plans.any fun plan => plan.candidate == index

private partial def resolveCandidate
    (candidates : Array Candidate)
    (external : Array Expr)
    (index : Nat)
    (path : List Nat)
    (plans : Array Plan) : TermElabM (Array Plan) := do
  if planContains plans index then
    return plans
  if path.contains index then
    let candidate := candidateAt candidates index
    throwErrorAt candidate.stx
      "automatic keyed-layer construction found a dependency cycle"
  let candidate := candidateAt candidates index
  let mut dependencies := #[]
  for input in candidate.inputs do
    match ← findByKey external input with
    | some externalEntry =>
        unless ← sameEntry externalEntry input do
          throwErrorAt candidate.stx m!
            "external service {(← entryMessage externalEntry)} has the same key as {(← entryMessage input)}, but it has a different service type"
    | none =>
        let providers ← providersFor candidates input
        if providers.isEmpty then
          throwErrorAt candidate.stx m!
            "no layer provides required service {(← entryMessage input)}"
        if providers.size > 1 then
          throwErrorAt candidate.stx m!
            "more than one layer provides required service {(← entryMessage input)}"
        let (providerIndex, providedEntry) := providers[0]!
        unless ← sameEntry providedEntry input do
          throwErrorAt candidate.stx m!
            "provided service {(← entryMessage providedEntry)} has the requested key, but it has a different service type from {(← entryMessage input)}"
        unless dependencies.contains providerIndex do
          dependencies := dependencies.push providerIndex
  let mut plans := plans
  for dependency in dependencies do
    plans ← resolveCandidate candidates external dependency
      (index :: path) plans
  return plans.push { candidate := index, dependencies }

private def checkDisjointOutputs
    (candidates : Array Candidate)
    (indices : Array Nat)
    (reference : Syntax) : TermElabM Unit := do
  for leftPosition in [0:indices.size] do
    for rightPosition in [leftPosition + 1:indices.size] do
      let left := candidateAt candidates indices[leftPosition]!
      let right := candidateAt candidates indices[rightPosition]!
      for leftOutput in left.outputs do
        for rightOutput in right.outputs do
          if ← sameKey leftOutput rightOutput then
            throwErrorAt reference m!
              "selected layers have overlapping output service {(← entryMessage leftOutput)}"

private def mergedOutputRow
    (candidates : Array Candidate)
    (indices : Array Nat) : MetaM Expr := do
  let mut result := (candidateAt candidates indices[0]!).outputRow
  for position in [1:indices.size] do
    let next := (candidateAt candidates indices[position]!).outputRow
    result ← mkAppM ``Row.merge #[result, next]
  return result

private def explicitRow (entries : Array Expr) : MetaM Expr := do
  let entryType ← inferType entries[0]!
  mkListLit entryType entries.toList

private def nodeIdent (reference : Syntax) (index : Nat) : Ident :=
  mkIdentFrom reference <| Name.mkSimple s!"_keyed_make_node_{index}"

private def zipIdent (reference : Syntax) (index : Nat) : Ident :=
  mkIdentFrom reference <| Name.mkSimple s!"_keyed_make_zip_{index}"

private def addZipBindings
    (reference : Syntax)
    (candidates : Array Candidate)
    (indices : Array Nat)
    (bindings : Array (TSyntax `keyedGraphBinding))
    (nextZip : Nat) : TermElabM
      (Ident × Array (TSyntax `keyedGraphBinding) × Nat) := do
  let mut current := nodeIdent reference indices[0]!
  let mut currentOutput :=
    (candidateAt candidates indices[0]!).outputRow
  let mut bindings := bindings
  let mut nextZip := nextZip
  for position in [1:indices.size] do
    let right := nodeIdent reference indices[position]!
    let rightOutput :=
      (candidateAt candidates indices[position]!).outputRow
    let mergedOutput ← mkAppM ``Row.merge #[currentOutput, rightOutput]
    let mergedEntries ← rowEntries mergedOutput
    let normalizedOutput ← explicitRow mergedEntries
    let providerType ← mkAppM ``Environment.CanProvide #[
      normalizedOutput, normalizedOutput]
    let provider ← synthInstance providerType
    let providerSyntax ← Term.exprToSyntax provider
    let outputSyntax ← Term.exprToSyntax normalizedOutput
    let combined := zipIdent reference nextZip
    let binding ← `(keyedGraphBinding|
      let $combined := KeyedLayer.projectOutput
        (required := $outputSyntax) (provider := $providerSyntax)
        (KeyedLayer.zipFresh $current $right (by decide));)
    bindings := bindings.push binding
    current := combined
    currentOutput := normalizedOutput
    nextZip := nextZip + 1
  return (current, bindings, nextZip)

private def generateGraph
    (reference : Syntax)
    (candidates : Array Candidate)
    (plans : Array Plan)
    (roots : Array Nat)
    (inputRow errorType outputRow provider : Expr) : TermElabM Term := do
  let inputSyntax ← Term.exprToSyntax inputRow
  let errorSyntax ← Term.exprToSyntax errorType
  let outputSyntax ← Term.exprToSyntax outputRow
  let providerSyntax ← Term.exprToSyntax provider
  let mut bindings : Array (TSyntax `keyedGraphBinding) := #[]
  let mut nextZip := 0
  for plan in plans do
    let candidate := candidateAt candidates plan.candidate
    let name := nodeIdent reference plan.candidate
    if plan.dependencies.isEmpty then
      let binding ← `(keyedGraphBinding|
        let $name := KeyedLayer.widenInput
          (available := $inputSyntax) $(candidate.stx);)
      bindings := bindings.push binding
    else
      let (dependencies, nextBindings, next) ←
        addZipBindings reference candidates plan.dependencies bindings nextZip
      bindings := nextBindings
      nextZip := next
      let binding ← `(keyedGraphBinding|
        let $name := $dependencies >>> $(candidate.stx);)
      bindings := bindings.push binding
  let (root, finalBindings, _) ←
    addZipBindings reference candidates roots bindings nextZip
  `(keyed_graph (error := $errorSyntax) {
      $finalBindings:keyedGraphBinding*
      yield KeyedLayer.projectOutput
        (required := $outputSyntax) (provider := $providerSyntax) $root
    })

end KeyedLayerMake

@[term_elab keyedLayerMake]
def elabKeyedLayerMake : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.make [$layers,*]) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | throwErrorAt stx
    "`KeyedLayer.make` requires an expected `KeyedLayer` type"
  let expectedType ← instantiateMVars expectedType
  let some expected ← KeyedLayerMake.keyedLayerType? expectedType |
    throwErrorAt stx
      "`KeyedLayer.make` requires an expected `KeyedLayer` type"
  if ← hasAssignableMVar expected.errorType <||>
      hasAssignableMVar expected.output then
    throwErrorAt stx
      "`KeyedLayer.make` requires known error and output rows"
  let some inputRow ← KeyedLayerMake.environmentRow? expected.input |
    throwErrorAt stx
      "`KeyedLayer.make` requires a keyed `Environment` input"
  let external ← KeyedLayerMake.rowEntries inputRow
  let requested ← KeyedLayerMake.rowEntries expected.output
  if requested.isEmpty then
    throwErrorAt stx
      "`KeyedLayer.make` requires at least one requested output service"
  let mut candidates := #[]
  for layer in layers.getElems do
    let expression ← Term.elabTerm layer none
    Term.synthesizeSyntheticMVarsNoPostponing
    let type ← instantiateMVars (← inferType expression)
    let some candidateType ← KeyedLayerMake.keyedLayerType? type |
      throwErrorAt layer "an automatic layer candidate must have type `KeyedLayer R E entries`"
    let some candidateInputRow ←
        KeyedLayerMake.environmentRow? candidateType.input |
      throwErrorAt layer
        "an automatic layer candidate must use a keyed `Environment` input"
    let inputs ← KeyedLayerMake.rowEntries candidateInputRow
    let outputs ← KeyedLayerMake.rowEntries candidateType.output
    let index := candidates.size
    candidates := candidates.push {
      index
      stx := layer
      inputs
      outputRow := candidateType.output
      outputs
    }
  let mut roots := #[]
  for output in requested do
    let providers ← KeyedLayerMake.providersFor candidates output
    if providers.isEmpty then
      throwErrorAt stx m!
        "no layer provides requested service {(← KeyedLayerMake.entryMessage output)}"
    if providers.size > 1 then
      throwErrorAt stx m!
        "more than one layer provides requested service {(← KeyedLayerMake.entryMessage output)}"
    let (providerIndex, providedEntry) := providers[0]!
    unless ← KeyedLayerMake.sameEntry providedEntry output do
      throwErrorAt stx m!
        "provided service {(← KeyedLayerMake.entryMessage providedEntry)} has the requested key, but it has a different service type from {(← KeyedLayerMake.entryMessage output)}"
    unless roots.contains providerIndex do
      roots := roots.push providerIndex
  let mut plans := #[]
  for root in roots do
    plans ← KeyedLayerMake.resolveCandidate candidates external root [] plans
  for plan in plans do
    KeyedLayerMake.checkDisjointOutputs candidates plan.dependencies stx
  KeyedLayerMake.checkDisjointOutputs candidates roots stx
  for candidate in candidates do
    unless KeyedLayerMake.planContains plans candidate.index do
      logWarningAt candidate.stx "unused automatic layer candidate"
  let availableOutputRow ←
    KeyedLayerMake.mergedOutputRow candidates roots
  let availableOutputs ←
    KeyedLayerMake.rowEntries availableOutputRow
  let availableRow ← KeyedLayerMake.explicitRow availableOutputs
  let requestedRow ← KeyedLayerMake.explicitRow requested
  let providerType ← mkAppM ``Environment.CanProvide #[
    availableRow, requestedRow]
  let provider ← synthInstance providerType
  let generated ← KeyedLayerMake.generateGraph stx candidates plans roots
    inputRow expected.errorType expected.output provider
  Term.elabTerm generated expectedType

@[term_elab zProvide]
def elabZProvide : TermElab := fun stx expectedType? => do
  let `(Z.provide $program [$layers,*]) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | throwErrorAt stx
    "`Z.provide` requires an expected `Z R E A` type"
  let expectedType ← instantiateMVars expectedType
  let some expected ← KeyedLayerMake.zType? expectedType |
    throwErrorAt stx "`Z.provide` requires an expected `Z R E A` type"
  if ← hasAssignableMVar expected.errorType <||>
      hasAssignableMVar expected.success then
    throwErrorAt stx "`Z.provide` requires known error and success types"
  let some _ ← KeyedLayerMake.environmentRow? expected.environment |
    throwErrorAt stx
      "`Z.provide` requires a keyed `Environment` in its expected type"
  let programExpression ← Term.elabTerm program none
  Term.synthesizeSyntheticMVarsNoPostponing
  let programType ← instantiateMVars (← inferType programExpression)
  let some actual ← KeyedLayerMake.zType? programType |
    throwErrorAt program "the program supplied to `Z.provide` must have type `Z R E A`"
  let some outputRow ←
      KeyedLayerMake.environmentRow? actual.environment |
    throwErrorAt program
      "the program supplied to `Z.provide` must require a keyed `Environment`"
  unless ← isDefEq actual.success expected.success do
    throwErrorAt program
      "the program success type does not match the expected `Z.provide` success type"
  let inputSyntax ← Term.exprToSyntax expected.environment
  let errorSyntax ← Term.exprToSyntax expected.errorType
  let successSyntax ← Term.exprToSyntax expected.success
  let programEnvironmentSyntax ←
    Term.exprToSyntax actual.environment
  let outputSyntax ← Term.exprToSyntax outputRow
  let generated ← `(KeyedLayer.provide
    (show KeyedLayer $inputSyntax $errorSyntax $outputSyntax from
      KeyedLayer.make [$layers,*])
    (show Z $programEnvironmentSyntax $errorSyntax $successSyntax from
      Z.intoJoined $program))
  Term.elabTerm generated expectedType

end StableServiceKeys
