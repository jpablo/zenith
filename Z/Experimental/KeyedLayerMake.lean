import Z.Experimental.StableServiceKeys

/-!
Automatic construction for keyed layers.

`KeyedLayer.make (outputs) [layers]` finds one provider for each requested
service, infers the external input row and normalized error channel, and emits
ordinary `KeyedLayer` compositions. `KeyedLayer.make [layers]` keeps the
complete expected-type form for an explicitly selected input or error type.
The dependency graph exists only while Lean elaborates the term.

`Z.provide` reads the program's keyed environment, infers the graph input and
the joined program-and-layer error channel, and scopes the generated layer
around the program.

`#keyed_layer_graph (outputs) [layers]` infers the graph input and error type
and prints the selected graph without building a value. An explicit target
`KeyedLayer` type keeps the boundary form.
-/

open Lean Meta Elab Term Command

namespace StableServiceKeys

syntax (name := keyedLayerMake)
  "KeyedLayer.make" "[" term,* "]" : term

syntax (name := keyedLayerMakeInferred)
  "KeyedLayer.make" "(" term ")" "[" term,* "]" : term

syntax (name := zProvide)
  "Z.provide" term:max "[" term,* "]" : term

syntax (name := keyedLayerGraph)
  "#keyed_layer_graph" "(" term ")" "[" term,* "]" : command

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
  errorType : Expr
  outputRow : Expr
  outputs : Array Expr
  deriving Inhabited

private structure Plan where
  candidate : Nat
  dependencies : Array Nat

private structure Resolution where
  external : Array Expr
  plans : Array Plan

private structure Analysis where
  expected : KeyedLayerType
  inputRow : Expr
  external : Array Expr
  requested : Array Expr
  candidates : Array Candidate
  roots : Array Nat
  plans : Array Plan
  provider : Expr

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
  let service ← whnf (← mkAppM ``Entry.Service #[entry])
  return m!"{service}"

private def candidateAt
    (candidates : Array Candidate)
    (index : Nat) : Candidate :=
  candidates[index]!

private def planContains (plans : Array Plan) (index : Nat) : Bool :=
  plans.any fun plan => plan.candidate == index

private partial def resolveCandidate
    (candidates : Array Candidate)
    (fixedExternal : Bool)
    (index : Nat)
    (path : List Nat)
    (resolution : Resolution) : TermElabM Resolution := do
  if planContains resolution.plans index then
    return resolution
  if path.contains index then
    let candidate := candidateAt candidates index
    throwErrorAt candidate.stx
      "automatic keyed-layer construction found a dependency cycle"
  let candidate := candidateAt candidates index
  let mut dependencies := #[]
  let mut resolution := resolution
  for input in candidate.inputs do
    match ← findByKey resolution.external input with
    | some externalEntry =>
        unless ← sameEntry externalEntry input do
          throwErrorAt candidate.stx m!
            "external service {(← entryMessage externalEntry)} has the same key as {(← entryMessage input)}, but it has a different service type"
    | none =>
        let providers ← providersFor candidates input
        if providers.isEmpty then
          if fixedExternal then
            throwErrorAt candidate.stx m!
              "no layer provides required service {(← entryMessage input)}"
          else
            resolution := {
              resolution with
              external := resolution.external.push input
            }
        else if providers.size > 1 then
          throwErrorAt candidate.stx m!
            "more than one layer provides required service {(← entryMessage input)}"
        else
          let (providerIndex, providedEntry) := providers[0]!
          unless ← sameEntry providedEntry input do
            throwErrorAt candidate.stx m!
              "provided service {(← entryMessage providedEntry)} has the requested key, but it has a different service type from {(← entryMessage input)}"
          unless dependencies.contains providerIndex do
            dependencies := dependencies.push providerIndex
  for dependency in dependencies do
    resolution ← resolveCandidate candidates fixedExternal dependency
      (index :: path) resolution
  return {
    resolution with
    plans := resolution.plans.push { candidate := index, dependencies }
  }

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

private def normalizeRow
    (entryType : Expr)
    (entries : Array Expr) : MetaM Expr := do
  let mut row ← mkListLit entryType []
  for entry in entries do
    row ← mkAppM ``Row.insert #[entry, row]
  let normalized ← rowEntries row
  mkListLit entryType normalized.toList

private partial def flattenError
    (errorType : Expr) : TermElabM (Array Expr) := do
  let errorType ← whnf (← instantiateMVars errorType)
  if errorType.isConstOf ``Empty then
    return #[]
  if errorType.isAppOfArity ``Sum 2 then
    let left ← flattenError errorType.getAppArgs[0]!
    let right ← flattenError errorType.getAppArgs[1]!
    return left ++ right
  return #[errorType]

private def normalizeErrors
    (errorTypes : Array Expr) : TermElabM (Array Expr) := do
  let flattened ← errorTypes.foldlM (init := #[])
    fun result errorType => do
      return result ++ (← flattenError errorType)
  return Lean.sortExprs flattened |>.1

private def joinErrors (errorTypes : Array Expr) : TermElabM Expr := do
  let normalized ← normalizeErrors errorTypes
  normalized.reverse.foldlM (init := mkConst ``Empty)
    fun right left => do
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
        (KeyedLayer.zipFreshPar $current $right (by decide));)
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

private def analyze
    (reference : Syntax)
    (caller : String)
    (expectedType : Expr)
    (layers : Array Term)
    (additionalErrors : Array Expr := #[]) : TermElabM Analysis := do
  let expectedType ← instantiateMVars expectedType
  let some expected ← keyedLayerType? expectedType |
    throwErrorAt reference m!
      "{caller} requires a `KeyedLayer` type"
  if ← hasAssignableMVar expected.output then
    throwErrorAt reference m!
      "{caller} requires a known output row"
  let inferInput := expected.input.isMVar
  let inferError := expected.errorType.isMVar
  let initialExternal ←
    if inferInput then
      pure #[]
    else
      let some inputRow ← environmentRow? expected.input |
        throwErrorAt reference m!
          "{caller} requires a keyed `Environment` input"
      rowEntries inputRow
  let requested ← rowEntries expected.output
  if requested.isEmpty then
    throwErrorAt reference m!
      "{caller} requires at least one requested output service"
  let mut candidates := #[]
  for layer in layers do
    let expression ← Term.elabTerm layer none
    Term.synthesizeSyntheticMVarsNoPostponing
    let type ← instantiateMVars (← inferType expression)
    let some candidateType ← keyedLayerType? type |
      throwErrorAt layer
        "an automatic layer candidate must have type `KeyedLayer R E entries`"
    let some candidateInputRow ← environmentRow? candidateType.input |
      throwErrorAt layer
        "an automatic layer candidate must use a keyed `Environment` input"
    let inputs ← rowEntries candidateInputRow
    let outputs ← rowEntries candidateType.output
    let index := candidates.size
    candidates := candidates.push {
      index
      stx := layer
      inputs
      errorType := candidateType.errorType
      outputRow := candidateType.output
      outputs
    }
  let mut roots := #[]
  for output in requested do
    let providers ← providersFor candidates output
    if providers.isEmpty then
      throwErrorAt reference m!
        "no layer provides requested service {(← entryMessage output)}"
    if providers.size > 1 then
      throwErrorAt reference m!
        "more than one layer provides requested service {(← entryMessage output)}"
    let (providerIndex, providedEntry) := providers[0]!
    unless ← sameEntry providedEntry output do
      throwErrorAt reference m!
        "provided service {(← entryMessage providedEntry)} has the requested key, but it has a different service type from {(← entryMessage output)}"
    unless roots.contains providerIndex do
      roots := roots.push providerIndex
  let mut resolution : Resolution := {
    external := initialExternal
    plans := #[]
  }
  for root in roots do
    resolution ← resolveCandidate candidates (!inferInput) root [] resolution
  for plan in resolution.plans do
    checkDisjointOutputs candidates plan.dependencies reference
  checkDisjointOutputs candidates roots reference
  for candidate in candidates do
    unless planContains resolution.plans candidate.index do
      logWarningAt candidate.stx "unused automatic layer candidate"
  let entryType ← inferType requested[0]!
  let inputRow ←
    if inferInput then
      normalizeRow entryType resolution.external
    else
      let some inputRow ← environmentRow? expected.input |
        throwErrorAt reference m!
          "{caller} requires a keyed `Environment` input"
      pure inputRow
  let external ← rowEntries inputRow
  let inputType ← mkAppM ``Environment #[inputRow]
  if inferInput then
    unless ← isDefEq expected.input inputType do
      throwErrorAt reference m!
        "{caller} could not assign the inferred input row"
  let mut errorTypes := additionalErrors
  for plan in resolution.plans do
    errorTypes := errorTypes.push (candidateAt candidates plan.candidate).errorType
  if inferError then
    let inferredError ← joinErrors errorTypes
    unless ← isDefEq expected.errorType inferredError do
      throwErrorAt reference m!
        "{caller} could not assign the inferred error type"
  let resolvedExpected : KeyedLayerType := {
    input := ← instantiateMVars expected.input
    errorType := ← instantiateMVars expected.errorType
    output := ← instantiateMVars expected.output
  }
  let availableOutputRow ← mergedOutputRow candidates roots
  let availableOutputs ← rowEntries availableOutputRow
  let availableRow ← explicitRow availableOutputs
  let requestedRow ← explicitRow requested
  let providerType ← mkAppM ``Environment.CanProvide #[
    availableRow, requestedRow]
  let provider ← synthInstance providerType
  return {
    expected := resolvedExpected
    inputRow
    external
    requested
    candidates
    roots
    plans := resolution.plans
    provider
  }

private def entriesMessage (entries : Array Expr) : MetaM MessageData := do
  if entries.isEmpty then
    return "(none)"
  else
    return MessageData.joinSep (← entries.toList.mapM entryMessage) ", "

private def candidateMessage
    (candidates : Array Candidate)
    (index : Nat) : MessageData :=
  let candidate := candidateAt candidates index
  m!"[{index}] {candidate.stx}"

private def candidatesMessage
    (candidates : Array Candidate)
    (indices : Array Nat) : MessageData :=
  if indices.isEmpty then
    "(none)"
  else
    MessageData.joinSep
      (indices.toList.map fun index => candidateMessage candidates index)
      " | "

private def renderAnalysis (analysis : Analysis) : MetaM MessageData := do
  let mut lines : Array MessageData := #[
    "Keyed layer graph",
    m!"error type: {analysis.expected.errorType}",
    m!"external inputs: {(← entriesMessage analysis.external)}",
    m!"final outputs: {(← entriesMessage analysis.requested)}",
    "selected providers:"
  ]
  for output in analysis.requested do
    let providers ← providersFor analysis.candidates output
    let provider := providers[0]!.1
    lines := lines.push m!
      "  {(← entryMessage output)} <- {candidateMessage analysis.candidates provider}"
  lines := lines.push "selected candidates:"
  for plan in analysis.plans do
    let candidate := candidateAt analysis.candidates plan.candidate
    lines := lines.push m!"  {candidateMessage analysis.candidates plan.candidate}"
    lines := lines.push m!
      "    inputs: {(← entriesMessage candidate.inputs)}"
    lines := lines.push m!
      "    outputs: {(← entriesMessage candidate.outputs)}"
  lines := lines.push "dependency edges:"
  let mut edgeCount : Nat := 0
  for plan in analysis.plans do
    for dependency in plan.dependencies do
      edgeCount := edgeCount + 1
      lines := lines.push m!
        "  {candidateMessage analysis.candidates dependency} -> {candidateMessage analysis.candidates plan.candidate}"
  if edgeCount == 0 then
    lines := lines.push "  (none)"
  lines := lines.push "parallel groups:"
  let mut parallelCount : Nat := 0
  for plan in analysis.plans do
    if plan.dependencies.size > 1 then
      parallelCount := parallelCount + 1
      lines := lines.push m!
        "  inputs of {candidateMessage analysis.candidates plan.candidate}: {candidatesMessage analysis.candidates plan.dependencies}"
  if analysis.roots.size > 1 then
    parallelCount := parallelCount + 1
    lines := lines.push m!
      "  final providers: {candidatesMessage analysis.candidates analysis.roots}"
  if parallelCount == 0 then
    lines := lines.push "  (none)"
  let mut consumers : Array Nat :=
    Array.replicate analysis.candidates.size 0
  for plan in analysis.plans do
    for dependency in plan.dependencies do
      consumers := consumers.set! dependency (consumers[dependency]! + 1)
  lines := lines.push "shared nodes:"
  let mut sharedCount : Nat := 0
  for plan in analysis.plans do
    let count := consumers[plan.candidate]!
    if count > 1 then
      sharedCount := sharedCount + 1
      lines := lines.push m!
        "  {candidateMessage analysis.candidates plan.candidate} ({count} consumers)"
  if sharedCount == 0 then
    lines := lines.push "  (none)"
  lines := lines.push "unused candidates:"
  let mut unusedCount : Nat := 0
  for candidate in analysis.candidates do
    unless planContains analysis.plans candidate.index do
      unusedCount := unusedCount + 1
      lines := lines.push m!"  {candidateMessage analysis.candidates candidate.index}"
  if unusedCount == 0 then
    lines := lines.push "  (none)"
  return MessageData.joinSep lines.toList "\n"

end KeyedLayerMake

@[term_elab keyedLayerMake]
def elabKeyedLayerMake : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.make [$layers,*]) := stx | throwUnsupportedSyntax
  Term.tryPostponeIfNoneOrMVar expectedType?
  let some expectedType := expectedType? | throwErrorAt stx
    "`KeyedLayer.make` requires an expected `KeyedLayer` type"
  let expectedType ← instantiateMVars expectedType
  let analysis ← KeyedLayerMake.analyze stx "`KeyedLayer.make`"
    expectedType layers.getElems
  let generated ← KeyedLayerMake.generateGraph stx
    analysis.candidates analysis.plans analysis.roots
    analysis.inputRow analysis.expected.errorType analysis.expected.output
    analysis.provider
  Term.elabTerm generated expectedType

@[term_elab keyedLayerMakeInferred]
def elabKeyedLayerMakeInferred : TermElab := fun stx expectedType? => do
  let `(KeyedLayer.make ($output) [$layers,*]) := stx |
    throwUnsupportedSyntax
  let output ← Term.elabTerm output none
  Term.synthesizeSyntheticMVarsNoPostponing
  let output ← instantiateMVars output
  let inputLevel ← mkFreshLevelMVar
  let input ← mkFreshExprMVar (mkSort inputLevel.succ)
  let errorType ← mkFreshExprMVar (mkSort (.succ .zero))
  let targetType ← mkAppM ``KeyedLayer #[input, errorType, output]
  let analysis ← KeyedLayerMake.analyze stx "`KeyedLayer.make`"
    targetType layers.getElems
  let resolvedType ← mkAppM ``KeyedLayer #[
    analysis.expected.input,
    analysis.expected.errorType,
    analysis.expected.output]
  let generated ← KeyedLayerMake.generateGraph stx
    analysis.candidates analysis.plans analysis.roots
    analysis.inputRow analysis.expected.errorType analysis.expected.output
    analysis.provider
  let expression ← Term.elabTerm generated resolvedType
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType expression
  | none => pure expression

@[command_elab keyedLayerGraph]
meta def elabKeyedLayerGraph : CommandElab
  | stx@`(#keyed_layer_graph ($target) [$layers,*]) =>
      liftTermElabM do
        let target ← Term.elabTerm target none
        Term.synthesizeSyntheticMVarsNoPostponing
        let target ← instantiateMVars target
        let expectedType ←
          match ← KeyedLayerMake.keyedLayerType? target with
          | some _ => pure target
          | none =>
              let inputLevel ← mkFreshLevelMVar
              let input ← mkFreshExprMVar (mkSort inputLevel.succ)
              let errorType ←
                mkFreshExprMVar (mkSort (.succ .zero))
              mkAppM ``KeyedLayer #[input, errorType, target]
        let analysis ← KeyedLayerMake.analyze stx "`#keyed_layer_graph`"
          expectedType layers.getElems
        logInfoAt stx (← KeyedLayerMake.renderAnalysis analysis)
  | _ => throwUnsupportedSyntax

@[term_elab zProvide]
def elabZProvide : TermElab := fun stx expectedType? => do
  let `(Z.provide $program [$layers,*]) := stx | throwUnsupportedSyntax
  let programExpression ← Term.elabTerm program none
  Term.synthesizeSyntheticMVarsNoPostponing
  let programType ← instantiateMVars (← inferType programExpression)
  let some actual ← KeyedLayerMake.zType? programType |
    throwErrorAt program "the program supplied to `Z.provide` must have type `Z R E A`"
  let some outputRow ←
      KeyedLayerMake.environmentRow? actual.environment |
    throwErrorAt program
      "the program supplied to `Z.provide` must require a keyed `Environment`"
  let expected ←
    match expectedType? with
    | none => pure none
    | some expectedType =>
        let expectedType ← instantiateMVars expectedType
        match ← KeyedLayerMake.zType? expectedType with
        | some expected => pure (some expected)
        | none =>
          if ← hasAssignableMVar expectedType then
            pure none
          else
            throwErrorAt stx
              "`Z.provide` requires an expected `Z R E A` type"
  if let some expected := expected then
    unless ← isDefEq actual.success expected.success do
      throwErrorAt program
        "the program success type does not match the expected `Z.provide` success type"
  let input ←
    match expected with
    | some expected => pure expected.environment
    | none =>
        let level ← mkFreshLevelMVar
        mkFreshExprMVar (mkSort level.succ)
  let errorType ←
    match expected with
    | some expected => pure expected.errorType
    | none => mkFreshExprMVar (mkSort (.succ .zero))
  let layerType ← mkAppM ``KeyedLayer #[input, errorType, outputRow]
  let analysis ← KeyedLayerMake.analyze stx "`Z.provide`"
    layerType layers.getElems #[actual.errorType]
  let success ← instantiateMVars actual.success
  let resultType ← mkAppM ``Z #[
    analysis.expected.input,
    analysis.expected.errorType,
    success]
  let inputSyntax ← Term.exprToSyntax analysis.expected.input
  let errorSyntax ← Term.exprToSyntax analysis.expected.errorType
  let successSyntax ← Term.exprToSyntax success
  let programEnvironmentSyntax ←
    Term.exprToSyntax actual.environment
  let outputSyntax ← Term.exprToSyntax outputRow
  let layer ← KeyedLayerMake.generateGraph stx
    analysis.candidates analysis.plans analysis.roots
    analysis.inputRow analysis.expected.errorType analysis.expected.output
    analysis.provider
  let generated ← `(KeyedLayer.provide
    (show KeyedLayer $inputSyntax $errorSyntax $outputSyntax from
      $layer)
    (show Z $programEnvironmentSyntax $errorSyntax $successSyntax from
      Z.intoJoined $program))
  let expression ← Term.elabTerm generated resultType
  match expectedType? with
  | some expectedType => Term.ensureHasType expectedType expression
  | none => pure expression

end StableServiceKeys
