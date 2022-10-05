import Z.ExecutionDiagram


namespace GraphViz
  
  def formatNode [ToString A] (nodeId: NodeId) (a: A) (extra: List (String × String) := []) (color: String := "") (opts: String :=""): String :=
    let extras     := extra.map fun (k,v) => s!"<tr><td align='right'>{k}:</td><td align='left'>{v}</td></tr>"
    let colorAttr  := if color.isEmpty then "" else s!"BGCOLOR=\"{color}\""
    let tableStyle := s!"CELLPADDING=\"4\" BORDER=\"0\" CELLBORDER=\"1\" CELLSPACING=\"0\""
    let label      := s!"<table {tableStyle}><tr><td {colorAttr} colspan='2'><b>{a}</b></td></tr>{String.join extras}</table>"
    s!"\"{nodeId}\" [shape=none, label=<{label}> {opts}]"


  def newId (name: String): IO NodeId := do
    let r <- IO.monoNanosNow.toIO
    return s!"{name}-{r}"

end GraphViz


open System
open IO

/-- Implementation of `ExecutionDiagram` that writes a Graphviz diagram to the specified path -/
def GraphViz.graphvizIO (path: FilePath): IO (ExecutionDiagram (IO Unit)) := do

  let handle <- FS.Handle.mk path FS.Mode.write

  let println txt := 
    FS.Handle.putStrLn handle txt

  let printNode {A} [ToString A] (nodeId: NodeId) (a: A) (extra: List (String × String) := []) (color: String := "") (opts: String :="") := 
    println <| formatNode nodeId a extra color opts

  let printArrow (parentId: NodeId) (newId: NodeId) (opts: String := "") := 
    println s!"  \"{parentId}\" -> \"{newId}\" {opts}"

  let diagram: ExecutionDiagram (IO Unit) := {

    header := println "digraph D {  node [shape=box]"

    footer := println "}"

    errorHandler := fun parentId? nextEffectId => do
      match parentId? with
        | none => IO.unit
        | some parentId =>
          printArrow parentId nextEffectId  s!"[label = \"λ (recover)\"]",

    continue_ := fun parentId? nextEffectId => do
      match parentId? with
        | none => IO.unit
        | some parentId =>
          printArrow parentId nextEffectId  s!"[label = \"λ\"]"

    interruption := fun interruptedBoxId nextEffectId (currentTime initialTime: Nat) => do
      println (formatNode interruptedBoxId s!"⌛ 🛑 interrupted!" [("t", s!"{currentTime - initialTime} ms")] "white")
      printArrow interruptedBoxId nextEffectId "[label = generated]"

    currentNode := fun (label: String) (currentEffectStr: String) currentEffectId (interruption: Interruption) (initialTime currentTime stackSize: Nat) color => do
      let lbl := if label.isEmpty then [] else [("label", label)]
      let ex := [
          ("stack",           toString $ stackSize), 
          -- ("environment",     toString $ envSize), 
          ("isInterruptible", if (<- interruption.isInterruptible.get) then "✅" else "❌"), 
          ("interrupted",     if (<- interruption.interrupted.get)     then "✅" else "❌"), 
          ("isInterrupting",  if interruption.isInterrupting           then "✅" else "❌"), 
          ("t",               s!"{currentTime - initialTime} ms")
        ]
      printNode currentEffectId currentEffectStr (lbl ++ ex) color

    done := fun fiberId currentEffectId color (msg: String) => do
      let exitId <- newId fiberId
      println (formatNode exitId msg [] color)
      printArrow currentEffectId exitId

    syncTry := fun fiberId currentEffectId before => do
      let after <- IO.monoMsNow.toIO
      let resultId <- newId fiberId
      println (formatNode resultId "IO" [("took", s!"{after - before} ms")] "pink")
      printArrow currentEffectId resultId

    onSuccess := fun e1 e2 =>
      printArrow e1 e2

    async := fun fiberId effectId before => do
      let after <- IO.monoMsNow.toIO
      let resultId <- newId fiberId
      println (formatNode resultId "IO" [("took", s!"{after - before} ms")] "pink")
      printArrow effectId resultId s!"[label = \"λ\"]"

    fork := fun (fiberId: FiberId) currentEffectId effectId (currentTime initialTime: Nat) newFiberBoxId => do
      let attrs := [
        ("t", s!"{currentTime - initialTime} ms"), 
        ("fiberId", s!"{fiberId}")
      ]
      println (formatNode newFiberBoxId s!"🧵 new fiber" attrs "white")
      printArrow currentEffectId newFiberBoxId "[color=red, arrowhead=none]"
      printArrow newFiberBoxId effectId "[color=red]"

    onSuccessAndFailure := fun e1 e2 =>
      printArrow e1 e2

    setInterruptStatus := fun e1 e2 e3 => do
      printArrow e1 e2 "[label = original, style = dotted]"
      printArrow e1 e3 "[label = generated]"

    widenEnv := fun e1 e2 =>
      printArrow e1 e2

    provideEnvironment := fun fiberId (currentEffectId effectId: String) color => do
      let envId <- newId fiberId
      println (formatNode envId "Environment" [] color)
      printArrow currentEffectId envId
      printArrow currentEffectId effectId
  }

  return diagram
