import Z.Interpreter
import Z.DefaultServices
import Z.Layer


namespace Fiber
  
  def join  (self: Fiber E A): Z Unit E A := 
    Z.async self.awaitAsync |>.withLabel s!"⌛ ⑂ Fiber.join ({self.fiberId})"

  def interrupt (self: Fiber E A): Z Unit Empty (Exit E A) := do
    Z.succeed (self.interrupted.set true) |>.withLabel s!"⌛ 🛑 Fiber.interrupted ← true ({self.fiberId})"
    self.join.foldCauseZ (pure ∘ Exit.failure) (pure ∘ Exit.success)

end Fiber


namespace Z
  /-- 
    Evaluate the given effect `self`.
    Only allow execution of effects without any dependencies `(R := Unit)` 

    - `useDiagram`: If provided will write a graphviz representation of program execution to the given file.

  -/
  def unsafeRunSync [ToString A] (self: Z Unit E A) (fiberId: FiberId) (useDiagram: Option String := none): IO (Option (Exit E A)) := do
    let diagram :=
      match useDiagram with
        | some file => GraphViz.graphvizIO ⟨file⟩
        | none      => ExecutionDiagram.empty
    diagram.header
    let t0    <- IO.monoMsNow.toIO
    let fiber <- unsafeRunFiber diagram self Environment.empty "" fiberId t0
    let exit  <- fiber.awaitPoll (fiberId := fiberId)
    diagram.footer
    return exit

end Z
