import Z.Interpreter
import Z.DefaultServices
import Z.Layer
import Z.Do


namespace Fiber
  
  def join  (self: Fiber E A): Z Unit E A := 
    Z.async self.awaitAsync |>.withLabel s!"⌛ ⑂ Fiber.join ({self.fiberId})"

  def interrupt (self: Fiber E A): Z Unit Empty (Exit E A) := do
    Z.succeed self.requestInterrupt |>.withLabel s!"⌛ 🛑 Fiber.interrupted ← true ({self.fiberId})"
    self.join.foldCauseZ (Exit.failure ∘> pure) (Exit.success ∘> pure)

end Fiber
