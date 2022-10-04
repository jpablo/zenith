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

  def consoleWith (f: Console -> Z R E A) : Z R E A :=
    f (DefaultServices.live.get Console)

  def randomWith (f: Random -> Z R E A) : Z R E A :=
    f (DefaultServices.live.get Random)

end Z

namespace Console

  -- def printLine (msg: String) : Z [] IO.Error Unit :=
  --   Z.attempt (IO.println msg) {label := s!"📺 println '{msg}'"}

end Console