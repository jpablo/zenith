import Z.Interpreter
import Z.Scope
import Z.DefaultServices
import Z.Layer
import Z.Do
import Z.KeyedLayerMake
import Z.ServiceKeyLaws


namespace Fiber
  
  def join  (self: Fiber E A): Z Unit E A := 
    Z.async self.awaitAsync |>.withLabel s!"⌛ ⑂ Fiber.join ({self.fiberId})"

  /--
  Interrupt `self` and wait for its exit.

  A cause raised while waiting only describes `self` if `self` actually
  finished. Otherwise it belongs to the calling fiber -- interrupting the
  caller must not be reported as the target fiber's exit -- so it propagates.
  -/
  def interrupt (self: Fiber E A): Z Unit Empty (Exit E A) := do
    Z.succeed self.requestInterrupt |>.withLabel s!"⌛ 🛑 Fiber.interrupted ← true ({self.fiberId})"
    self.join.foldCauseZ
      (fun cause => Z.withIO self.state.get fun
        | .done exit => Z.internal.succeedNow exit
        | _ =>
          match cause.failureOrCause with
          | .inl _ => Z.internal.succeedNow (Exit.failure cause)
          | .inr unhandled =>
              (Z.failCause (R := Unit) unhandled).map impossible)
      (Exit.success ∘> pure)

end Fiber
