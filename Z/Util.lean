import Z.Exit


def ENABLE_LOG := true
-- def ENABLE_LOG := false

def IO.unit : IO Unit := 
  pure ()

namespace Color
  def reset  := "\u001b[0m"

  def black  := "\u001b[30m"
  def red    := "\u001b[31m"
  def green  := "\u001b[32m"
  def yellow := "\u001b[33m"
  def blue   := "\u001b[34m"
  def magenta:= "\u001b[35m"
  def cyan   := "\u001b[36m"
  def white  := "\u001b[37m"
end Color

def log (fiberId: String) (s: String) (color: String := Color.green) := 
  if ENABLE_LOG then IO.println s!"{color}[{fiberId}] {s}{Color.reset}" else IO.unit

instance toSEmpty: ToString Empty := 
  ⟨fun _ => "Impossible!"⟩ 


def FiberId := String deriving ToString

structure TLift (α : Type u) : Type (max u v) where
  up :: down : α


def NodeId := String 
  deriving ToString, Repr



@[inline] def Function.andThen (f : A → B) (g : B → C) : A → C :=
  fun x => g (f x)

/-- andThen  -/
infixl:90 " ∘> "  => Function.andThen
