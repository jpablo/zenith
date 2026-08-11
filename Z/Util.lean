import Z.Exit


/-- Interpreter logging is disabled unless a program enables it explicitly. -/
def ENABLE_LOG := false

namespace RuntimeLog

private initialize enabled : IO.Ref Bool <- IO.mkRef ENABLE_LOG

def setEnabled (value : Bool) : IO Unit :=
  enabled.set value

def isEnabled : IO Bool :=
  enabled.get

end RuntimeLog

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

def log (fiberId: String) (s: String) (color: String := Color.green) : IO Unit := do
  if <- RuntimeLog.isEnabled then
    try
      IO.eprintln s!"{color}[{fiberId}] {s}{Color.reset}"
    catch _ =>
      pure ()

instance toSEmpty: ToString Empty := 
  ⟨fun _ => "Impossible!"⟩ 


def FiberId := String deriving ToString

structure TLift (α : Type u) : Type (max u v) where
  up :: down : α


def NodeId := String 
  deriving ToString, Repr



@[inline] def Function.andThen (f : A → B) (g : B → C) : A → C :=
  fun x => g (f x)

/-- andThen: (A → B) ∘> (B → C) : A → C  -/
infixl:90 " ∘> "  => Function.andThen
