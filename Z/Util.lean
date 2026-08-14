/-- Interpreter logging is disabled unless a program enables it explicitly. -/
def ENABLE_LOG := false

namespace RuntimeLog

private initialize enabled : IO.Ref Bool <- IO.mkRef ENABLE_LOG

/-- Set logging for fibers that start after this call. -/
def setEnabled (value : Bool) : IO Unit :=
  enabled.set value

/-- Report whether interpreter logging is currently enabled. -/
def isEnabled : IO Bool :=
  enabled.get

end RuntimeLog

/-- The successful unit action, provided for compatibility with older code. -/
def IO.unit : IO Unit :=
  pure ()

namespace Color
  /-- ANSI escape sequence that resets terminal color output. -/
  def reset  := "\u001b[0m"

  /-- ANSI escape sequence for black terminal output. -/
  def black  := "\u001b[30m"
  /-- ANSI escape sequence for red terminal output. -/
  def red    := "\u001b[31m"
  /-- ANSI escape sequence for green terminal output. -/
  def green  := "\u001b[32m"
  /-- ANSI escape sequence for yellow terminal output. -/
  def yellow := "\u001b[33m"
  /-- ANSI escape sequence for blue terminal output. -/
  def blue   := "\u001b[34m"
  /-- ANSI escape sequence for magenta terminal output. -/
  def magenta:= "\u001b[35m"
  /-- ANSI escape sequence for cyan terminal output. -/
  def cyan   := "\u001b[36m"
  /-- ANSI escape sequence for white terminal output. -/
  def white  := "\u001b[37m"
end Color

namespace RuntimeLog

/-- Write a log message after the caller has checked that logging is enabled. -/
def write
    (fiberId : String)
    (message : String)
    (color : String := Color.green) : IO Unit := do
  try
    IO.eprintln s!"{color}[{fiberId}] {message}{Color.reset}"
  catch _ =>
    pure ()

end RuntimeLog

/-- Write a colored fiber log message only when interpreter logging is enabled. -/
def log
    (fiberId : String)
    (message : String)
    (color : String := Color.green) : IO Unit := do
  if ← RuntimeLog.isEnabled then
    RuntimeLog.write fiberId message color

instance toSEmpty : ToString Empty :=
  ⟨fun _ => "Impossible!"⟩


/-- The stable textual identifier assigned to one running fiber. -/
def FiberId := String deriving ToString

structure TLift (α : Type u) : Type (max u v) where
  up :: down : α


/-- The stable textual identifier assigned to one execution-diagram node. -/
def NodeId := String
  deriving ToString, Repr



/-- Compose two functions from left to right. -/
@[inline] def Function.andThen (f : A → B) (g : B → C) : A → C :=
  fun x => g (f x)

/-- andThen: (A → B) ∘> (B → C) : A → C  -/
infixl:90 " ∘> "  => Function.andThen
