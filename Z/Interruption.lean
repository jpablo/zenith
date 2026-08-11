/-- State used to control interruption in the main interpreter loop. -/
structure Interruption where
  interrupted : IO.Ref Bool
  isInterruptible : IO.Ref Bool
  isInterrupting : Bool
  interruptHandler : IO.Ref (IO Unit)

def Interruption.toString (self : Interruption) : IO String := do
  let interrupted ← self.interrupted.get
  let isInterruptible ← self.isInterruptible.get
  pure s!"Interruption (interrupted: {interrupted}, isInterruptible: {isInterruptible}, isInterrupting: {self.isInterrupting})"

def Interruption.shouldInterrupt (self : Interruption) : IO Bool := do
  if self.isInterrupting then
    pure false
  else if !(← self.interrupted.get) then
    pure false
  else
    self.isInterruptible.get
