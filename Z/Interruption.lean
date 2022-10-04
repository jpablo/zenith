/-- State needed to control interruption in the main loop -/
structure Interruption where
  interrupted    : IO.Ref Bool
  isInterruptible: IO.Ref Bool
  isInterrupting : Bool

def Interruption.toString (i: Interruption): IO String := do
    return s!"Interruption (interrupted: {<- i.interrupted.get}, isInterruptible: {<- i.isInterruptible.get}, isInterrupting: {i.isInterrupting})"

def Interruption.shouldInterrupt (self: Interruption): IO Bool := do
  return (<- self.interrupted.get) && (<- self.isInterruptible.get) && !self.isInterrupting

