/-- State used to control interruption in the main interpreter loop. -/
structure Interruption where
  interrupted : IO.Ref Bool
  isInterruptible : IO.Ref Bool
  /-- Whether this execution path carries an interrupt cause no handler saw yet. -/
  isInterrupting : Bool
  /-- Whether the pending interrupt request was already turned into a cause. -/
  interruptDelivered : IO.Ref Bool
  interruptHandler : IO.Ref (IO Unit)

/-- Render the current interruption state for diagnostics. -/
def Interruption.toString (self : Interruption) : IO String := do
  let interrupted ← self.interrupted.get
  let isInterruptible ← self.isInterruptible.get
  pure s!"Interruption (interrupted: {interrupted}, isInterruptible: {isInterruptible}, isInterrupting: {self.isInterrupting})"

/-- Report whether the interpreter must now deliver interruption. -/
def Interruption.shouldInterrupt (self : Interruption) : IO Bool := do
  if self.isInterrupting then
    pure false
  else if !(← self.interrupted.get) then
    pure false
  else if ← self.interruptDelivered.get then
    pure false
  else
    self.isInterruptible.get

/--
Consume the pending interrupt request: from now on the interruption travels the
execution stack as a `Cause.interrupt`, so the unwind is not preempted again.
-/
def Interruption.beginUnwind (self : Interruption) : IO Interruption := do
  self.interruptDelivered.set true
  pure { self with isInterrupting := true }

/--
Leave the unwind, because a handler took over the interrupt cause. A handler
that recovers keeps running with a fiber that a later request can interrupt.
-/
def Interruption.endUnwind (self : Interruption) : Interruption :=
  { self with isInterrupting := false }
