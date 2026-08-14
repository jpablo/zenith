import Z.Util
import Z.Exit
import Z.Runtime.Interruption
import Init.System.Promise

open IO (userError)

/-- The lifecycle state of a running computation and its listeners. -/
inductive FiberState (E A: Type): Type
  | created

  | running 
    (task     : Task (Except IO.Error Unit))
    (observers: List (Observer E A))

  | done (result: Exit E A)


/-- Report whether the fiber has not yet completed. -/
def FiberState.isRunning (self: FiberState E A) : IO Bool := do 
  match self with
  | .created        => return true
  | .running task .. => BaseIO.toIO ((IO.hasFinished task).map not)
  | .done _         => return false
    

/-- A handle for observing, joining, and interrupting one running computation. -/
structure Fiber (E A: Type) where
  fiberId    : FiberId
  state      : IO.Ref (FiberState E A)
  interrupted: IO.Ref Bool
  interruptDelivered : IO.Ref Bool
  interruptHandler : IO.Ref (IO Unit)
  task       : IO.Ref (Option (Task (Except IO.Error Unit)))
  completion : IO.Promise (Exit E A)


namespace Fiber

  /- Constructors -/

  protected def empty (fiberId: FiberId): IO (Fiber E A) := do
    let _ : Nonempty (Exit E A) :=
      ⟨.failure .interrupt⟩
    return Fiber.mk
      fiberId
      (<- IO.mkRef .created)
      (<- IO.mkRef false)
      (<- IO.mkRef false)
      (<- IO.mkRef IO.unit)
      (<- IO.mkRef none)
      (<- IO.Promise.new (α := Exit E A))


  /- "Methods" -/

  variable (self: Fiber E A) 

  protected def setTask (task: Task (Except IO.Error Unit)) : IO Unit := do
    self.task.set (some task)
    self.state.modify fun
      | .created => .running task []
      | .running _ observers => .running task observers
      | .done result => .done result

  /-- Render mutable fiber state for diagnostics. -/
  def showState: IO String := do
    match (<- self.state.get) with
      | .created => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .created)"
      | .done _  => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .done)"
      | .running task observers => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .running) (hasFinished: {(<- IO.hasFinished task)}) (observers : {observers.length})"

  /-- Wait until this fiber has one final exit value. -/
  def await : IO (Exit E A) := do
    match <- IO.wait self.completion.result? with
    | some exit => pure exit
    | none => throw (userError
        s!"Internal defect: completion promise was dropped for fiber {self.fiberId}")

  /-- Wait until the underlying runtime task is available and completes. -/
  partial def awaitTask (self : Fiber E A) : IO Unit := do
    match <- self.task.get with
      | none => IO.sleep 1 *> awaitTask self
      | some task =>
        let _ <- IO.wait task
        return ()

  /-- Request interruption of this fiber. -/
  def requestInterrupt : IO Unit := do
    self.interruptDelivered.set false
    self.interrupted.set true
    (<- self.interruptHandler.get)


  /--
  If the result is present, return it immediately.
  Otherwise, add `observer` to the current observer list.

  This runs from the parent fiber through `Fiber.join`.
  -/
  protected def awaitAsync (observer: Observer E A) : IO Unit := do
    if <- RuntimeLog.isEnabled then
      RuntimeLog.write self.fiberId
        s!"<-- Fiber.awaitAsync ({<- self.showState})" Color.yellow
    let result? : Option (Option (Exit E A)) <- self.state.modifyGet fun
      | .created => (none, .created)
      | .running task observers => (some none, .running task (observer :: observers))
      | .done result => (some (some result), .done result)
    match result? with
      | none => throw $ userError "Internal defect: Can't await on a non-running fiber"
      | some none => log self.fiberId "Still running, saved observer for later..." Color.yellow
      | some (some result) => observer result

  /--
  Set the state to `.done result` and notify all registered observers.

  This runs in the child fiber.
  -/
  protected def complete : Observer E A :=
    fun (result: Exit E A) => do
      let observers? : Option (List (Observer E A)) <- self.state.modifyGet fun
        | .created => (some [], .done result)
        | .running _ observers => (some observers, .done result)
        | done@(.done _) => (none, done)
      match observers? with
        | none => IO.unit
        | some observers =>
          self.completion.resolve result
          if <- RuntimeLog.isEnabled then
            RuntimeLog.write self.fiberId
              s!"Fiber.complete ({<- self.showState})" Color.yellow
          for observer in observers do
            log self.fiberId s!"complete: calling observers" Color.yellow
            try observer result
            catch _ => pure ()


  /-- Type-erased operations for observing and interrupting a fiber. -/
  structure FiberInfo where
    fiberId    : FiberId
    interrupt  : IO Unit
    interrupted: IO Bool
    await      : IO Unit

  /-- Convert this typed fiber handle to its type-erased diagnostic form. -/
  def toFiberInfo: FiberInfo where 
    fiberId     := self.fiberId
    interrupt   := self.requestInterrupt
    interrupted := self.interrupted.get
    await       := do
      let _ <- self.await
      return ()
  

  /-- Build the interpreter interruption state that belongs to this fiber. -/
  def toInterruption : IO Interruption := do
    return Interruption.mk
      (interrupted := self.interrupted)
      (isInterruptible := <- IO.mkRef true)
      (isInterrupting := false)
      (interruptDelivered := self.interruptDelivered)
      (interruptHandler := self.interruptHandler)

end Fiber
