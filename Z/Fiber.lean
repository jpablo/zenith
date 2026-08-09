import Z.Util
import Z.Interruption

open IO (userError)

/-- Keeps track of a computation in progress and listeners -/
inductive FiberState (E A: Type): Type
  | created

  | running 
    (task     : Task (Except IO.Error Unit))
    (observers: List (Observer E A))

  | done (result: Exit E A)


def FiberState.isRunning (self: FiberState E A) : IO Bool := do 
  match self with
  | .created        => return true
  | .running task .. => BaseIO.toIO ((IO.hasFinished task).map not)
  | .done _         => return false
    

/-- A `Fiber` is the immutable handle to the mutable FiberState -/
structure Fiber (E A: Type) where
  fiberId    : FiberId
  state      : IO.Ref (FiberState E A)
  interrupted: IO.Ref Bool
  interruptHandler : IO.Ref (IO Unit)
  task       : IO.Ref (Option (Task (Except IO.Error Unit)))


namespace Fiber

  /- Constructors -/

  protected def empty (fiberId: FiberId): IO (Fiber E A) := do
    return Fiber.mk
      fiberId
      (<- IO.mkRef .created)
      (<- IO.mkRef false)
      (<- IO.mkRef IO.unit)
      (<- IO.mkRef none)


  /- "Methods" -/

  variable (self: Fiber E A) 

  protected def setTask (task: Task (Except IO.Error Unit)) : IO Unit := do
    self.task.set (some task)
    self.state.modify fun
      | .created => .running task []
      | .running _ observers => .running task observers
      | .done result => .done result

  def showState: IO String := do
    match (<- self.state.get) with
      | .created => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .created)"
      | .done _  => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .done)"
      | .running task observers => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .running) (hasFinished: {(<- IO.hasFinished task)}) (observers : {observers.length})"

  /-- Use polling for now. When available use a promise or something similar. -/
  partial def awaitPoll (pollMs: UInt32 := 100) (fiberId: FiberId): IO (Option (Exit E A)) := do
    -- dbg_trace s!"({fiberId}) Fiber.await (fiberId: {self.fiberId})"
    match (<- self.state.get) with
      | .created => IO.sleep pollMs *> awaitPoll pollMs fiberId

      | .running task .. =>
        match (<- IO.wait task) with
          | .error ex => return some (.failure (.die ex))
          | _         => IO.sleep pollMs *> awaitPoll pollMs fiberId

      | .done a => return some a

  partial def awaitTask (self : Fiber E A) : IO Unit := do
    match <- self.task.get with
      | none => IO.sleep 1 *> awaitTask self
      | some task =>
        let _ <- IO.wait task
        return ()

  def requestInterrupt : IO Unit := do
    self.interrupted.set true
    (<- self.interruptHandler.get)


  /-- 
  If the result is present, return it immediately.
  Otherwise register the cc `observer` at the top of current observers.

  Note: Evaluated from the parent thread, by way of `Fiber.join`
  -/
  protected def awaitAsync (observer: Observer E A) : IO Unit := do
    log self.fiberId s!"<-- Fiber.awaitAsync ({<- self.showState})" Color.yellow
    let result? : Option (Option (Exit E A)) <- self.state.modifyGet fun
      | .created => (none, .created)
      | .running task observers => (some none, .running task (observer :: observers))
      | .done result => (some (some result), .done result)
    match result? with
      | none => throw $ userError "Internal defect: Can't await on a non-running fiber"
      | some none => log self.fiberId "Still running, saved observer for later..." Color.yellow
      | some (some result) => observer result

  /-- 
  Sets state to `(.done result)` and evaluates all registered observers.
  
  Note: This is evaluated in a child thread. 
  -/
  protected def complete: Observer E A := 
    fun (result: Exit E A) => do
      log self.fiberId s!"Fiber.complete ({<- self.showState})" Color.yellow
      let observers? : Option (List (Observer E A)) <- self.state.modifyGet fun
        | .created => (some [], .done result)
        | .running _ observers => (some observers, .done result)
        | done@(.done _) => (none, done)
      match observers? with
        | none => IO.unit
        | some observers =>
          for observer in observers do
            log self.fiberId s!"complete: calling observers" Color.yellow
            observer result


  /-- Contains some data needed to interact with Fibers without exposing the types `E`, `A`  -/
  structure FiberInfo where
    fiberId    : FiberId
    interrupt  : IO Unit
    interrupted: IO Bool
    await      : IO Unit

  def toFiberInfo: FiberInfo where 
    fiberId     := self.fiberId
    interrupt   := self.requestInterrupt
    interrupted := self.interrupted.get
    await       := do
      let _ <- self.awaitPoll (fiberId := self.fiberId)
      return ()
  

  def toInterruption : IO Interruption := do
    return Interruption.mk 
      self.interrupted 
      (isInterruptible := <- IO.mkRef true) 
      (isInterrupting := false)
      self.interruptHandler

end Fiber
