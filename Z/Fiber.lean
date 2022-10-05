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


namespace Fiber

  /- Constructors -/

  protected def empty (fiberId: FiberId): IO (Fiber E A) := do
    return Fiber.mk fiberId (<- IO.mkRef .created) (<- IO.mkRef false)


  /- "Methods" -/

  variable (self: Fiber E A) 

  protected def setTask (task: Task (Except IO.Error Unit)) : IO Unit := do
    self.state.set (.running task [])

  def showState: IO String := do
    match (<- self.state.get) with
      | .created => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .created)"
      | .done _  => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .done)"
      | .running task observers => return s!"Fiber: (fiberId: {self.fiberId}) (interrupted: {<- self.interrupted.get}) (state: .running) (hasFinished: {(<- IO.hasFinished task)}) (observers : {observers.length})"

  /-- Use polling for now. When available use a promise or something similar. -/
  partial def awaitPoll (pollMs: UInt32 := 100) (fiberId: FiberId): IO (Option (Exit E A)) := do
    -- dbg_trace s!"({fiberId}) Fiber.await (fiberId: {self.fiberId})"
    if (<- self.interrupted.get) then 
      return none
    else
      match (<- self.state.get) with
        | .created => return none

        | .running task .. => 
          match (<- IO.wait task) with
            | .error ex => return some (.failure (.die ex))
            | _         => IO.sleep pollMs *> awaitPoll pollMs fiberId

        | .done a => return some a


  /-- 
  If the result is present, return it immediately.
  Otherwise register the cc `observer` at the top of current observers.

  Note: Evaluated from the parent thread, by way of `Fiber.join`
  -/
  protected def awaitAsync (observer: Observer E A) : IO Unit := do
    log self.fiberId s!"<-- Fiber.awaitAsync ({<- self.showState})" Color.yellow
    match (<- self.state.get) with
      | .created                => throw $ userError "Internal defect: Can't await on a non-running fiber"
      | .running task observers => 
        log self.fiberId "Still running, saving observer for later..." Color.yellow
        self.state.modify fun _ => .running task (observer :: observers)
      | .done result            => observer result

  /-- 
  Sets state to `(.done result)` and evaluates all registered observers.
  
  Note: This is evaluated in a child thread. 
  -/
  protected def complete: Observer E A := 
    fun (result: Exit E A) => do
      log self.fiberId s!"Fiber.complete ({<- self.showState})" Color.yellow
      match (<- self.state.get) with
        | .created => IO.unit

        | .running _ observers => 
          self.state.set (.done result)
          for observer in observers do
            log self.fiberId s!"complete: calling observers" Color.yellow
            observer result
        
        | .done _ => 
          throw $ userError "Internal defect: Fiber being completed multiple times"


  /-- Contains some data needed to interact with Fibers without exposing the types `E`, `A`  -/
  structure FiberInfo where
    fiberId    : FiberId
    interrupt  : IO Unit
    interrupted: IO Bool

  def toFiberInfo: FiberInfo where 
    fiberId     := self.fiberId
    interrupt   := self.interrupted.set true
    interrupted := self.interrupted.get
  

  def toInterruption : IO Interruption := do
    return Interruption.mk 
      self.interrupted 
      (isInterruptible := <- IO.mkRef true) 
      (isInterrupting := false)

end Fiber
