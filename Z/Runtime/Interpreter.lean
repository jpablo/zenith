import Z.Combinators
import Z.Runtime.Trace
import Z.Runtime.Models
import Z.Runtime.Sequential
import Init.System.Promise

open IO (userError)
-- Enable dot notation for `Fiber` operations.
open Fiber
open Function (const)

namespace ZCore

private initialize nextFiberSerial : IO.Ref Nat ← IO.mkRef 0

private inductive AsyncResumeState
  | registering
  | synchronous (deliver : IO Unit)
  | suspended
  | resumed

private structure AsyncResumeGate where
  state : IO.Ref AsyncResumeState

@[inline] private def AsyncResumeGate.create : IO AsyncResumeGate := do
  pure { state := ← IO.mkRef .registering }

/--
Deliver a resumption. A resumption produced while `register` is still running
is replayed by `finishRegistration` on the fiber's own task, so a synchronous
completion never pays for a task hop.
-/
@[inline] private def AsyncResumeGate.resume
    (self : AsyncResumeGate)
    (deliver : IO Unit) : IO Unit := do
  let runInTask ← self.state.modifyGet fun
    | .registering => (false, .synchronous deliver)
    | .suspended => (true, .resumed)
    | current => (false, current)
  if runInTask then
    let _ ← IO.asTask deliver
    pure ()

/--
Deliver a resumption that cannot wait for `register` to return, such as an
interruption reaching a fiber whose registration blocks.
-/
@[inline] private def AsyncResumeGate.resumeNow
    (self : AsyncResumeGate)
    (deliver : IO Unit) : IO Unit := do
  let runInTask ← self.state.modifyGet fun
    | .registering => (true, .resumed)
    | .suspended => (true, .resumed)
    | current => (false, current)
  if runInTask then
    let _ ← IO.asTask deliver
    pure ()

@[inline] private def AsyncResumeGate.finishRegistration
    (self : AsyncResumeGate) : IO Unit := do
  let synchronousDelivery ← self.state.modifyGet fun
    | .synchronous deliver => (some deliver, .resumed)
    | .registering => (none, .suspended)
    | current => (none, current)
  match synchronousDelivery with
  | some deliver => deliver
  | none => pure ()

/-- Run interpreter bookkeeping and report the `IO.Error` it may raise. -/
@[inline] private def captureDefect (action : IO Unit) : IO (Option IO.Error) := do
  try
    action
    pure none
  catch ioError =>
    pure (some ioError)

@[inline] private def invalidRuntimeInstruction (message : String) : IO Unit :=
  throw (userError message)

private def freshFiberId (parentFiberId : FiberId) (name : String) : IO FiberId := do
  let serial ← nextFiberSerial.modifyGet fun current =>
    (current, current + 1)
  let scope := if parentFiberId.isEmpty then name else s!"{parentFiberId}-{name}"
  pure s!"{scope}-{serial}"

private def interruptChildren (fiberInfos : IO.Ref (List FiberInfo)) : IO Unit := do
  let children ← fiberInfos.get
  for child in children do
    child.interrupt
  for child in children do
    child.await

private def completeStackWithDefect
    (stack : Stack E A E₁ A₁)
    (error : IO.Error) : IO Unit :=
  match stack with
  | .more (tail := tail) .. => completeStackWithDefect tail error
  | .done complete => complete (.failure (.die error))

private def terminateWithDefect
    (fiberInfos : IO.Ref (List FiberInfo))
    (stack : Stack E A E₁ A₁)
    (error : IO.Error) : IO Unit := do
  let finalError ←
    try
      interruptChildren fiberInfos
      pure error
    catch cleanupError =>
      pure <| userError
        s!"{error}\nwhile interrupting the children of the failing fiber: {cleanupError}"
  completeStackWithDefect stack finalError

@[inline] private def freshDiagramNodeId
    (diagram : ExecutionDiagram (IO Unit))
    (state : RunState R E A E₁ A₁) : IO NodeId :=
  if diagram.enabled then state.newId else pure ""

@[inline] private def prepareDiagramNode
    (diagram : ExecutionDiagram (IO Unit))
    (effect : ZCore R E A)
    (nodeId : NodeId) : ZCore R E A :=
  if diagram.enabled then effect.ensureNodeId nodeId else effect

@[inline] private def diagramParentId
    (diagram : ExecutionDiagram (IO Unit))
    (nodeId : NodeId) : Option NodeId :=
  if diagram.enabled then some nodeId else none

mutual

  variable (diagram : ExecutionDiagram (IO Unit))

  /-- Run an effect in a new task and return its fiber. -/
  partial def unsafeRunFiber
      (self : ZCore R E A)
      (env : Environment Rfiber)
      [Environment.CanProvide Rfiber R]
      (parentFiberId : FiberId)
      (name : String)
      (startTime : Nat) : IO (Fiber E A) := do
    let fiberId ← freshFiberId parentFiberId name
    let fiber ← Fiber.empty fiberId
    let loggingEnabled ← RuntimeLog.isEnabled
    let state : RunState .. := {
      interruption := (← fiber.toInterruption)
      fiberInfos := (← IO.mkRef [])
      stack := .done fiber.complete
      environment := env
      fiberId := fiberId
      initialTime := startTime
      loggingEnabled := loggingEnabled
    }
    -- Continue in the background.
    let task ← IO.asTask do
      try
        if state.loggingEnabled then
          RuntimeLog.write fiberId
            "-->  Z.unsafeRunFiber -- starting run loop in a new task"
        self.runLoop state
        if state.loggingEnabled then
          RuntimeLog.write fiberId
            "<-- Z.unsafeRunFiber -- finishing execution\n"
      catch ioError =>
        terminateWithDefect state.fiberInfos state.stack ioError
    fiber.setTask task
    pure fiber

  /-- Run an effect on the current task and wait for its final exit value. -/
  partial def unsafeRunInline
      (self : ZCore R E A)
      (env : Environment Rfiber)
      [Environment.CanProvide Rfiber R]
      (parentFiberId : FiberId)
      (name : String)
      (startTime : Nat) : IO (Exit E A) := do
    let fiberId ← freshFiberId parentFiberId name
    let _ : Nonempty (Exit E A) :=
      ⟨.failure .interrupt⟩
    let completion ← IO.Promise.new (α := Exit E A)
    let interruption : Interruption := {
      interrupted := (← IO.mkRef false)
      isInterruptible := (← IO.mkRef true)
      isInterrupting := false
      interruptDelivered := (← IO.mkRef false)
      interruptHandler := (← IO.mkRef IO.unit)
    }
    let loggingEnabled ← RuntimeLog.isEnabled
    let state : RunState .. := {
      interruption := interruption
      fiberInfos := (← IO.mkRef [])
      stack := .done fun exit => BaseIO.toIO (completion.resolve exit)
      environment := env
      fiberId := fiberId
      initialTime := startTime
      loggingEnabled := loggingEnabled
    }
    try
      if state.loggingEnabled then
        RuntimeLog.write fiberId "-->  Z.unsafeRunInline -- starting run loop"
      self.runLoop state
      if state.loggingEnabled then
        RuntimeLog.write fiberId
          "<-- Z.unsafeRunInline -- waiting for completion\n"
    catch ioError =>
      terminateWithDefect state.fiberInfos state.stack ioError
    match ← IO.wait completion.result? with
    | some exit => pure exit
    | none => throw (userError
        s!"Internal defect: completion promise was dropped for fiber {fiberId}")

  /-- Main interpreter.

  - `diagram`: service that records an execution diagram.
  - `self`: effect evaluated by the current fiber.
  - `R`: environment required by `self`.
  - `Rfiber`: environment available to the current fiber.
  - `validEnv`: evidence that `Rfiber` can provide `R`.
  - `state`: execution stack and other bookkeeping data.

  The match patterns use public API names because the constructors are private
  to `Core.lean`.
  -/
  private partial def runLoop
      (self : ZCore R E A)
      [validEnv : Environment.CanProvide Rfiber R]
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    let selfNodeId ← freshDiagramNodeId diagram state
    let self := prepareDiagramNode diagram self selfNodeId

    let logRunLoop (message : String) :=
      RuntimeLog.write state.fiberId
        s!"[runLoop] [stack: {Stack.size state.stack}] [node: {self.nodeId}] {message}"

    if state.loggingEnabled then
      logRunLoop s!". {self.showHead} ({self.label})"

    let color := diagram.color state.fiberId
    let startedAt ←
      if diagram.enabled then IO.monoMsNow.toIO else pure 0
    let shouldInterrupt ← state.interruption.shouldInterrupt

    if shouldInterrupt then
      if state.loggingEnabled then
        logRunLoop s!"shouldInterrupt: {shouldInterrupt}"
      self.runWithInterruption startedAt state
    else
      -- Send the current effect to the configured execution observer.
      let diagramDefect ← captureDefect do
        if diagram.enabled then
          diagram.currentNode
            self.label (toString self) self.nodeId state.interruption
            state.initialTime startedAt (Stack.size state.stack) color
      if let some ioError := diagramDefect then
        runWithErrorHandler (.die ioError) state
        return

      self.runDispatched startedAt state


  /--
  Execute the `IO` and observability work around one instruction routing
  decision. The state transition for sequential instructions is defined in
  `Sequential.run`.
  -/
  private partial def runDispatched
      (self : ZCore R E A)
      [validEnv : Environment.CanProvide Rfiber R]
      (startedAt : Nat)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    let color := diagram.color state.fiberId
    match Sequential.run self state (diagramParentId diagram self.nodeId) with
    | .resume source exit nextState =>
        match source, exit with
        | .done, .success value =>
            if diagram.enabled then
              diagram.done state.fiberId self.nodeId color "Exit.success"
            continueOrComplete value nextState
        | .done, .failure cause =>
            if diagram.enabled then
              diagram.done state.fiberId self.nodeId color "Exit.failure"
            runWithErrorHandler cause nextState
        | .environment, .success value =>
            continueOrComplete value nextState
        | .environment, .failure cause =>
            runWithErrorHandler cause nextState

    | .evaluate effect nextState nextValidEnv edge => do
        let effectNodeId ← freshDiagramNodeId diagram state
        let effect := prepareDiagramNode diagram effect effectNodeId
        if diagram.enabled then
          match edge with
          | .flatMap _ => diagram.onSuccess self.nodeId effect.nodeId
          | .foldCauseM _ => diagram.onSuccessAndFailure self.nodeId effect.nodeId
          | .contramap => diagram.widenEnv self.nodeId effect.nodeId
          | .provideEnvironment =>
              diagram.provideEnvironment state.fiberId self.nodeId effect.nodeId color
        effect.runLoop (validEnv := nextValidEnv) nextState

    | .unsupported => self.runRuntimeInstruction startedAt state


  /-- Execute the instruction forms that require direct `IO` runtime work. -/
  private partial def runRuntimeInstruction
      (self : ZCore R E A)
      [validEnv : Environment.CanProvide Rfiber R]
      (startedAt : Nat)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    /-
    This dependent eliminator retains the exact indices required by the raw
    `IO` and fiber branches. The sequential branches are rejected here because
    `runDispatched` already routed them through `Sequential.run`.
    -/
    self.casesOn
      (motive := fun R E A _ =>
        Environment.CanProvide Rfiber R ->
        RunState Rfiber E A E₁ A₁ ->
        IO Unit)
      (fun {_ _ _} _ _ _ _ =>
        throw <| userError
          "Internal defect: the runtime dispatcher received a completed instruction")
      (fun {_ R _} io _ validEnv state => do
        let _ : Environment.CanProvide Rfiber R := validEnv
        try
          let result ← io
          if diagram.enabled then
            diagram.syncTry state.fiberId self.nodeId startedAt
          continueOrComplete result state
        catch ioError =>
          let nextEffect := ZCore.done <| .failure <| .die ioError
          nextEffect.runLoop state)
      (fun {E A _} register _ _ state => do
        let resumeGate ← AsyncResumeGate.create
        let deliver (exit : Exit E A) : IO Unit :=
          completeAsyncExit self.nodeId startedAt exit state
        let deliverInterrupt : IO Unit := do
          let interruption ← state.interruption.beginUnwind
          completeAsyncExit self.nodeId startedAt (.failure .interrupt)
            { state with interruption }
        let interrupt := do
          if ← state.interruption.shouldInterrupt then
            resumeGate.resumeNow deliverInterrupt
        let callback (exit : Exit E A) := do
          if ← state.interruption.shouldInterrupt then
            resumeGate.resume deliverInterrupt
          else
            resumeGate.resume (deliver exit)
        state.interruption.interruptHandler.set interrupt
        if ← state.interruption.shouldInterrupt then
          resumeGate.resume deliverInterrupt
        else
          try register callback
          catch ioError =>
            resumeGate.resume (deliver (.failure (.die ioError)))
        resumeGate.finishRegistration)
      (fun {E A _} register _ _ state => do
        let resumeGate ← AsyncResumeGate.create
        let cancelReady ← IO.Promise.new (α := IO Unit)
        let cancelClaimed ← IO.mkRef false
        -- Only the first claimant may run the user cancellation action.
        let claimCancel : IO Bool :=
          cancelClaimed.modifyGet fun claimed => (!claimed, true)
        let deliver (exit : Exit E A) : IO Unit :=
          completeAsyncExit self.nodeId startedAt exit state
        let deliverUnwind (exit : Exit E A) : IO Unit := do
          let interruption ← state.interruption.beginUnwind
          completeAsyncExit self.nodeId startedAt exit
            { state with interruption }
        let runCancel : IO Unit := do
          match ← IO.wait cancelReady.result? with
          | some cancel =>
              try
                cancel
                resumeGate.resumeNow (deliverUnwind (.failure .interrupt))
              catch ioError =>
                resumeGate.resumeNow (deliverUnwind (.failure (.die ioError)))
          | none =>
              resumeGate.resumeNow (deliverUnwind (.failure .interrupt))
        let interrupt := do
          if ← state.interruption.shouldInterrupt then
            if ← claimCancel then
              -- `runCancel` waits for a registration that may still block.
              let _ ← IO.asTask runCancel
              pure ()
        let callback (exit : Exit E A) := do
          if ← state.interruption.shouldInterrupt then
            pure ()
          else if ← claimCancel then
            resumeGate.resume (deliver exit)
        state.interruption.interruptHandler.set interrupt
        if ← state.interruption.shouldInterrupt then
          cancelReady.resolve IO.unit
          if ← claimCancel then
            resumeGate.resume (deliverUnwind (.failure .interrupt))
        else
          try
            let cancel ← register callback
            cancelReady.resolve cancel
            if ← state.interruption.shouldInterrupt then
              interrupt
          catch ioError =>
            cancelReady.resolve IO.unit
            if ← state.interruption.shouldInterrupt then
              if ← claimCancel then
                resumeGate.resume (deliverUnwind (.failure .interrupt))
            else if ← claimCancel then
              resumeGate.resume (deliver (.failure (.die ioError)))
        resumeGate.finishRegistration)
      (fun {_ _ _ _} _ _ _ _ _ =>
        invalidRuntimeInstruction
          "Internal defect: the runtime dispatcher received a flatMap instruction")
      (fun {_ _ _ _ _} _ _ _ _ _ _ =>
        invalidRuntimeInstruction
          "Internal defect: the runtime dispatcher received a foldCauseM instruction")
      (fun {R _ _} effect name _ validEnv state => do
        let _ : Environment.CanProvide Rfiber R := validEnv
        let newFiberBoxId ← freshDiagramNodeId diagram state
        let effectId ← freshDiagramNodeId diagram state
        let effect := prepareDiagramNode diagram effect newFiberBoxId
        let effect :=
          if diagram.enabled then effect.setNodeId effectId else effect
        let fiber ← effect.unsafeRunFiber
          state.environment state.fiberId name state.initialTime
        if diagram.enabled then
          diagram.fork fiber.fiberId self.nodeId effectId startedAt
            state.initialTime newFiberBoxId
        state.fiberInfos.modify (fiber.toFiberInfo :: ·)
        continueOrComplete fiber state)
      (fun {R _ _} effect status _ validEnv state => do
        let _ : Environment.CanProvide Rfiber R := validEnv
        let isInterruptible := state.interruption.isInterruptible
        let oldIsInterruptible ← isInterruptible.get
        isInterruptible.set status.toBool
        let restore := isInterruptible.set oldIsInterruptible
        let effectNodeId ← freshDiagramNodeId diagram state
        let effect := prepareDiagramNode diagram effect effectNodeId
        let nextEffect := effect.ensuringUnmasked
          (.succeed' restore {label := s!"isInterruptible ← {oldIsInterruptible}"})
        let nextEffectNodeId ← freshDiagramNodeId diagram state
        let nextEffect :=
          prepareDiagramNode diagram nextEffect nextEffectNodeId
        if diagram.enabled then
          diagram.setInterruptStatus self.nodeId effect.nodeId nextEffect.nodeId
        nextEffect.runLoop state)
      (fun {_ _ _ _} _ _ _ _ _ =>
        invalidRuntimeInstruction
          "Internal defect: the runtime dispatcher received a contramap instruction")
      (fun {_} _ _ _ =>
        invalidRuntimeInstruction
          "Internal defect: the runtime dispatcher received an environment instruction")
      (fun {_ _ _} _ _ _ _ _ =>
        invalidRuntimeInstruction
          "Internal defect: the runtime dispatcher received a provideEnvironment instruction")
      validEnv state


  private partial def completeAsyncExit
      (nodeId : NodeId)
      (startedAt : Nat)
      (exit : Exit E A)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    let diagramDefect ← captureDefect do
      state.interruption.interruptHandler.set IO.unit
      if diagram.enabled then
        diagram.async state.fiberId nodeId startedAt
    try
      match diagramDefect with
      | some ioError => runWithErrorHandler (.die ioError) state
      | none =>
        match exit with
        | .failure cause => runWithErrorHandler cause state
        | .success value => continueOrComplete value state
    catch ioError =>
      terminateWithDefect state.fiberInfos state.stack ioError


  private partial def runWithErrorHandler
      (cause : Cause E)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    if state.loggingEnabled then
      RuntimeLog.write state.fiberId
        s!"[runWithErrorHandler] [stack: {Stack.size state.stack}]"
    match Sequential.failure cause state with
    | .evaluate nextEffect nextState nextValidEnv edge => do
        let nextEffectNodeId ← freshDiagramNodeId diagram state
        let nextEffect := prepareDiagramNode diagram nextEffect nextEffectNodeId
        if diagram.enabled then
          match edge with
          | .failure parentId => diagram.errorHandler parentId nextEffect.nodeId
          | .success _ =>
              throw <| userError "Internal defect: failure resumed through a success edge"
        nextEffect.runLoop (validEnv := nextValidEnv) nextState
    | .finish exit fiberInfos complete => do
        interruptChildren fiberInfos
        complete exit
    | .resumeFailure mappedCause nextState =>
        runWithErrorHandler mappedCause nextState
    | .invalid =>
        throw <| userError
          "Internal defect: stack entry has no error handler or error-type proof"

  /-- Pass a value to the first continuation, or complete the fiber. -/
  private partial def continueOrComplete
      (value : A)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    match Sequential.success value state with
    | .finish exit fiberInfos complete => do
        if state.loggingEnabled then
          RuntimeLog.write state.fiberId
            s!"[continueOrComplete] [stack: {Stack.size state.stack}] .done"
        interruptChildren fiberInfos
        complete exit
    | .evaluate nextEffect nextState nextValidEnv edge => do
        if state.loggingEnabled then
          RuntimeLog.write state.fiberId
            s!"[continueOrComplete] [stack: {Stack.size state.stack}] .more"
        let nextEffectNodeId ← freshDiagramNodeId diagram state
        let nextEffect := prepareDiagramNode diagram nextEffect nextEffectNodeId
        if diagram.enabled then
          match edge with
          | .success parentId => diagram.continue_ parentId nextEffect.nodeId
          | .failure _ =>
              throw <| userError "Internal defect: success resumed through a failure edge"
        nextEffect.runLoop (validEnv := nextValidEnv) nextState
    | .resumeFailure cause nextState =>
        runWithErrorHandler cause nextState
    | .invalid =>
        throw <| userError "Internal defect: invalid success-resume action"

  private partial def runWithInterruption
      (self : ZCore R E A)
      [validEnv : Environment.CanProvide Rfiber R]
      (startedAt : Nat)
      (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    -- The interrupted box is the current node, which is already in the graph.
    let interruptedBoxId := self.nodeId
    -- Generate a new ID if the interrupted effect runs again.
    let self := if diagram.enabled then self.resetNodeId else self

    let nextEffect : ZCore Unit E Empty :=
      ZCore.failCause Cause.interrupt |>.withLabel "failCause: interrupt"
        |>.withLabel "shouldInterrupt = true"
    let nextEffectNodeId ← freshDiagramNodeId diagram state
    let nextEffect :=
      prepareDiagramNode diagram nextEffect nextEffectNodeId

    if diagram.enabled then
      diagram.interruption
        interruptedBoxId nextEffect.nodeId startedAt state.initialTime

    let interruption ← state.interruption.beginUnwind
    nextEffect.runLoop
      { state with
        interruption := interruption
        stack := .more
          (fun _ : Empty => self)
          none
          (eq_E_E₁? := some (.up rfl))
          state.stack
          none
          (validEnv := validEnv)
          (env := state.environment)
      }

end

end ZCore

open System
open IO

namespace Z

/--
Run a closed Zenith effect with an explicit execution observer.

The default runner uses `ExecutionDiagram.empty`. Import `Zenith.Debug` to use
the Graphviz observer.
-/
def unsafeRunSyncWithDiagram
    (self : Z Unit E A)
    (diagram : ExecutionDiagram (IO Unit))
    (fiberId : FiberId := "main") : IO (Exit E A) := do
  diagram.header
  let startTime ←
    if diagram.enabled then IO.monoMsNow.toIO else pure 0
  let exit ← ZCore.unsafeRunInline
    diagram
    (self.close ())
    Environment.empty
    ""
    fiberId
    startTime
  diagram.footer
  pure exit

/-- Start a closed Zenith effect and return its fiber without waiting. -/
def unsafeFork
    (self : Z Unit E A)
    (fiberId : FiberId := "main") : IO (Fiber E A) := do
  ZCore.unsafeRunFiber
    ExecutionDiagram.empty
    (self.close ())
    Environment.empty
    ""
    fiberId
    0

/-- Run a closed Zenith effect with the fiber interpreter. -/
def unsafeRunSync
    (self : Z Unit E A)
    (fiberId : FiberId := "main") : IO (Exit E A) :=
  unsafeRunSyncWithDiagram self ExecutionDiagram.empty fiberId

end Z
