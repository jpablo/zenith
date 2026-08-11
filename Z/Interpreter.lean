import Z.Combinators
import Z.Colors
import Z.GraphvizDiagram
import Z.InterpreterModels
import Init.System.Promise

open IO (userError)
-- Needed to use dot notation on Fibers
open Fiber
open Function (const)

namespace ZCore

private initialize nextFiberSerial : IO.Ref Nat <- IO.mkRef 0

private def freshFiberId (parentFiberId : FiberId) (name : String) : IO FiberId := do
  let serial <- nextFiberSerial.modifyGet fun current =>
    (current, current + 1)
  pure s!"{parentFiberId}-{name}-{serial}"

private def interruptChildren (fiberInfos : IO.Ref (List FiberInfo)) : IO Unit := do
  let children <- fiberInfos.get
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
  let finalError <-
    try
      interruptChildren fiberInfos
      pure error
    catch cleanupError =>
      pure cleanupError
  try
    completeStackWithDefect stack finalError
  catch _ =>
    pure ()

mutual

  variable (diagram : ExecutionDiagram (IO Unit))

  /-- 
  Entry point:
  
  Runs the given effect in IO and returns a Fiber  
  -/
  partial def unsafeRunFiber (self : ZCore R E A) (env : Environment Rfiber) [R ∣ Rfiber] (parentFiberId : FiberId) (name : String) (startTime : Nat) : IO (Fiber E A) := do
    let fiberId <- freshFiberId parentFiberId name
    let fiber <- Fiber.empty fiberId
    let state : RunState .. := {
        interruption := (<- fiber.toInterruption)
        fiberInfos   := (<- IO.mkRef [])
        stack        := .done fiber.complete
        environment  := env
        fiberId      := fiberId
        initialTime  := startTime
    }
    -- continue in the background
    let task <- IO.asTask do
      try
        log fiberId s!"-->  Z.unsafeRunFiber -- starting run loop in a new task"
        self.runLoop state
        log fiberId s!"<-- Z.unsafeRunFiber -- finishing execution\n"
      catch ioError =>
        terminateWithDefect state.fiberInfos state.stack ioError
    fiber.setTask task
    return fiber



  /-- 
  Main interpreter.

  - `diagram` : Service to trace execution onto a diagram
  - `self`    : Effect that will be evalauted by the current fiber
  - `R`       : Environment needed to run `self`
  - `Rfiber`  : Environment available in the current fiber
  - `validEnv`: proof that `R` is a component of `Rfiber`
  - `state`   : execution stack and other bookkiping data

  Note that the cases obtained after `match self with ....` correspond not to the Z inductive type itself
  but rather to the public API names.

  The reason is that all the Z constructors are marked as `private`, which means they can't be used outside
  of the file `Core.lean`. 
  -/
  private partial def runLoop (self : ZCore R E A) [validEnv : R ∣ Rfiber] (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    
    /- First ensure we have a nodeId -/
    let self := self.ensureNodeId (<- state.newId)

    let llog (msg : String) 
      := log state.fiberId s!"[runLoop] [stack: {Stack.size state.stack}] [node: {self.nodeId}] {msg}"

    llog s!". {self.showHead} ({self.label})"

    let color := Colors.get state.fiberId
    let t₀ <- IO.monoMsNow.toIO
    let shouldInterrupt <- state.interruption.shouldInterrupt

    if shouldInterrupt then 
      llog s!"shouldInterrupt: {shouldInterrupt}"
      self.runWithInterruption t₀ state
    else
      /- Write the graphviz node for the current effect -/
      diagram.currentNode 
        self.label (toString self) self.nodeId state.interruption state.initialTime t₀ (Stack.size state.stack) color
      
      /- Note: we need to match on the instance `validEnv` so that it is propagated in the branches:
        https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Help.20understanding.20GADTs
      -/
      (match self, validEnv with

        | .done' (Exit.success value) _, _ => do
          diagram.done state.fiberId self.nodeId color "Exit.success"
          continueOrComplete value state

        | .done' (Exit.failure cause) _, _ => do
          diagram.done state.fiberId self.nodeId color "Exit.failure"
          runWithErrorHandler cause state
        
        | .succeed' io _, _ => do
          try
            let result <- io
            diagram.syncTry state.fiberId self.nodeId t₀
            continueOrComplete result state
          catch ioError =>
            let nextEffect := ZCore.done <| .failure <| .die ioError
            nextEffect.runLoop state


        | .flatMap effect next _, validEnv' => do
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.onSuccess self.nodeId effect.nodeId
          -- An `onSuccess` node doesn't change the error type; we take advantage of this fact to capture the proposition `E = E₁` 
          -- and store it in the stack so that it can be used when finding the next error handler.
          effect.runLoop { state with 
            stack := 
              .more (E₁ := E) next none (eq_E_E₁? := some (.up rfl)) state.stack (parentId := self.nodeId) (validEnv := validEnv') (env := state.environment)
          }
          

        /- Important special case: registerCallback == Fiber.await -/
        | .async register _, _ => do
          let resumed <- IO.mkRef false
          let resume (exit : Exit E A) : IO Unit := do
            let isFirst <- resumed.modifyGet fun alreadyResumed => (!alreadyResumed, true)
            if isFirst then
              let _ <- IO.asTask do
                try
                  state.interruption.interruptHandler.set IO.unit
                  diagram.async state.fiberId self.nodeId t₀
                  match exit with
                    | .failure cause => runWithErrorHandler cause state
                    | .success value => continueOrComplete value state
                catch ioError =>
                  terminateWithDefect state.fiberInfos state.stack ioError
              pure ()
          let interrupt := do
            if <- state.interruption.shouldInterrupt then
              resume (.failure .interrupt)
          let callback (exit : Exit E A) := do
            if <- state.interruption.shouldInterrupt then
              resume (.failure .interrupt)
            else
              resume exit
          state.interruption.interruptHandler.set interrupt
          if <- state.interruption.shouldInterrupt then
            interrupt
          else
            try register callback
            catch ioError => resume (.failure (.die ioError))

        | .asyncInterrupt register _, _ => do
          let resumed <- IO.mkRef false
          let cancelReady ← IO.Promise.new (α := IO Unit)
          let resume (exit : Exit E A) : IO Unit := do
            let isFirst <- resumed.modifyGet fun alreadyResumed =>
              (!alreadyResumed, true)
            if isFirst then
              let _ <- IO.asTask do
                try
                  state.interruption.interruptHandler.set IO.unit
                  diagram.async state.fiberId self.nodeId t₀
                  match exit with
                  | .failure cause => runWithErrorHandler cause state
                  | .success value => continueOrComplete value state
                catch ioError =>
                  terminateWithDefect state.fiberInfos state.stack ioError
              pure ()
          let interrupt := do
            if <- state.interruption.shouldInterrupt then
              match ← IO.wait cancelReady.result? with
              | some cancel =>
                  try
                    cancel
                    resume (.failure .interrupt)
                  catch ioError =>
                    resume (.failure (.die ioError))
              | none =>
                  resume (.failure .interrupt)
          let callback (exit : Exit E A) := do
            if <- state.interruption.shouldInterrupt then
              pure ()
            else
              resume exit
          state.interruption.interruptHandler.set interrupt
          if <- state.interruption.shouldInterrupt then
            cancelReady.resolve IO.unit
            interrupt
          else
            try
              let cancel ← register callback
              cancelReady.resolve cancel
              if <- state.interruption.shouldInterrupt then
                interrupt
            catch ioError =>
              cancelReady.resolve IO.unit
              if <- state.interruption.shouldInterrupt then
                resume (.failure .interrupt)
              else
                resume (.failure (.die ioError))

        | ZCore.fork effect name _, _ => do
          let effect := effect.ensureNodeId (<- state.newId)
          let newFiberBoxId := effect.nodeId
          let effectId <- state.newId
          let effect := effect.setNodeId effectId
          /- -------------------------- -/
          /- Launch a new Task -/
          let fiber <- effect.unsafeRunFiber state.environment state.fiberId name state.initialTime
          /- -------------------------- -/
          diagram.fork fiber.fiberId self.nodeId effectId t₀ state.initialTime newFiberBoxId
          state.fiberInfos.modify (fiber.toFiberInfo :: ·)
          continueOrComplete fiber state


        | .foldCauseZ effect errorHandler next _, validEnv' => do
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.onSuccessAndFailure self.nodeId effect.nodeId
          effect.runLoop { state with 
            stack := 
              .more next errorHandler none state.stack (parentId := self.nodeId) (validEnv := validEnv') (env := state.environment)
          }


        | .setInterruptStatus effect status _, _ => do
          let isInterruptible := state.interruption.isInterruptible
          let oldIsInterruptible <- isInterruptible.get
          /- ------------------------------ -/
          isInterruptible.set status.toBool
          /- ------------------------------ -/
          let restore := isInterruptible.set oldIsInterruptible
          let effect  := effect.ensureNodeId (<- state.newId)
          let nextEffect := effect
            |>.ensuring (.succeed' restore {label := s!"isInterruptible ← {oldIsInterruptible}"}) 
            |>.ensureNodeId (<- state.newId)
          diagram.setInterruptStatus self.nodeId effect.nodeId nextEffect.nodeId
          nextEffect.runLoop state

        | .contramap f effect _, _ => do
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.widenEnv self.nodeId effect.nodeId
          effect.runLoop (validEnv := IsComponent.contramap f) state

        | .environment _ _ , validEnv' => do
          continueOrComplete (validEnv'.get state.environment) state

        | .provideEnvironment effect env _ , _ => do
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.provideEnvironment state.fiberId self.nodeId effect.nodeId color
          effect.runLoop {state with environment := state.environment ++ env})


  private partial def runWithErrorHandler (cause : Cause E) (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    log state.fiberId  s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    -- looking at the stack to decide what do do next:
    (match state.stack with
      -- error handler found, use it to produce the next effect.
      | .more _ (some errorHandler) _ tail parentId? validEnv env => do
        let nextEffect := errorHandler cause |>.ensureNodeId (<- state.newId)
        diagram.errorHandler parentId? nextEffect.nodeId
        nextEffect.runLoop (validEnv := validEnv) { state with stack := tail, environment := env }

      -- No error handler found at the top of the stack; try with the tail.
      | .more _ none (some (.up eq_E_E₁)) tail .. => do
        let cause₁: Cause E₁ := cause.map (cast eq_E_E₁) 
        runWithErrorHandler cause₁ {state with stack := tail}

      | .more _ none none .. => do
        throw <| userError
          "Internal defect: stack entry has no error handler or error-type proof"

      -- nothing else to do, return control to the user
      | .done complete => do
        interruptChildren state.fiberInfos
        complete (.failure cause))
    

  /-- `A` will be passed to the first continuation in the stack  -/
  private partial def continueOrComplete (value : A) (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    let msg := s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    (match state.stack with
      | .done complete  => do
        log state.fiberId s!"{msg} .done"
        interruptChildren state.fiberInfos
        complete (.success value)

      | .more next _ _ tail parentId? validEnv env => do
        log state.fiberId s!"{msg} .more"
        let nextEffect := next value |>.ensureNodeId (<- state.newId)
        diagram.continue_ parentId? nextEffect.nodeId
        nextEffect.runLoop (validEnv := validEnv) {state with stack := tail, environment := env })


  private partial def runWithInterruption (self : ZCore R E A) [validEnv : R ∣ Rfiber] t₀ (state : RunState Rfiber E A E₁ A₁) := do
    -- We need to use the current node's Id for the interrupted box, as it is already in the graph.
    let interruptedBoxId := self.nodeId
    -- reset the current node's Id, it will be re-generated later if needed.
    let self := self.resetNodeId

    let nextEffect : ZCore Unit E Empty :=
      ZCore.failCause Cause.interrupt |>.withLabel "failCause: interrupt"
        |>.withLabel "shouldInterrupt = true"
        |>.ensureNodeId (<- state.newId)
    
    diagram.interruption interruptedBoxId nextEffect.nodeId t₀ state.initialTime

    nextEffect.runLoop
      { state with
        interruption := {state.interruption with isInterrupting := true}
        stack := .more (fun _ : Empty => self) none (eq_E_E₁? := some (.up rfl)) state.stack none (validEnv := validEnv) (env := state.environment)
      }

end

end ZCore

open System
open IO

namespace Z

/-- Start a closed Zenith effect and return its fiber without waiting. -/
def unsafeFork
    (self : Z Unit E A)
    (fiberId : FiberId := "main") : IO (Fiber E A) := do
  let startTime <- IO.monoMsNow.toIO
  ZCore.unsafeRunFiber
    ExecutionDiagram.empty
    (self.close ())
    Environment.empty
    ""
    fiberId
    startTime

/-- Run a closed Zenith effect with the fiber interpreter. -/
def unsafeRunSync
    (self : Z Unit E A)
    (fiberId : FiberId := "main")
    (useDiagram : Option String := none) : IO (Exit E A) := do
  let diagram <-
    match useDiagram with
    | some file =>
        pure <| GraphViz.graphvizIO <|
          <- FS.Handle.mk file FS.Mode.write
    | none => pure ExecutionDiagram.empty
  diagram.header
  let startTime <- IO.monoMsNow.toIO
  let fiber <- ZCore.unsafeRunFiber
    diagram
    (self.close ())
    Environment.empty
    ""
    fiberId
    startTime
  let exit <- fiber.await
  fiber.awaitTask
  diagram.footer
  pure exit

end Z
