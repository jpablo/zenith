import Z.Combinators
import Z.Colors
import Z.GraphvizDiagram
import Z.InterpreterModels

open IO (userError)
-- Needed to use dot notation on Fibers
open Fiber
open Function (const)

namespace Z

mutual

  variable (diagram : ExecutionDiagram (IO Unit))

  /-- 
  Main interpreter.

  - `diagram` : Service to trace execution onto a diagram
  - `self`    : Effect that will be evalauted by the current fiber
  - `R`       : Environment needed to run `self`
  - `Rfiber`  : Environment available in the current fiber
  - `validEnv`: proof that `R` is a component of `Rfiber`
  - `state`   : execution stack and other bookkiping data
  -/
  partial def runLoop (self : Z R E A) [validEnv : R ∣ Rfiber] (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    
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
      match self, validEnv with

        | .done' (Exit.success value) _, _ =>
          diagram.done state.fiberId self.nodeId color "Exit.success"
          continueOrComplete value state

        | .done' (Exit.failure cause) _, _ => 
          diagram.done state.fiberId self.nodeId color "Exit.failure"
          runWithErrorHandler cause state
        
        | .succeed' io _, _ =>  
          EStateM.tryCatch 
            (do 
              let result <- io
              diagram.syncTry state.fiberId self.nodeId t₀
              continueOrComplete result state
            )
            (fun ioError => 
              let nextEffect := Z.done <| .failure <| .die ioError
              nextEffect.runLoop state)


        | .flatMap effect next _, validEnv' =>
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.onSuccess self.nodeId effect.nodeId
          -- An `onSuccess` node doesn't change the error type; we take advantage of this fact to capture the proposition `E = E₁` 
          -- and store it in the stack so that it can be used when finding the next error handler.
          effect.runLoop { state with 
            stack := 
              .more (E₁ := E) next none (eq_E_E₁? := some (.up rfl)) state.stack (parentId := self.nodeId) (validEnv := validEnv') (env := state.environment)
          }
          

        /- Important special case: registerCallback == Fiber.await -/
        | .async await _, _ =>
          await fun
            | .failure e => do
              diagram.async state.fiberId self.nodeId t₀
              runWithErrorHandler e state

            | .success a => do
              diagram.async state.fiberId self.nodeId t₀
              continueOrComplete a state 

        | .fork effect name _, _ =>
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


        | .foldCauseZ effect errorHandler next _, validEnv' => 
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.onSuccessAndFailure self.nodeId effect.nodeId
          effect.runLoop { state with 
            stack := 
              .more next errorHandler none state.stack (parentId := self.nodeId) (validEnv := validEnv') (env := state.environment)
          }


        | .setInterruptStatus effect status _, _ =>
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

        | .contramap f effect _, _ =>
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.widenEnv self.nodeId effect.nodeId
          effect.runLoop (validEnv := IsComponent.contramap f) state

        | .environment _ _ , validEnv' => 
          continueOrComplete (validEnv'.get state.environment) state

        | .provideEnvironment effect env _ , _ => 
          let effect := effect.ensureNodeId (<- state.newId)
          diagram.provideEnvironment state.fiberId self.nodeId effect.nodeId color
          effect.runLoop {state with environment := state.environment ++ env}


  private partial def runWithErrorHandler (cause : Cause E) (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    log state.fiberId  s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    -- looking at the stack to decide what do do next:
    match state.stack with
      -- error handler found, use it to produce the next effect.
      | .more _ (some errorHandler) _ tail parentId? validEnv env =>
        let nextEffect := errorHandler cause |>.ensureNodeId (<- state.newId)
        diagram.errorHandler parentId? nextEffect.nodeId
        nextEffect.runLoop (validEnv := validEnv) { state with stack := tail, environment := env }

      -- No error handler found at the top of the stack; try with the tail.
      | .more _ none (some (.up eq_E_E₁)) tail .. => 
        let cause₁: Cause E₁ := cause.map (cast eq_E_E₁) 
        runWithErrorHandler cause₁ {state with stack := tail}

      | .more _ none none .. => 
        log state.fiberId "Internal defect: Stack not empty but don't know what do do next. (This should not happen)"

      -- nothing else to do, return control to the user
      | .done complete =>
        complete (.failure cause)
    

  /-- `A` will be passed to the first continuation in the stack  -/
  private partial def continueOrComplete (value : A) (state : RunState Rfiber E A E₁ A₁) : IO Unit := do
    let msg := s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    match state.stack with
      | .done complete  => 
        log state.fiberId s!"{msg} .done"
        complete (.success value)

      | .more next _ _ tail parentId? validEnv env =>
        log state.fiberId s!"{msg} .more"
        let nextEffect := next value |>.ensureNodeId (<- state.newId)
        diagram.continue_ parentId? nextEffect.nodeId
        nextEffect.runLoop (validEnv := validEnv) {state with stack := tail, environment := env }


  partial def runWithInterruption (self : Z R E A) [validEnv : R ∣ Rfiber] t₀ (state : RunState Rfiber E A E₁ A₁) := do
    -- We need to use the current node's Id for the interrupted box, as it is already in the graph.
    let interruptedBoxId := self.nodeId
    -- reset the current node's Id, it will be re-generated later if needed.
    let self := self.resetNodeId

    let nextEffect : Z Unit _ _  := 
      Z.failCause Cause.interrupt |>.withLabel "failCause: interrupt"
        |>.withLabel "shouldInterrupt = true"
        |>.ensureNodeId (<- state.newId)
    
    diagram.interruption interruptedBoxId nextEffect.nodeId t₀ state.initialTime

    nextEffect.runLoop
      { state with
        interruption := {state.interruption with isInterrupting := true}
        stack := .more (fun _ => self) none (eq_E_E₁? := some (.up rfl)) state.stack none (validEnv := validEnv) (env := state.environment)
      }



  /-- Runs the given effect in IO and returns a Fiber  -/
  partial def unsafeRunFiber (self : Z R E A) (env : Environment Rfiber) [R ∣ Rfiber] (parentFiberId : FiberId) (name : String) (startTime : Nat) : IO (Fiber E A) := do
    let fiberId := s!"{parentFiberId}-{name}-{<- IO.rand 0 100000}"
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
      log fiberId s!"-->  Z.unsafeRunFiber -- starting run loop in a new task"
      self.runLoop state
      -- TODO: restore this later!
      -- for fiberRef in (<- fiberInfos.get) do
      --   log fiberId s!" finishing, interrupting child: {fiberRef.fiberId}"
      --   fiberRef.interrupt
      log fiberId s!"<-- Z.unsafeRunFiber -- finishing execution\n"
    fiber.setTask task
    return fiber
end

end Z

