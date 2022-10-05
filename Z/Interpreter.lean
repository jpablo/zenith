import Z.Combinators
import Z.Colors
import Z.GraphvizDiagram
import Z.InterpreterModels
import Z.Fiber

open IO (userError)
-- Needed to use dot notation on Fibers
open Fiber
open Function (const)

namespace Z

mutual

  variable (diagram: ExecutionDiagram (IO Unit))

  /-- Main interpreter -/
  partial def runLoop (currentEffect: Z Rexp E A) [inst: Rexp ⊂ Rprov] (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    
    /- First ensure we have a nodeId -/
    let (currentEffectId, currentEffect) := currentEffect.ensureNodeId (<- state.newId)

    let llog (msg: String) 
      := log state.fiberId s!"[runLoop] [stack: {Stack.size state.stack}] [node: {currentEffectId}] {msg}"

    llog s!". {currentEffect.showHead} ({currentEffect.label})"

    let i := state.fiberId.hash % Colors.all.size
    let color := Colors.all[i.toNat]!
    let currentTime <- IO.monoMsNow.toIO

    let shouldInterrupt <- state.interruption.shouldInterrupt

    if shouldInterrupt then 
      llog s!"shouldInterrupt: {shouldInterrupt}"
      runWithInterruption currentEffect currentTime state
    else

      /- Write the graphviz node for the current effect -/
      diagram.currentNode 
        currentEffect.label (toString currentEffect) currentEffectId state.interruption state.initialTime currentTime (Stack.size state.stack) color

      let before <- IO.monoMsNow.toIO
      
      /- Note: we need to match on the instance `inst` so that it is propagated in the branches:
        https://leanprover.zulipchat.com/#narrow/stream/270676-lean4/topic/Help.20understanding.20GADTs
      -/
      match currentEffect, inst with

        | .done' (Exit.success value) _, _ =>
          diagram.done state.fiberId currentEffectId color "Exit.success"
          continueOrComplete value state

        | .done' (Exit.failure cause) _, _ => 
          diagram.done state.fiberId currentEffectId color "Exit.failure"
          runWithErrorHandler cause state
        
        | .succeed' io _, _ =>  
          EStateM.tryCatch 
            (do 
              let result <- io
              diagram.syncTry state.fiberId currentEffectId before
              continueOrComplete result state
            )
            (fun ioError => 
              runLoop (.done $ .failure $ .die ioError) state)


        | .flatMap effect next _, inst =>
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          diagram.onSuccess currentEffectId effectId
          -- An `onSuccess` node doesn't change the error type; we take advantage of this fact to capture the proposition `E = E₁` 
          -- and store it in the stack so that it can be used when finding the next error handler.
          let stack' := .more (E₁ := E) next none (eq_E_E₁? := some (.up rfl)) state.stack (parentId := currentEffectId) (validEnv := inst) (env := state.environment)
          -- pseudocode: let stack' := stack.push next
          runLoop effect {state with stack := stack'}
          

        /- Important special case: registerCallback == Fiber.await -/
        | .async await _, _ =>
          llog "--> | .async"
          await fun
            | .failure e => do
              diagram.async state.fiberId currentEffectId before
              runWithErrorHandler e state

            | .success a => do
              diagram.async state.fiberId currentEffectId before
              continueOrComplete a state 
          llog "<-- | .async"


        | .fork effect name _, _ =>
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          let newFiberBoxId := effectId
          let effectId <- state.newId
          let effect := effect.setNodeId effectId
          /- -------------------------- -/
          /- Launch a new Task -/
          let fiber <- Z.unsafeRunFiber effect state.environment state.fiberId name state.initialTime
          /- -------------------------- -/
          diagram.fork fiber.fiberId currentEffectId effectId currentTime state.initialTime newFiberBoxId
          state.fiberInfos.modify (fiber.toFiberRef :: ·)
          continueOrComplete fiber state


        | .foldCauseZ effect errorHandler next _, inst => 
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          diagram.onSuccessAndFailure currentEffectId effectId

          let stack' := .more next errorHandler none state.stack (parentId := currentEffectId) (validEnv := inst) (env := state.environment)
          runLoop effect {state with stack := stack'}


        | .setInterruptStatus effect status _, _ =>
          let isInterruptible := state.interruption.isInterruptible
          let oldIsInterruptible <- isInterruptible.get
          /- ------------------------------ -/
          isInterruptible.set status.toBool
          /- ------------------------------ -/
          let restore: IO Unit := isInterruptible.set oldIsInterruptible
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          let nextEffect := effect.ensuring (.succeed' restore {label := s!"isInterruptible ← {oldIsInterruptible}"})
          let (nextEffectId, nextEffect) := nextEffect.ensureNodeId (<- state.newId)
          diagram.setInterruptStatus currentEffectId effectId nextEffectId
          runLoop nextEffect state

        | .contramap f effect _, p =>
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          diagram.widenEnv currentEffectId effectId
          runLoop effect (inst := Subset.contramap f) state

        | .environment _ _ , inst => 
          continueOrComplete (inst.get state.environment) state

        | .provideEnvironment effect env _ , inst => 
          let (effectId, effect) := effect.ensureNodeId (<- state.newId)
          diagram.provideEnvironment state.fiberId currentEffectId effectId color
          runLoop effect {state with environment := state.environment ++ env}


  private partial def runWithErrorHandler (cause: Cause E) (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    log state.fiberId  s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    match state.stack with
      | .more _ (some errorHandler) _ tail parentId? validEnv env =>
        -- error handler found, just use it
        let nextEffect := errorHandler cause
        let (nextEffectId, nextEffect) := nextEffect.ensureNodeId (<- state.newId)
        diagram.errorHandler parentId? nextEffectId
        runLoop nextEffect (inst := validEnv) {state with stack := tail, environment := env}

      | .more _ none (some (.up eq_E_E₁)) tail .. => 
        -- No error handler found at the top of the stack; try with the tail.
        let cause₁: Cause E₁ := cause.map (cast eq_E_E₁) 
        runWithErrorHandler cause₁ {state with stack := tail}

      | .more _ none none .. => 
        log state.fiberId "Internal defect: Stack not empty but don't know what do do next. (This should not happen)"

      | .done complete =>
        -- nothing else to do, return control to the user
        complete (.failure cause)
    

  /-- `A` will be passed to the first continuation in the stack  -/
  private partial def continueOrComplete (value: A) (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    let msg := s!"[continueOrComplete] [stack: {Stack.size state.stack}]"
    match state.stack with
      | .done complete  => 
        log state.fiberId s!"{msg} .done"
        complete (.success value)

      | .more next _ _ tail parentId? validEnv env =>
        log state.fiberId s!"{msg} .more"

        let nextEffect := ((next value).ensureNodeId (<- state.newId)).2

        diagram.continue_ parentId? nextEffect.nodeId
        runLoop (currentEffect := nextEffect) (inst := validEnv) {state with stack := tail, environment := env }


  partial def runWithInterruption (currentEffect: Z Rexp E A)  [inst: Rexp ⊂ Rprov] currentTime (state: RunState Rprov E A E₁ A₁) := do
    -- We need to use the current node's Id for the interrupted box, as it is already in the graph.
    let interruptedBoxId := currentEffect.nodeId
    -- reset the current node's Id, it will be re-generated later if needed.
    let currentEffect := currentEffect.resetNodeId

    let (_, (nextEffect: Z Unit _ _))  := 
      Z.failCause Cause.interrupt |>.withLabel "failCause: interrupt"
        |>.withLabel "shouldInterrupt = true"
        |>.ensureNodeId (<- state.newId)
    
    diagram.interruption interruptedBoxId nextEffect.nodeId currentTime state.initialTime

    runLoop
      nextEffect
      { state with
        interruption := {state.interruption with isInterrupting := true}
        stack := .more (fun _ => currentEffect) none (eq_E_E₁? := some (.up rfl)) state.stack none (validEnv := inst) (env := state.environment)
      }



  /-- Runs the given effect in IO and returns a Fiber  -/
  partial def Z.unsafeRunFiber (self: Z Rexp E A) (env: Environment Rprov) [Rexp ⊂ Rprov] (parentFiberId: FiberId) (name: String) (startTime: Nat) : IO (Fiber E A) := do
    let fiberId := s!"{parentFiberId}-{name}-{<- IO.rand 0 100000}"
    let fiber <- Fiber.empty fiberId
    let state: RunState .. := {
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
      runLoop self state
      -- TODO: restore this later!
      -- for fiberRef in (<- fiberInfos.get) do
      --   log fiberId s!" finishing, interrupting child: {fiberRef.fiberId}"
      --   fiberRef.interrupt
      log fiberId s!"<-- Z.unsafeRunFiber -- finishing execution\n"
    fiber.setTask task
    return fiber
end

end Z

/-- 
  Evaluate the given effect `self`.
  Only allow execution of effects without any dependencies `(R := Unit)` 

  - `useDiagram`: If provided will write a graphviz representation of program execution to the given file.

-/
def Z.unsafeRunSync [ToString A] (self: Z Unit E A) (fiberId: FiberId) (useDiagram: Option String := none): IO (Option (Exit E A)) := do
  let diagram :=
    match useDiagram with
      | some file => GraphViz.graphvizIO ⟨file⟩
      | none      => ExecutionDiagram.empty
  diagram.header
  let t0    <- IO.monoMsNow.toIO
  let fiber <- Z.unsafeRunFiber diagram self Environment.empty "" fiberId t0
  let exit  <- fiber.awaitPoll (fiberId := fiberId)
  diagram.footer
  return exit

