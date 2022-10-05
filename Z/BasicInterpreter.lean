import Z.Environment
import Z.InterpreterModels
import Z.Colors
import Z.Fiber


namespace Z
open Fiber


mutual

  variable (diagram: ExecutionDiagram (IO Unit))

  /-- Main interpreter -/
  partial def runLoop1 (currentEffect: Z Rexp E A) [inst: Rexp ⊂ Rprov] (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    
    if (<- state.interruption.shouldInterrupt) then 
      runWithInterruption1 currentEffect (<- IO.monoMsNow.toIO) state
    else
      match currentEffect, inst with

        | .done' (.success value) _, _ => continueOrComplete1 value state

        | .done' (.failure cause) _, _ => runWithErrorHandler1 cause state
        
        | .succeed' io _, _ =>  
          EStateM.tryCatch 
            (do continueOrComplete1 (<- io) state )
            (fun ioError => runLoop1 (.done $ .failure $ .die ioError) state)

        | .flatMap effect next _, inst =>
          runLoop1 effect {state with stack := .more (E₁ := E) next none (eq_E_E₁? := some (.up rfl)) state.stack none (validEnv := inst) (env := state.environment)}
          
        | .async await _, _ =>
          await fun
            | .failure e => runWithErrorHandler1 e state
            | .success a => continueOrComplete1 a state 

        | .fork effect name _, _ =>
          let fiber <- unsafeRunFiber effect state.environment state.fiberId name state.initialTime
          state.fiberInfos.modify (fiber.toFiberRef :: ·)
          continueOrComplete1 fiber state

        | .foldCauseZ effect errorHandler next _, inst => 
          runLoop1 effect {state with stack := .more next errorHandler none state.stack none (validEnv := inst) (env := state.environment) }

        | .setInterruptStatus effect status _, _ =>
          let isInterruptible := state.interruption.isInterruptible
          let oldIsInterruptible <- isInterruptible.get
          /- ------------------------------ -/
          isInterruptible.set status.toBool
          /- ------------------------------ -/
          let restore: IO Unit := isInterruptible.set oldIsInterruptible
          let nextEffect := effect.ensuring (.succeed' restore)
          runLoop1 nextEffect state

        | .contramap f effect _, _ => 
          runLoop1 effect (inst := Subset.contramap f) state

        | .environment _ _ , inst => 
          continueOrComplete1 (inst.get state.environment) state

        | .provideEnvironment effect env _ , inst => 
          runLoop1 effect {state with environment := state.environment ++ env}


  private partial def runWithErrorHandler1 (cause: Cause E) (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    match state.stack with
      | .more _ (some errorHandler) _ tail _ validEnv env =>
        runLoop1 (errorHandler cause) (inst := validEnv) {state with stack := tail, environment := env}

      | .more _ none (some (.up eq_E_E₁)) tail .. => 
        runWithErrorHandler1 (cause.map (cast eq_E_E₁)) {state with stack := tail}

      | .more _ none none .. => 
        log state.fiberId "Internal defect: Stack not empty but don't know what do do next. (This should not happen)"

      | .done complete =>
        complete (.failure cause)
    

  /-- `A` will be passed to the first continuation in the stack  -/
  private partial def continueOrComplete1 (value: A) (state: RunState Rprov E A E₁ A₁) : IO Unit := do
    match state.stack with
      | .done complete => complete (.success value)

      | .more next _ _ tail _ validEnv env =>
        runLoop1 (currentEffect := (next value)) (inst := validEnv) {state with stack := tail, environment := env }


  partial def runWithInterruption1 (currentEffect: Z Rexp E A)  [inst: Rexp ⊂ Rprov] currentTime (state: RunState Rprov E A E₁ A₁) := do
    let nextEffect: Z Unit _ _  := 
      Z.failCause Cause.interrupt
    runLoop1
      nextEffect
      { state with
        interruption := {state.interruption with isInterrupting := true}
        stack := .more (fun _ => currentEffect) none (eq_E_E₁? := some (.up rfl)) state.stack none (validEnv := inst) (env := state.environment)
      }


  /-- Runs the given effect in IO and returns a Fiber  -/
  partial def unsafeRunFiber (self: Z Rexp E A) (env: Environment Rprov) [Rexp ⊂ Rprov] (parentFiberId: FiberId) (name: String) (startTime: Nat) : IO (Fiber E A) := do
    let fiberId := s!"{parentFiberId}-{name}-{<- IO.rand 0 100000}"
    let fiber <- Fiber.empty fiberId
    let state: RunState ..  := {
        interruption := (<- fiber.toInterruption)
        fiberInfos   := (<- IO.mkRef [])
        stack        := .done fiber.complete
        environment  := env
        fiberId      := fiberId
        initialTime  := startTime
    }
    -- continue in the background
    let task <- IO.asTask do
      runLoop1 self state
      -- TODO: restore this later!
      -- for fiberRef in (<- fiberInfos.get) do
      --   log fiberId s!" finishing, interrupting child: {fiberRef.fiberId}"
      --   fiberRef.interrupt
    fiber.setTask task
    return fiber


end


end Z
