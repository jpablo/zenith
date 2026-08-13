import Z.Random

/-!
Composable policies for effect retry and repetition.
-/

namespace Schedule

/-- The action selected by one schedule step. -/
inductive Decision
  | done
  | continue (delayMilliseconds : UInt32)
  deriving BEq, Repr

end Schedule

/--
A schedule is a state machine. It consumes one effect result at each step,
emits one output, and decides whether to stop or continue after a delay.
-/
structure Schedule.{ur} (R : Type ur) (Input Output : Type) :
    Type (max 1 ur) where
  State : Type
  initial : State
  step : Input -> State ->
    Z R Empty (State × Output × Schedule.Decision)

namespace Schedule

/--
A mutable handle that manually advances a schedule. `next` returns `some`
output while the schedule continues and `none` after its terminal step.
-/
structure Driver.{ur}
    (R : Type ur)
    (Input Output : Type)
    (schedule : Schedule R Input Output) :
    Type (max 1 ur) where
  private reference : IO.Ref (Option Output × schedule.State)

namespace Driver

/-- Return the most recently observed output, if the driver has one. -/
def last
    (self : Driver R Input Output schedule) : Z Unit Empty (Option Output) :=
  Z.succeed do
    let (output, _) ← self.reference.get
    pure output

/-- Reset the driver to the schedule's initial state and clear its output. -/
def reset (self : Driver R Input Output schedule) : Z Unit Empty Unit :=
  Z.succeed <| self.reference.set (none, schedule.initial)

/-- Return the driver's current internal schedule state. -/
def state (self : Driver R Input Output schedule) :
    Z Unit Empty schedule.State :=
  Z.succeed do
    let (_, state) ← self.reference.get
    pure state

private def delay (milliseconds : UInt32) : Z R Empty Unit :=
  (Z.sleep milliseconds).contramap fun _ : R => ()

/-- Advance one schedule step, waiting for its delay if it continues. -/
def next
    (self : Driver R Input Output schedule)
    (input : Input) : Z R Empty (Option Output) :=
  Z.withIO self.reference.get fun (_, currentState) =>
    (schedule.step input currentState).flatMap fun
      (nextState, output, decision) =>
        Z.withIO (self.reference.set (some output, nextState)) fun _ =>
          match decision with
          | .done => Z.internal.succeedNow none
          | .continue milliseconds =>
              delay milliseconds *> Z.internal.succeedNow (some output)

end Driver

/-- Build a schedule from an initial state and a step function. -/
def make
    {S : Type}
    (initial : S)
    (step : Input -> S ->
      Z R Empty (S × Output × Decision)) :
    Schedule R Input Output where
  State := S
  initial := initial
  step := step

/--
Create a mutable driver for the duration of `use`. A driver cannot be returned
from `Z`, because it contains a schedule, which lives in a higher universe.
-/
def driver
    (self : Schedule R Input Output)
    (use : Driver R Input Output self -> Z R E A) : Z R E A :=
  Z.withIO (IO.mkRef ((none : Option Output), self.initial)) fun reference =>
    use { reference }

/-- Change the environment supplied to each schedule step. -/
def contramapEnvironment
    (environment : R₀ -> R₁)
    (self : Schedule R₁ Input Output) :
    Schedule R₀ Input Output where
  State := self.State
  initial := self.initial
  step input state := (self.step input state).contramap environment

/-- Transform every output without changing the schedule decisions. -/
def map
    (self : Schedule R Input Output)
    (output : Output -> Output₁) : Schedule R Input Output₁ where
  State := self.State
  initial := self.initial
  step input state :=
    (self.step input state).map fun (nextState, value, decision) =>
      (nextState, output value, decision)

/-- Replace every output with one constant value. -/
def as
    (self : Schedule R Input Output)
    (value : Output₁) : Schedule R Input Output₁ :=
  self.map fun _ => value

/--
Fold the outputs for which the underlying schedule continues. The terminal
output is not added to the accumulator.
-/
def fold
    (self : Schedule R Input Output)
    (initial : Accumulator)
    (accumulate : Accumulator -> Output -> Accumulator) :
    Schedule R Input Accumulator where
  State := self.State × Accumulator
  initial := (self.initial, initial)
  step input state :=
    let scheduleState := state.1
    let accumulator := state.2
    (self.step input scheduleState).map fun
      (nextScheduleState, output, decision) =>
        match decision with
        | .done =>
            ((nextScheduleState, accumulator), accumulator, .done)
        | .continue delay =>
            let nextAccumulator := accumulate accumulator output
            ((nextScheduleState, nextAccumulator), accumulator,
              .continue delay)

/--
Effectfully fold the outputs for which the underlying schedule continues.
The schedule and accumulator environment requirements are combined.
-/
def foldZIO
    [meet : Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (initial : Accumulator)
    (accumulate : Accumulator -> Output -> Z R₂ Empty Accumulator) :
    Schedule R Input Accumulator where
  State := self.State × Accumulator
  initial := (self.initial, initial)
  step input state :=
    let scheduleState := state.1
    let accumulator := state.2
    let base : Z R Empty (self.State × Output × Decision) :=
      (self.step input scheduleState).contramap meet.left
    base.flatMap fun (nextScheduleState, output, decision) =>
      match decision with
      | .done =>
          Z.internal.succeedNow
            ((nextScheduleState, accumulator), accumulator, .done)
      | .continue delay =>
          let folded : Z R Empty Accumulator :=
            (accumulate accumulator output).contramap meet.right
          folded.map fun nextAccumulator =>
            ((nextScheduleState, nextAccumulator), accumulator,
              .continue delay)

/-- Count the outputs for which the underlying schedule continues. -/
def repetitions
    (self : Schedule R Input Output) : Schedule R Input Nat :=
  self.fold 0 fun count _ => count + 1

/--
Collect every underlying output, including the output from the terminal step.
-/
def collectAll
    (self : Schedule R Input Output) : Schedule R Input (List Output) where
  State := self.State × List Output
  initial := (self.initial, [])
  step input state :=
    let scheduleState := state.1
    let outputs := state.2
    (self.step input scheduleState).map fun
      (nextScheduleState, output, decision) =>
        let nextOutputs := outputs ++ [output]
        ((nextScheduleState, nextOutputs), nextOutputs, decision)

/-- Stop at the first step without requesting another effect run. -/
def stop : Schedule Unit Input Unit :=
  make () fun _ _ => Z.succeedNow ((), (), .done)

/-- Continue without a delay for exactly `count` additional effect runs. -/
def recurs (count : Nat) : Schedule Unit Input Nat :=
  make 0 fun _ current =>
    let decision :=
      if current < count then .continue 0 else .done
    Z.succeedNow (current + 1, current, decision)

/-- Continue once without a delay. -/
def once : Schedule Unit Input Unit :=
  (recurs 1).as ()

/-- Continue forever without a delay and emit the recurrence count. -/
def forever : Schedule Unit Input Nat :=
  make 0 fun _ current =>
    Z.succeedNow (current + 1, current, .continue 0)

/-- Continue forever without a delay and emit each input. -/
def identity : Schedule Unit Input Input :=
  make () fun input state =>
    Z.succeedNow (state, input, .continue 0)

/-- Continue forever with a fixed delay between effect runs. -/
def spaced (milliseconds : UInt32) : Schedule Unit Input Nat :=
  make 0 fun _ current =>
    Z.succeedNow (current + 1, current, .continue milliseconds)

private def Decision.intersection : Decision -> Decision -> Decision
  | .continue leftDelay, .continue rightDelay =>
      .continue (max leftDelay rightDelay)
  | _, _ => .done

private def Decision.union : Decision -> Decision -> Decision
  | .done, .done => .done
  | .continue delay, .done | .done, .continue delay => .continue delay
  | .continue leftDelay, .continue rightDelay =>
      .continue (min leftDelay rightDelay)

/--
Continue while both schedules continue. Run both steps with the same input,
pair their outputs, and select the longer delay.
-/
def intersect
    [meet : Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output₁)
    (other : Schedule R₂ Input Output₂) :
    Schedule R Input (Output₁ × Output₂) where
  State := self.State × other.State
  initial := (self.initial, other.initial)
  step input state :=
    let left : Z R Empty
        (self.State × Output₁ × Decision) :=
      (self.step input state.1).contramap meet.left
    let right : Z R Empty
        (other.State × Output₂ × Decision) :=
      (other.step input state.2).contramap meet.right
    left.zipWith right fun
      (leftState, leftOutput, leftDecision)
      (rightState, rightOutput, rightDecision) =>
        ((leftState, rightState),
          (leftOutput, rightOutput),
          leftDecision.intersection rightDecision)

/-- A ZIO-compatible named alias for schedule intersection. -/
def zip
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output₁)
    (other : Schedule R₂ Input Output₂) :
    Schedule R Input (Output₁ × Output₂) :=
  self.intersect other

/--
Continue while either schedule continues. Run both steps with the same input,
pair their outputs, and select the shorter active delay.
-/
def union
    [meet : Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output₁)
    (other : Schedule R₂ Input Output₂) :
    Schedule R Input (Output₁ × Output₂) where
  State := self.State × other.State
  initial := (self.initial, other.initial)
  step input state :=
    let left : Z R Empty
        (self.State × Output₁ × Decision) :=
      (self.step input state.1).contramap meet.left
    let right : Z R Empty
        (other.State × Output₂ × Decision) :=
      (other.step input state.2).contramap meet.right
    left.zipWith right fun
      (leftState, leftOutput, leftDecision)
      (rightState, rightOutput, rightDecision) =>
        ((leftState, rightState),
          (leftOutput, rightOutput),
          leftDecision.union rightDecision)

/-- A ZIO-compatible named alias for schedule union. -/
def either
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output₁)
    (other : Schedule R₂ Input Output₂) :
    Schedule R Input (Output₁ × Output₂) :=
  self.union other

/-- Run `self` to completion and then run `other`, tagging each output. -/
def andThenEither
    [meet : Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output₁)
    (other : Schedule R₂ Input Output₂) :
    Schedule R Input (Sum Output₁ Output₂) where
  State := self.State × other.State × Bool
  initial := (self.initial, other.initial, true)
  step input
    | (leftState, rightState, true) =>
        let left : Z R Empty
            (self.State × Output₁ × Decision) :=
          (self.step input leftState).contramap meet.left
        left.flatMap fun (nextLeftState, leftOutput, decision) =>
          match decision with
          | .continue delay =>
              Z.internal.succeedNow
                ((nextLeftState, rightState, true),
                  Sum.inl leftOutput,
                  .continue delay)
          | .done =>
              let right : Z R Empty
                  (other.State × Output₂ × Decision) :=
                (other.step input rightState).contramap meet.right
              right.map fun (nextRightState, rightOutput, rightDecision) =>
                ((nextLeftState, nextRightState, false),
                  Sum.inr rightOutput,
                  rightDecision)
    | (leftState, rightState, false) =>
        let right : Z R Empty
            (other.State × Output₂ × Decision) :=
          (other.step input rightState).contramap meet.right
        right.map fun (nextRightState, rightOutput, rightDecision) =>
          ((leftState, nextRightState, false),
            Sum.inr rightOutput,
            rightDecision)

/-- Run `self` to completion and then run `other`. -/
def andThen
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (other : Schedule R₂ Input Output) : Schedule R Input Output :=
  (self.andThenEither other).map (Sum.elim id id)

private def multiplyDelay (delay : UInt32) (factor : Nat) : UInt32 :=
  let maximum : Nat := 4294967295
  UInt32.ofNat (min (delay.toNat * factor) maximum)

private def addDelay (left right : UInt32) : UInt32 :=
  let maximum : Nat := 4294967295
  UInt32.ofNat (min (left.toNat + right.toNat) maximum)

private def scaleDelay (delay : UInt32) (percentage : Nat) : UInt32 :=
  let maximum : Nat := 4294967295
  UInt32.ofNat (min ((delay.toNat * percentage) / 100) maximum)

/--
Continue forever with geometric backoff. The first delay is `base`; each next
delay is the previous delay multiplied by `factor`.
-/
def exponential
    (base : UInt32)
    (factor : Nat := 2) : Schedule Unit Input UInt32 :=
  make base fun _ delay =>
    Z.succeedNow
      (multiplyDelay delay factor, delay, .continue delay)

/--
Continue forever with Fibonacci backoff. The first two delays are `one`; each
later delay is the saturated sum of the preceding two delays.
-/
def fibonacci (one : UInt32) : Schedule Unit Input UInt32 :=
  make (one, one) fun _ state =>
    let current := state.1
    let next := state.2
    Z.succeedNow
      ((next, addDelay current next), current, .continue current)

/--
Randomly scale each continuing delay by an integer percentage. The default
range is 80% through 120%. The range endpoints are normalized when reversed.
-/
def jittered
    [meet : Environment.Meet R₁ Random R]
    (self : Schedule R₁ Input Output)
    (minimumPercent : Nat := 80)
    (maximumPercent : Nat := 120) : Schedule R Input Output where
  State := self.State
  initial := self.initial
  step input state :=
    let base : Z R Empty (self.State × Output × Decision) :=
      (self.step input state).contramap meet.left
    base.flatMap fun (nextState, output, decision) =>
      match decision with
      | .done => Z.internal.succeedNow (nextState, output, .done)
      | .continue delay =>
          let lower := min minimumPercent maximumPercent
          let upper := max minimumPercent maximumPercent
          let percentage : Z R Empty Nat :=
            (Random.nextNatZ lower upper).contramap meet.right
          percentage.map fun selected =>
            (nextState, output, .continue (scaleDelay delay selected))

/--
Keep an underlying continue decision only when `predicate` accepts its input
and output. An underlying stop decision always stays stopped.
-/
def check
    (self : Schedule R Input Output)
    (predicate : Input -> Output -> Bool) : Schedule R Input Output where
  State := self.State
  initial := self.initial
  step input state :=
    (self.step input state).map fun (nextState, output, decision) =>
      let checkedDecision :=
        match decision with
        | .done => .done
        | .continue delay =>
            if predicate input output then .continue delay else .done
      (nextState, output, checkedDecision)

/--
Use an effectful predicate to keep or stop an underlying continue decision.
The schedule and predicate environment requirements are combined.
-/
def checkZIO
    [meet : Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (predicate : Input -> Output -> Z R₂ Empty Bool) :
    Schedule R Input Output where
  State := self.State
  initial := self.initial
  step input state :=
    let base : Z R Empty (self.State × Output × Decision) :=
      (self.step input state).contramap meet.left
    base.flatMap fun (nextState, output, decision) =>
      match decision with
      | .done =>
          Z.internal.succeedNow (nextState, output, .done)
      | .continue delay =>
          let accepted : Z R Empty Bool :=
            (predicate input output).contramap meet.right
          accepted.map fun keepGoing =>
            let checkedDecision :=
              if keepGoing then .continue delay else .done
            (nextState, output, checkedDecision)

/-- Continue while the input satisfies `predicate`. -/
def whileInput
    (self : Schedule R Input Output)
    (predicate : Input -> Bool) : Schedule R Input Output :=
  self.check fun input _ => predicate input

/-- Continue while the effectful input predicate returns true. -/
def whileInputZIO
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (predicate : Input -> Z R₂ Empty Bool) : Schedule R Input Output :=
  self.checkZIO fun input _ => predicate input

/-- Continue until the input satisfies `predicate`. -/
def untilInput
    (self : Schedule R Input Output)
    (predicate : Input -> Bool) : Schedule R Input Output :=
  self.whileInput fun input => !(predicate input)

/-- Continue until the effectful input predicate returns true. -/
def untilInputZIO
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (predicate : Input -> Z R₂ Empty Bool) : Schedule R Input Output :=
  self.checkZIO fun input _ =>
    (predicate input).map fun stopNow => !stopNow

/-- Continue while the output satisfies `predicate`. -/
def whileOutput
    (self : Schedule R Input Output)
    (predicate : Output -> Bool) : Schedule R Input Output :=
  self.check fun _ output => predicate output

/-- Continue while the effectful output predicate returns true. -/
def whileOutputZIO
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (predicate : Output -> Z R₂ Empty Bool) : Schedule R Input Output :=
  self.checkZIO fun _ output => predicate output

/-- Continue until the output satisfies `predicate`. -/
def untilOutput
    (self : Schedule R Input Output)
    (predicate : Output -> Bool) : Schedule R Input Output :=
  self.whileOutput fun output => !(predicate output)

/-- Continue until the effectful output predicate returns true. -/
def untilOutputZIO
    [Environment.Meet R₁ R₂ R]
    (self : Schedule R₁ Input Output)
    (predicate : Output -> Z R₂ Empty Bool) : Schedule R Input Output :=
  self.checkZIO fun _ output =>
    (predicate output).map fun stopNow => !stopNow

instance [Environment.Meet R₁ R₂ R] :
    HAnd
      (Schedule R₁ Input Output₁)
      (Schedule R₂ Input Output₂)
      (Schedule R Input (Output₁ × Output₂)) where
  hAnd := intersect

instance [Environment.Meet R₁ R₂ R] :
    HOr
      (Schedule R₁ Input Output₁)
      (Schedule R₂ Input Output₂)
      (Schedule R Input (Output₁ × Output₂)) where
  hOr := union

instance [Environment.Meet R₁ R₂ R] :
    HAppend
      (Schedule R₁ Input Output)
      (Schedule R₂ Input Output)
      (Schedule R Input Output) where
  hAppend := andThen

end Schedule

namespace Z

private def scheduleStep
    (policy : Schedule R Input Output)
    (input : Input)
    (state : policy.State) :
    Z R E (policy.State × Output × Schedule.Decision) :=
  (policy.step input state).mapFailure Empty.elim

private def scheduleDelay (milliseconds : UInt32) : Z R E Unit :=
  Z.adapt (fun _ : R => ()) Empty.elim id (Z.sleep milliseconds)

private partial def retryLoop
    (effect : Z R E A)
    (policy : Schedule R E Output)
    (state : policy.State) : Z R E A :=
  effect.foldCauseZ
    (fun cause =>
      match cause with
      | .fail error =>
          scheduleStep policy error state |>.flatMap fun
            (nextState, _, decision) =>
              match decision with
              | .done =>
                  (Z.failCause (R := R) (.fail error)).map impossible
              | .continue delay =>
                  scheduleDelay delay *> retryLoop effect policy nextState
      | unhandled =>
          (Z.failCause (R := R) unhandled).map impossible)
    Z.internal.succeedNow

/-- Retry typed failures according to `policy`. -/
def retry
    [meet : Environment.Meet R₁ R₂ R]
    (self : Z R₁ E A)
    (policy : Schedule R₂ E Output) : Z R E A :=
  let effect := self.contramap meet.left
  let policy := policy.contramapEnvironment meet.right
  retryLoop effect policy policy.initial

private def combineResidual
    (combine : Cause Empty -> Cause Empty -> Cause Empty)
    (left right : Option (Cause Empty)) : Option (Cause Empty) :=
  match left, right with
  | some leftCause, some rightCause => some (combine leftCause rightCause)
  | some cause, none | none, some cause => some cause
  | none, none => none

/-- Keep defects and interruptions after typed failures are handled. -/
private def residualCause : Cause E -> Option (Cause Empty)
  | .fail _ => none
  | .die error => some (.die error)
  | .interrupt => some .interrupt
  | .sequential left right =>
      combineResidual .sequential (residualCause left) (residualCause right)
  | .parallel left right =>
      combineResidual .parallel (residualCause left) (residualCause right)

private partial def retryOrElseEitherLoop
    (partialFallback : Z R E₁ (Sum B A))
    (effect : Z R E A)
    (policy : Schedule R E Output)
    (orElse : E -> Output -> Z R E₁ B)
    (state : policy.State) : Z R E₁ (Sum B A) :=
  effect.foldCauseZ
    (fun cause =>
      match residualCause cause with
      | some unhandled =>
          (Z.failCause (R := R) (unhandled.map Empty.elim)).map impossible
      | none =>
          match cause.failureOption with
          | none =>
              (Z.die (R := R) <| IO.userError
                "retryOrElse received an empty failure cause").map impossible
          | some error =>
              scheduleStep policy error state |>.flatMap fun
                (nextState, output, decision) =>
                  match decision with
                  | .done => (orElse error output).map Sum.inl
                  | .continue delay =>
                      scheduleDelay delay *>
                        retryOrElseEitherLoop partialFallback effect policy
                          orElse nextState)
    (fun value => Z.internal.succeedNow (Sum.inr value))

/--
Retry typed failures. If the policy stops, pass its terminal output and the
last error to `orElse`. Tag fallback success on the left and effect success on
the right.
-/
def retryOrElseEither
    [effectPolicy : Environment.Meet R₁ R₂ EffectAndPolicy]
    [complete : Environment.Meet EffectAndPolicy R₃ R]
    (self : Z R₁ E A)
    (policy : Schedule R₂ E Output)
    (orElse : E -> Output -> Z R₃ E₁ B) :
    Z R E₁ (Sum B A) :=
  let effect : Z R E A :=
    self.contramap fun environment =>
      effectPolicy.left (complete.left environment)
  let policy : Schedule R E Output :=
    policy.contramapEnvironment fun environment =>
      effectPolicy.right (complete.left environment)
  let fallback (error : E) (output : Output) : Z R E₁ B :=
    (orElse error output)
      |>.contramap complete.right
  let partialFallback : Z R E₁ (Sum B A) :=
    Z.internal.done (.failure .interrupt)
  retryOrElseEitherLoop partialFallback effect policy fallback policy.initial

/--
Retry typed failures and run `orElse` after the policy stops. Both successful
paths have the same value type.
-/
def retryOrElse
    [Environment.Meet R₁ R₂ EffectAndPolicy]
    [Environment.Meet EffectAndPolicy R₃ R]
    (self : Z R₁ E A)
    (policy : Schedule R₂ E Output)
    (orElse : E -> Output -> Z R₃ E₁ A) : Z R E₁ A :=
  (self.retryOrElseEither policy orElse).map (Sum.elim id id)

private partial def repeatLoop
    (fallback : Z R E Output)
    (effect : Z R E A)
    (policy : Schedule R A Output)
    (state : policy.State) : Z R E Output :=
  effect.foldCauseZ
    (fun cause => (Z.failCause (R := R) cause).map impossible)
    (fun value =>
      scheduleStep policy value state |>.flatMap fun
        (nextState, output, decision) =>
          match decision with
          | .done => Z.internal.succeedNow output
          | .continue delay =>
              scheduleDelay delay *>
                repeatLoop fallback effect policy nextState)

/-- Repeat successful effects according to `policy` and return its output. -/
def «repeat»
    [meet : Environment.Meet R₁ R₂ R]
    (self : Z R₁ E A)
    (policy : Schedule R₂ A Output) : Z R E Output :=
  let effect := self.contramap meet.left
  let policy := policy.contramapEnvironment meet.right
  let fallback : Z R E Output :=
    Z.internal.done (.failure .interrupt)
  repeatLoop fallback effect policy policy.initial

end Z
