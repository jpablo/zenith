import Z.Combinators

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
