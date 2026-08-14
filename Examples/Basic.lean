import Z

/-!
Small examples of the public Zenith API.

Use ordinary `do` when all actions already have one environment and error
type. Use `zdo` when Zenith must infer and combine different requirements.
-/

open Console (live)

structure Person where
  name : String
  age : Nat

instance : ToString Person where
  toString person :=
    s!"Person (name = {person.name}, age = {person.age})"

/-! Creating and combining effects. -/

def succeedExample : Z Unit Empty String :=
  Z.succeed "hello from Z.succeed"

def zipExample : Z Unit Empty (Nat × String) :=
  (Z.succeed 8).zip (Z.succeed "LO")

/-- Run both effects in child fibers and keep left-to-right result order. -/
def zipParExample : Z Unit Empty (Nat × String) :=
  (Z.sleep 10 *> Z.succeed 8).zipPar
    (Z.sleep 10 *> Z.succeed "LO")

/-- Return the first successful effect and stop the losing effect. -/
def raceExample : Z Unit Empty String :=
  (Z.sleep 20 *> Z.succeed "slow").race
    (Z.sleep 10 *> Z.succeed "fast")

/-- Return `none` if the effect does not finish within 10 milliseconds. -/
def timeoutExample : Z Unit Empty (Option String) :=
  (Z.sleep 20 *> Z.succeed "too slow").timeout 10

/-- Run once, then repeat three more times and return the recurrence count. -/
def repeatExample : Z Unit Empty Nat :=
  (Z.succeed "tick").repeat (Schedule.recurs 3)

/-- Retry a typed failure at most three times. -/
def retryExample : Z Unit String Nat :=
  (Z.fail "not ready" : Z Unit String Nat).retry (Schedule.recurs 3)

/-- Exponential delays bounded to three retries. -/
def boundedBackoff : Schedule Unit String (UInt32 × Nat) :=
  (Schedule.exponential 10).zip (Schedule.recurs 3)

/-- Fibonacci delays bounded to four retries. -/
def boundedFibonacci : Schedule Unit String (UInt32 × Nat) :=
  (Schedule.fibonacci 10).zip (Schedule.recurs 4)

/-- Randomize exponential delays from 80% through 120% of their base value. -/
def jitteredBackoff : Schedule Random String (UInt32 × Nat) :=
  ((Schedule.exponential 10).jittered).zip (Schedule.recurs 3)

/-- Manually advance one step of a schedule. -/
def manualScheduleStep : Z Unit Empty (Option Nat) :=
  (Schedule.recurs (Input := String) 2).driver fun driver =>
    driver.next "temporary"

/-- Retry only while the error is temporary. -/
def temporaryRetry : Schedule Unit String Nat :=
  (Schedule.forever).whileInput fun error => error == "temporary"

/-- Read the retry limit from the schedule environment. -/
def configuredRetry : Schedule Nat String Nat :=
  (Schedule.forever).whileOutputM fun retries =>
    Z.serviceWith fun limit : Nat => retries < limit

/-- Collect the outputs that caused the schedule to continue. -/
def retryHistory : Schedule Unit String (List Nat) :=
  (Schedule.recurs 3).fold [] fun history retry => history ++ [retry]

/-- Collect all schedule outputs, including its terminal output. -/
def allRetryOutputs : Schedule Unit String (List Nat) :=
  (Schedule.recurs 3).collectAll

/-- Recover with the last error and final schedule output. -/
def retryOrElseExample : Z Unit Empty Nat :=
  (Z.fail "not ready" : Z Unit String Nat).retryOrElse
    (Schedule.recurs 3) fun _ retries => Z.succeed retries

/-- The same operation as `zipExample`, written with `do`. -/
def zipExample2 : Z Unit Empty (Nat × String) := do
  let number <- Z.succeed 8
  let text <- Z.succeed "LO"
  pure (number, text)

def mapExample : Z Unit Empty String :=
  zipExample.withLabel "zipExample"
    |>.map (fun (age, name) => { name, age : Person })
    |>.map (fun person =>
      s!"{person.name} is {person.age} years old")

def monadExample : Z Unit Empty Unit := do
  let (number, text) <- zipExample
  live.printLine s!"Got a tuple: ({number}, {text})"

/-! Lift synchronous `IO` into Zenith. -/

def fromIOExample : Z Unit Empty Unit :=
  Z.fromIO <| IO.println "hello from IO"

def attemptExample : Z Unit IO.Error Unit :=
  Z.attempt <| IO.println "hello from IO"

/--
`IO` coerces to a `Z` effect when the expected type is known.

The coercion targets the defect-only channel: a thrown `IO.Error` becomes a
defect, not a typed failure. Use `Z.attempt` to catch it as a typed error.
-/
def coercionExample : Z Unit Empty Unit :=
  IO.println "hello from IO"

/-! Asynchronous effects and fibers. -/

def asyncExample : Z Unit Empty Nat :=
  Z.async fun resume => do
    IO.println "sleeping for one second..."
    IO.sleep 1000
    IO.println "waking up"
    resume (.success 10)

def forkExample : Z Unit Empty Unit := do
  let left :=
    Z.repeatN 3 (live.printLine "- left" *> Z.sleep 20)
  let right :=
    Z.repeatN 3 (live.printLine "+ right" *> Z.sleep 20)
  let leftFiber <- left.fork "left"
  let rightFiber <- right.fork "right"
  leftFiber.join
  rightFiber.join
  live.printLine "both fibers finished"

/-- A long sequence is represented by the executable `ZCore` tree. -/
def stackSafetyExample : Z Unit Empty Unit :=
  Z.repeatN 20 <| live.printLine "Howdy!"

/-! Finalization and interruption. -/

def ensuringExample : Z Unit Empty Unit :=
  (live.printLine "work" *> Z.sleep 10)
    |>.repeatN 1
    |>.ensuring (live.printLine "finalizer")

def uninterruptibleExample : Z Unit Empty Unit :=
  Z.sleep 10 |>.uninterruptible

/-- The fiber finishes before the interruption request. -/
def interruptionExample1 : Z Unit Empty Unit := do
  let fiber <-
    (Z.sleep 1)
      |>.repeatN 4
      |>.fork "finished-fiber"
  Z.sleep 20
  let _ <- fiber.interrupt
  pure ()

/-- The interruption request stops a running fiber. -/
def interruptionExample2 : Z Unit Empty Unit := do
  let fiber <-
    (Z.sleep 50)
      |>.repeatN 5
      |>.fork "running-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt
  pure ()

/-- The interruption request can arrive before the first action completes. -/
def interruptionExample3 : Z Unit Empty Unit := do
  let fiber <-
    (Z.sleep 50)
      |>.repeatN 5
      |>.fork "new-fiber"
  let _ <- fiber.interrupt
  pure ()

/-- An uninterruptible fiber completes before `interrupt` returns. -/
def uninterruptibleExample1 : Z Unit Empty Unit := do
  let fiber <-
    (Z.sleep 20)
      |>.repeatN 2
      |>.uninterruptible
      |>.fork "uninterruptible-fiber"
  Z.sleep 10
  let _ <- fiber.interrupt
  pure ()

/-- Interruption takes effect after the uninterruptible region. -/
def uninterruptibleExample2 : Z Unit Empty Unit := do
  let region := (Z.sleep 20).repeatN 2 |>.uninterruptible
  let fiber <-
    (region *> Z.sleep 100)
      |>.fork "partly-uninterruptible-fiber"
  Z.sleep 10
  let _ <- fiber.interrupt
  pure ()

/-! Environment inference. -/

/-- `zdo` combines and normalizes the two environment requirements. -/
def envExample1 := zdo[Empty]
  let number <- Z.environment Nat
  let text <- Z.environment String
  live.printLine s!"environment: ({number}, {text})"

example : Z (Nat × String) Empty Unit := envExample1

def envExample1ready : Z Unit Empty Unit :=
  envExample1.provideEnvironment (1, "hello")

/-!
`Console` is in `Type 1`. Zenith can use it as an environment even though the
standard `IO` result type is restricted to `Type`.
-/
def envExample2 : Z Console Empty Unit :=
  Console.printLineZ "hello from Console.printLineZ"

def envExample2ready : Z Unit Empty Unit :=
  envExample2.provideEnvironment Console.live

/-- `zdo` infers the `Console` environment and the `IO.Error` channel. -/
def interactiveConsoleExample := zdo
  Console.printLineZ "What is your name?"
  let name <- Console.readLineZ
  Console.printLineZ s!"hello {name.trimAscii.toString}"

example : Z Console IO.Error Unit := interactiveConsoleExample
