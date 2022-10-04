import Z

open Console (consoleLive)

structure Person where
  name : String
  age  : Nat

instance : ToString Person where
  toString person := s!"Person (name = {person.name}, age = {person.age})"


def IO.repeatN (n: Nat) (za: IO A) : IO Unit := do
  let _ <- za
  if n > 0 then
    repeatN (n - 1) za


/-!  Examples -/


def succeedNowExample :=
  Z.succeedNow "hello from Z.succeed"

def zipExample :=
  (Z.succeedNow (8: Nat)).zip (.succeedNow "LO")


def zipExample2: Z Unit Empty (Nat × String) :=
  .flatMap (.done ∘ .success $ (8: Nat)) fun n => 
    .flatMap (.done ∘ .success $ "LO") fun s => 
      .done ∘ .success $ (n, s)

def mapExample :=
  zipExample.withLabel "zipExample"
    |>.map (fun (nat, str) => {name := str, age := nat : Person})
    |>.map (fun p => s!"{p.name} is {p.age} years old")

def monadExample := do 
  let (a, b) <- zipExample
  consoleLive.printLine s!"Got a tuple: ({a},{b})"

def succeedExample: Z Unit Empty Unit :=
  Z.succeed <| IO.println "hello from IO"

def attemptExample: Z Unit IO.Error Unit :=
  Z.attempt <| IO.println "hello from IO"


/-- Option 1: propagate type parameters up to the top level function -/
def asyncExample {R E} :=
  Z.async (R := R) (E := E) fun continueExecution => do
    IO.println "sleeping 1 seconds..."
    IO.sleep 1000
    IO.println "waking up"
    continueExecution (.success 10)

def forkExample := do
  let t1 := Z.repeatN 10 (consoleLive.printLine "- t1" *> .sleep 200)
  let t3 := Z.repeatN 10 (consoleLive.printLine "- t1" *> .sleep 200)
  let t2 := Z.repeatN 10 (consoleLive.printLine "+ t2" *> .sleep 200)
  let fiber1 <- t1.fork "f1"
  let fiber2 <- t2.fork "f2"
  -- fiber1.interrupt
  -- fiber2.interrupt
  let int    <- fiber1.join
  let int2   <- fiber2.join
  -- let ret := s!"Got two ints: ({int}, {int2})"
  -- .printLine ret
  consoleLive.printLine "done"

def taskExample: IO Unit := do
  let t1 := IO.repeatN 100 (IO.println "- t1" *> IO.sleep 200)
  let t2 := IO.repeatN 100 (IO.println "+ t2" *> IO.sleep 200)
  let _ <- t1.asTask
  let _ <- t2.asTask
  IO.println "done"


-- def ioFlatMapExample2 :=
-- Z.withIO (do IO.println "hi" ; return "one") (fun i => Z.printLine s!"got an {i}")



----------------------------------------------

/-- Option 2: Fix type parameters -/

def forkExample2 := do
  (consoleLive.printLine "(example) starting...")
  let fiber  <- ((consoleLive.printLine "Howdy" *> .sleep 10).repeatN 3).fork "f1"
  -- (Z.printLine "before sleep").orDie
  -- (Z.sleep 1).orDie
  -- (Z.printLine "after sleep").orDie
  let _      <- fiber.join
  -- (Z.printLine "(example) ==== Finishing example2 ====" ).orDie

----------------------------------------------





def stackOverflow :=
  Z.repeatN 20 <| consoleLive.printLine "Howdy!"


def stackOverflowIO :=
  let p := IO.println "Howdy!"
  IO.repeatN 10000000 p



def flatMapEx := do
  consoleLive.printLine "hi"
  consoleLive.printLine "there"



-- Investigate
def ensuringExample :=  do
  consoleLive.printLine "Howdy" *> Z.sleep 100
  |>.repeatN 1
  |>.ensuring (consoleLive.printLine "--- Bowdy! ---" )


def uninterruptibleExample :=
  Z.sleep 100 |>.uninterruptible


/-- Fiber finishes before interruption  -/
def interruptionExample1 := do
  let fiber <- 
    Z.sleep 1
    |>.repeatN 4
    |>.fork "my-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt
  
/-- Fiber is interrupted  -/
def interruptionExample2 := do
  let fiber <- 
    Z.sleep 50
    |>.repeatN 5
    |>.fork "my-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt

/-- Fiber is interrupted and then joined  -/
def interruptionExample2b := do
  let fiber <- 
    Z.sleep 50
    |>.repeatN 5
    |>.fork "my-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt
  -- interrupt already uses .join, so joining again is pointless
  let _ <- fiber.join
  
/-- Fiber is interrupted before execution  -/
def interruptionExample3 := do
  let fiber <- 
    Z.sleep 50
    |>.repeatN 5
    |>.fork "my-fiber"
  let _ <- fiber.interrupt
  

/-- Fiber is not interrupted  -/
def uninterruptibleExample1 := do
  let fiber <- 
    Z.sleep 50
    |>.repeatN 5
    |>.uninterruptible
    |>.fork "my-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt

/-- Fiber is interrupted only after the uninterruptible region has finised  -/
def uninterruptibleExample2 := do
  let fiber <- 
    ((Z.sleep 50 |>.repeatN 5 |>.uninterruptible) *> (Z.sleep 50 |>.repeatN 5))
    |>.fork "my-fiber"
  Z.sleep 100
  let _ <- fiber.interrupt


def e1: Z Nat Empty (Environment Nat)       := Z.environment Nat
def e2: Z String Empty (Environment String) := Z.environment String

def envExample1: Z (Nat × String) Empty Unit := do
  let env <- Z.environment (Nat × String)
  -- let nat <- Z.environment Nat
  -- let str <- Z.environment String
  consoleLive.printLine (env.get Nat)
  consoleLive.printLine (env.get String)

def envExample: Z Unit Empty Unit :=
  envExample1.provideEnvironment ((1: Nat), "hello")

def envExample3 :=
  (Z.succeedNow "hello from Z.succeed").provideEnvironment ()


