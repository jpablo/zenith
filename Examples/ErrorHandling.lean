import Z

/-! Examples of typed errors, `IO.Error`, and `zdo` recovery. -/

open Console (consoleLive)

inductive ExampleError where
  | failed
  deriving Repr

instance : ToString ExampleError where
  toString
    | .failed => "the example failed"

def typedFailure : Z Unit ExampleError Unit :=
  Z.fail ExampleError.failed

/-- Recover from a typed error with the direct `foldZ` combinator. -/
def errorHandling1a : Z Unit Empty Unit :=
  typedFailure.foldZ
    (fun error =>
      consoleLive.printLine s!"Recovered from an error: {error}")
    (fun _ => consoleLive.printLine "The action succeeded.")

/-- Native `try` syntax in `zdo` handles the typed error channel. -/
def errorHandling1b := zdo
  try
    typedFailure
  catch error =>
    consoleLive.printLine s!"Recovered from an error: {error}"

example : Z Unit Empty Unit := errorHandling1b

/-- `catchAll` is the direct combinator form of the same recovery. -/
def errorHandling1c : Z Unit Empty Unit :=
  typedFailure.catchAll fun error =>
    consoleLive.printLine s!"Recovered from an error: {error}"

/-- `Z.attempt` puts an `IO.Error` in the typed error channel. -/
def ioErrorExample : Z Unit IO.Error Nat :=
  Z.attempt (do
    throw (IO.userError "No such element") : IO Nat)

/-- Recovery removes the handled error from the inferred result type. -/
def errorHandling2a := zdo
  try
    let value <- ioErrorExample.withLabel "ioErrorExample"
    pure (value + 10)
  catch error =>
    let _ <- consoleLive.printLine
      s!"Recovered from an IO.Error: {error}"
    pure 10

example : Z Unit Empty Nat := errorHandling2a

/-- A bare `throw` is a defect. Native `try` can also recover from it. -/
def defectRecovery := zdo
  try
    let _ : Nat <- throw (IO.userError "defect")
    pure 0
  catch error =>
    let _ <- consoleLive.printLine s!"Recovered from a defect: {error}"
    pure 1

example : Z Unit Empty Nat := defectRecovery
