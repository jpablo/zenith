import Z
import Z.Console

open Console (consoleLive)


def errorHandling1a :=
  Z.foldZ (.fail "Failed!")
    (fun e => consoleLive.printLine s!"Recovered from an error: {e}")
    impossible

def errorHandling1b := do
  try
    throw <| IO.userError "Failed!"
  catch
    | e => consoleLive.printLine s!"Recovered from error: {e}"

def errorHandling1c :=
  tryCatch
    (throw <| IO.userError "Failed!")
    (fun e => consoleLive.printLine s!"Recovered from error: {e}" )


def ioErrorExample :=
  Z.attempt <| ioThrow "No such element!!"

def errorHandling2a :=
  Z.attempt (ioThrow "No such element!!") |>.withLabel "ioErrorExample"
    |>.catchAll
      (fun _ => consoleLive.printLine s!"Recovered from a IO.Error: e")
    |>.foldCauseZ
      (fun _ => (consoleLive.printLine s!"This should not be printed: c") *> pure 1)
      (fun _ => pure 0)
    |>.map (. + 10)

