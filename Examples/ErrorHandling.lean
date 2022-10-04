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



-- def act1 : Z Unit Empty Nat := 
--   throwThe IO.Error <| IO.userError "Error 1"

-- def act2 : Z Unit Empty Nat := 
--   throwThe String "Error 2"

-- def errorHandling1d :=
--   tryCatchThe IO.Error
--     (tryCatchThe String act1 fun _ => pure 100)
--     (fun _ => pure 200)


-- def errorHandling1e :=
--   tryCatchThe IO.Error
--     (tryCatchThe String act2 fun _ => pure 100)
--     (fun _ => pure 200)



-- def testCatchAll :=
--   let error := IO.userError "IO failure"
--   let failed := (Z.fail error).catchAll fun e =>
--     Z.attempt (throw e)
  
--   do
--     let exit <- failed.exit
--     return exit == (Exit.failure $ Cause.fail error)



/- 
ZIO.attempt(throw Exception("No such element"))
-/
def ioErrorExample :=
  Z.attempt (ioThrow $ (IO.userError "No such element!!" ))


/- 
ioErrorExample
  .catchAll(e => printLine(s"Recovered from a Throwable: $e"))
  .foldCauseZIO(
    c => printLine(s"This should not be printed: $c") *> ZIO.succeed(1),
    _ => ZIO.succeed(0)
  ).map(_ + 10)
-/
def errorHandling2a :=
  (Z.attempt (ioThrow $ IO.userError "No such element!!")).withLabel "ioErrorExample"
    |>.catchAll
      (fun _ => consoleLive.printLine s!"Recovered from a IO.Error: e")
    |>.foldCauseZ
      (fun _ => (consoleLive.printLine s!"This should not be printed: c") *> pure 1)
      (fun _ => pure 0)
    |>.map (. + 10)


-- /- Using try/catch syntax -/
-- def errorHandling2b := 
--   let z := do
--     try
--       try ioErrorExample
--       catch | e => consoleLive.printLine s!"Recovered from a IO.Error: {e}"
--       return 0
--     catch
--       | c =>
--         consoleLive.printLine s!"This should not be printed: {c}"
--         return 1
--   z.map (. + 10)