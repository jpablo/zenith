import Z.Combinators

structure Console where
  printLine {A: Type} [ToString A] (line : A) : Z Unit Empty Unit
  readLine : Z Unit IO.Error String

namespace Console

  def consoleLive: Console where
    printLine line := 
      Z.succeed' (IO.println line) |>.withLabel s!"📺 println '{line}'"
        
    readLine :=
      Z.attempt (do (<- IO.getStdin).getLine) |>.withLabel s!"📺 getLine"
        
end Console

/-! IO version, since we can't have Z services yet.  -/

structure ConsoleIO where
  printLine (line : String) : IO Unit
  readLine : IO String

namespace ConsoleIO

  def consoleLive: ConsoleIO where
    printLine line := IO.println line
    readLine := do (<- IO.getStdin).getLine

  /-! accessors -/
  
  def printLineZ (line : String) : Z ConsoleIO Empty Unit := do
    (<- Z.service ConsoleIO).printLine line

  def readLineZ : Z ConsoleIO Empty String := do
    (<- .service ConsoleIO).readLine

end ConsoleIO

