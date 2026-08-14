import Z.Combinators

structure Console where
  printLine {A: Type} [ToString A] (line : A) : Z Unit Empty Unit
  readLine : Z Unit IO.Error String

namespace Console

  def live : Console where
    printLine line := 
      Z.internal.succeed (IO.println line) |>.withLabel s!"📺 println '{line}'"
        
    readLine :=
      Z.attempt (do (<- IO.getStdin).getLine) |>.withLabel s!"📺 getLine"

  /-! Environment accessors. -/

  def printLineZ {A : Type} [ToString A]
      (line : A) : Z Console Empty Unit :=
    Z.serviceWithZ fun console => console.printLine line

  def readLineZ : Z Console IO.Error String :=
    Z.serviceWithZ fun console => console.readLine
        
end Console
