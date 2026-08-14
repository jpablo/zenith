import Z.Combinators

/-- Console operations required by programs that read or write standard I/O. -/
structure Console where
  printLine {A: Type} [ToString A] (line : A) : Z Unit Empty Unit
  readLine : Z Unit IO.Error String

namespace Console

  /-- The live console implementation backed by standard input and output. -/
  def live : Console where
    printLine line := 
      Z.internal.succeed (IO.println line) |>.withLabel s!"📺 println '{line}'"
        
    readLine :=
      Z.attempt (do (<- IO.getStdin).getLine) |>.withLabel s!"📺 getLine"

  /-! Environment accessors. -/

  /-- Print one value through the required `Console` service. -/
  def printLineM {A : Type} [ToString A]
      (line : A) : Z Console Empty Unit :=
    Z.serviceWithM fun console => console.printLine line

  /-- Read one line through the required `Console` service. -/
  def readLineM : Z Console IO.Error String :=
    Z.serviceWithM fun console => console.readLine
        
end Console
