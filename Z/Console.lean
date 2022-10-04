import Z.Combinators


structure Console where
  printLine {A: Type} [ToString A] (line: A) : Z Unit Empty Unit
  readLine : Z Unit IO.Error String



namespace Console

  def consoleLive: Console where
    printLine line := 
      Z.succeed' (IO.println line) |>.withLabel s!"📺 println '{line}'"
        
    readLine :=
      Z.attempt (do (<- IO.getStdin).getLine) |>.withLabel s!"📺 getLine"
        
end Console


-- #check Console.printLine