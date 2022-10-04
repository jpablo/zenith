import Examples
-- import Lean.Data.HashMap


-- def main': IO Unit := taskExample 




def main: IO Unit := do
  -- let z := asyncExample (E := Empty)
  let z := succeedNowExample -- (E := Empty)
  let r <- Z.unsafeRunSync z "main"
  IO.println s!"---- exiting main ----"
  IO.println s!"{r}"
    


-- #check IO.getStdin

-- def main: IO Unit := do
--   let stdin <- IO.getStdin
--   let line <- stdin.getLine
--   IO.println s!"hello: {line}"
--   stdin.flush



-- lake build >& /dev/null; ./build/bin/z

-- find . -name "*.lean" | entr -s 'lake build  >& /dev/null; ./build/bin/z'
-- find . -name "*.lean" | entr -s 'lake build'

-- ./build/bin/z  > out.txt; cat out.txt   | grep dot | sed 's/dot: //g' > out.dot; dot -Tsvg out.dot -o out.svg
