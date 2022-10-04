import Examples



def main: IO Unit := do
  -- let z := asyncExample (E := Empty)
  let z := succeedNowExample -- (E := Empty)
  let r <- Z.unsafeRunSync z "main"
  IO.println s!"---- exiting main ----"
  IO.println s!"{r}"
    


-- lake build >& /dev/null; ./build/bin/z

-- find . -name "*.lean" | entr -s 'lake build  >& /dev/null; ./build/bin/z'
-- find . -name "*.lean" | entr -s 'lake build'

-- ./build/bin/z  > out.txt; cat out.txt   | grep dot | sed 's/dot: //g' > out.dot; dot -Tsvg out.dot -o out.svg
