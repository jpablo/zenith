import Examples

def dotFile (name: String) := s!"diagrams/{name}.dot"

def main: IO Unit := do
  Z.unsafeRunSync succeedNowExample        <| dotFile "succeedNowExample"
  Z.unsafeRunSync zipExample               <| dotFile "zipExample"
  Z.unsafeRunSync zipExample2              <| dotFile "zipExample2"
  Z.unsafeRunSync mapExample               <| dotFile "mapExample"
  Z.unsafeRunSync monadExample             <| dotFile "monadExample"
  Z.unsafeRunSync succeedExample           <| dotFile "succeedExample"
  Z.unsafeRunSync attemptExample           <| dotFile "attemptExample"
  Z.unsafeRunSync coercionExample          <| dotFile "coercionExample"
  Z.unsafeRunSync asyncExample (E := Empty)<| dotFile "asyncExample"
  Z.unsafeRunSync forkExample              <| dotFile "forkExample"
  Z.unsafeRunSync stackOverflow            <| dotFile "stackOverflow"
  Z.unsafeRunSync flatMapEx                <| dotFile "flatMapEx"
  Z.unsafeRunSync ensuringExample          <| dotFile "ensuringExample"
  Z.unsafeRunSync uninterruptibleExample   <| dotFile "uninterruptibleExample"
  Z.unsafeRunSync interruptionExample1     <| dotFile "interruptionExample1"
  Z.unsafeRunSync interruptionExample2     <| dotFile "interruptionExample2"
  Z.unsafeRunSync interruptionExample2b    <| dotFile "interruptionExample2b"
  Z.unsafeRunSync interruptionExample3     <| dotFile "interruptionExample3"
  Z.unsafeRunSync uninterruptibleExample1  <| dotFile "uninterruptibleExample1"
  Z.unsafeRunSync uninterruptibleExample2  <| dotFile "uninterruptibleExample2"
  Z.unsafeRunSync envExample1ready         <| dotFile "envExample1ready"
  Z.unsafeRunSync envExample2ready         <| dotFile "envExample2ready"
  Z.unsafeRunSync envExample3              <| dotFile "envExample3"
  Z.unsafeRunSync errorHandling1a          <| dotFile "errorHandling1a"
  Z.unsafeRunSync errorHandling1b          <| dotFile "errorHandling1b"
  Z.unsafeRunSync errorHandling1c          <| dotFile "errorHandling1c"
  Z.unsafeRunSync errorHandling2a          <| dotFile "errorHandling2a"
  println! "---- exiting main ----"
    


-- find . -name "*.lean" | entr -s 'lake build'
-- ./build/bin/z
-- for f in $(find diagrams -name "*.dot"); do echo $f; dot -Tsvg $f -o diagrams/$(basename $f .dot).svg; done
