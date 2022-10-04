import Z.Util
import Z.Interruption

structure ExecutionDiagram (A: Type) where
  header                : A
  footer                : A
  errorHandler          : Option NodeId -> NodeId -> A
  continue_             : Option NodeId -> NodeId -> A
  interruption          : NodeId -> NodeId -> Nat -> Nat -> A
  currentNode           : String -> String -> NodeId -> Interruption -> Nat -> Nat -> Nat -> String -> A
  done                  : String -> NodeId -> String -> String -> A
  syncTry               : String -> NodeId -> Nat -> A
  onSuccess             : NodeId -> NodeId -> A
  async                 : String -> NodeId -> Nat -> A
  fork                  : FiberId -> NodeId -> NodeId -> Nat -> Nat -> NodeId -> A
  onSuccessAndFailure   : NodeId -> NodeId -> A
  setInterruptStatus    : NodeId -> NodeId -> NodeId -> A
  widenEnv              : NodeId -> NodeId -> A
  provideEnvironment    : String -> String -> String -> String -> A


namespace ExecutionDiagram
  def empty: ExecutionDiagram (IO Unit) where
    header                      := IO.unit
    footer                      := IO.unit
    errorHandler _ _            := IO.unit
    continue_ _ _               := IO.unit
    interruption _ _ _ _        := IO.unit
    currentNode _ _ _ _ _ _ _ _ := IO.unit
    done _ _ _ _                := IO.unit
    syncTry _ _ _               := IO.unit
    onSuccess _ _               := IO.unit
    async _ _ _                 := IO.unit
    fork _ _ _ _ _ _            := IO.unit
    onSuccessAndFailure _ _     := IO.unit
    setInterruptStatus _ _ _    := IO.unit
    widenEnv _ _                := IO.unit
    provideEnvironment _ _ _ _  := IO.unit

end ExecutionDiagram