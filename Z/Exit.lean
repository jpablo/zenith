import Z.Cause


inductive Exit (E A: Type): Type
  | success (a: A)
  | failure (failure: Cause E)
  deriving BEq

def Exit.show [bs: ToString E] : Exit E A -> String
  | .success _ => s!"Exit.success (...)"
  | .failure e => s!"Exit.failure ({toString e})"


instance [bs: ToString E]: ToString (Exit E A) where
  toString := Exit.show

def Observer (E A: Type) : Type := 
  Exit E A -> IO Unit
