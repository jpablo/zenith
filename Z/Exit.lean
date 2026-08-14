import Z.Cause


/-- The completed result of an effect. -/
inductive Exit (E A: Type): Type
  | success (a: A)
  | failure (failure: Cause E)
  deriving BEq

/-- Render an exit for diagnostics. -/
def Exit.show [bs: ToString E] : Exit E A -> String
  | .success _ => s!"Exit.success (...)"
  | .failure e => s!"Exit.failure ({toString e})"


instance [bs: ToString E]: ToString (Exit E A) where
  toString := Exit.show

/-- A callback that receives one final effect exit. -/
def Observer (E A: Type) : Type := 
  Exit E A -> IO Unit
