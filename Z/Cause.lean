

/-- Cause of error -/
inductive Cause (E: Type)
  | fail (userError: E)
  | die (ioError: IO.Error)
  | interrupt --(fiberId: FiberId)

def Cause.show [ToString E] : Cause E -> String
  | .fail e    => s!"Cause.fail ({toString e})"
  | .die ioe   => s!"Cause.die ({toString ioe})"
  | .interrupt => "Cause.interrupt"

instance [ToString E] : ToString (Cause E) := 
  ⟨Cause.show⟩ 
 

def Cause.map (f: E -> E₁) : Cause E -> Cause E₁
  | .fail e    => .fail (f e)
  | .die ioe   => .die ioe
  | .interrupt => .interrupt
