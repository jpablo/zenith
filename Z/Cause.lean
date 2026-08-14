

deriving instance BEq for IO.Error

/-- A typed failure, defect, interruption, or composition of such failures. -/
inductive Cause (E: Type)
  | fail (userError: E)
  | die (ioError: IO.Error)
  | interrupt --(fiberId: FiberId)
  | sequential (left right : Cause E)
  | parallel (left right : Cause E)
  deriving BEq

/-- Render a cause for diagnostics. -/
def Cause.show [ToString E] : Cause E -> String
  | fail e => s!"Cause.fail ({toString e})"
  | die ioe => s!"Cause.die ({toString ioe})"
  | interrupt => "Cause.interrupt"
  | sequential left right =>
      s!"Cause.sequential ({left.show}, {right.show})"
  | parallel left right =>
      s!"Cause.parallel ({left.show}, {right.show})"

instance [ToString E] : ToString (Cause E) := 
  ⟨Cause.show⟩ 
 

/-- Transform typed failures while preserving defects and interruption. -/
def Cause.map (f: E -> E₁) : Cause E -> Cause E₁
  | fail e => fail (f e)
  | die ioe => die ioe
  | interrupt => interrupt
  | sequential left right => sequential (left.map f) (right.map f)
  | parallel left right => parallel (left.map f) (right.map f)


/-- Return the first typed failure contained in a cause, if any. -/
def Cause.failureOption: Cause E -> Option E
  | fail e => some e
  | die _ | interrupt => none
  | sequential left right | parallel left right =>
      match left.failureOption with
      | some error => some error
      | none => right.failureOption

/-- Return a typed failure, or the remaining cause when no typed failure exists. -/
def Cause.failureOrCause: Cause E -> E ⊕ (Cause R)
  | fail e => .inl e
  | die ioe => .inr (die ioe)
  | interrupt => .inr interrupt
  | sequential left right =>
      match left.failureOrCause with
      | .inl error => .inl error
      | .inr mappedLeft =>
          match right.failureOrCause with
          | .inl error => .inl error
          | .inr mappedRight => .inr (sequential mappedLeft mappedRight)
  | parallel left right =>
      match left.failureOrCause with
      | .inl error => .inl error
      | .inr mappedLeft =>
          match right.failureOrCause with
          | .inl error => .inl error
          | .inr mappedRight => .inr (parallel mappedLeft mappedRight)
