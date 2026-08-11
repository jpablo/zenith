import Examples.TodoReport

open Z

def main (arguments : List String) : IO UInt32 := do
  let effect := (TodoReport.liveApplication arguments).provideEnvironment
    Z.Services.empty
  match <- Z.unsafeRunSync effect "todo-report" with
  | .success _ => pure 0
  | .failure (.fail (.inl error)) =>
      IO.eprintln s!"Configuration error: {error}"
      pure 2
  | .failure (.fail (.inr error)) =>
      IO.eprintln s!"File-system error: {error}"
      pure 1
  | .failure (.die error) =>
      IO.eprintln s!"Unexpected defect: {error}"
      pure 1
  | .failure .interrupt =>
      IO.eprintln "The TODO report was interrupted."
      pure 130
