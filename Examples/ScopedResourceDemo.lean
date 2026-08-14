import Z

/-!
A small runnable example of dynamic resource scopes.
-/

def openConnection : Z Scope Empty String :=
  Z.acquireRelease
    (do
      Z.fromIO <| IO.println "[acquire] open connection"
      pure "connection-1")
    (fun connection =>
      Z.fromIO <| IO.println s!"[release] close {connection}")

def scopedResourceProgram : Z Unit Empty Unit :=
  Z.scoped <| zdo
    let connection ← openConnection
    Z.fromIO <| IO.println s!"[use] query with {connection}"

def main : IO Unit := do
  match ← Z.unsafeRunSync scopedResourceProgram "scoped-resource-demo" with
  | .success () => pure ()
  | .failure cause =>
      throw (IO.userError s!"The scoped resource demo failed: {cause}")
