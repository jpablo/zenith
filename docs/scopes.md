# Dynamic resource scopes

`Scope` gives a dynamic lifetime to resources that are created inside a `Z`
program. It is a normal environment requirement. `Z.scoped` supplies this
requirement and removes it from the returned effect.

```lean
def openConnection : Z Scope Empty Connection :=
  Z.acquireRelease acquireConnection closeConnection

def program : Z Unit Empty Unit :=
  Z.scoped <| zdo
    let connection ← openConnection
    useConnection connection
```

`Z.acquireRelease` runs acquisition and finalizer registration in one
uninterruptible region. It does not register a finalizer if acquisition fails.
After registration, the finalizer runs when the enclosing scope closes.

A scope has these rules:

- It closes after success, typed failure, defect, or interruption.
- It runs finalizers in reverse registration order.
- It runs each finalizer at most once.
- It tries all finalizers, even when one finalizer has a defect.
- It combines multiple finalizer failures as a sequential `Cause` tree.
- A nested scope closes independently from its parent scope.

Use `Z.addFinalizer` when no resource value is needed:

```lean
def registeredCleanup : Z Scope Empty Unit :=
  Z.addFinalizer cleanup
```

## Scoped fibers

`Z.forkScoped` attaches a child fiber to the current scope:

```lean
def runWorker : Z Scope Empty (Fiber Empty Unit) :=
  worker.forkScoped "worker"
```

Forking and finalizer registration form one uninterruptible operation. When
the scope closes, it interrupts the child and waits for the child exit. A
child that completed before scope closure keeps its original exit value.

The public `Scope` value can register a finalizer through
`Scope.addFinalizer`. Only the private owner capability created by `Z.scoped`
can close the scope.

Run the example with:

```bash
lake exe scopedResource
```
