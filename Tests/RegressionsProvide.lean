import Tests.Support

/-!
Regression tests for `KeyedLayer.provide`.

The cancellation action registered by `provide` ends with `IO.wait waiter`,
while `HEIO.asyncInterrupt` can run that action synchronously from inside the
`waiter` task itself. Add the reproducing test here.
-/

def provideRegressionTests : List (String × IO Unit) := []
