# Regression tests

This folder contains the Zenith regression suite. Run all runtime tests with:

```sh
lake test
```

`Tests.lean` is the executable test runner. It imports each module in this
folder and can run one named test with:

```sh
lake exe tests TEST_NAME
```

Test groups follow the public feature layout:

* `Deferred.lean`, `Queue.lean`, and `Stream.lean` test concurrent values.
* `Scope.lean` tests finalizers, resources, and scoped fibers.
* `Primitives.lean` tests public value types and default services.
* `Regressions*.lean` tests earlier interpreter, layer, keyed-service, and
  provision defects.
* `HEIO.lean` tests the internal layer runtime.
* `Http.lean` tests the optional HTTP adapter.

`NotationScope.lean`, `CoercionScope.lean`, and `CoreImportBoundary.lean` are
compile-time checks. Their expected elaboration messages are part of the test.
