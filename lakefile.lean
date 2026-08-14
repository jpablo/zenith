import Lake
open Lake DSL

package z {
  -- add package configuration options here
}

@[default_target]
lean_exe z {
  root := `Main
}

@[test_driver]
lean_exe tests {
  root := `Tests
  -- Compile the negative elaboration checks without linking their generated
  -- initializers into the runtime executable.
  needs := #[`@/TestsLib]
}

lean_exe githubIssueSync {
  root := `Examples.GithubIssueSyncDemo
}

lean_exe stableServiceKeys {
  root := `Examples.StableServiceKeysDemoMain
}

lean_exe todoReport {
  root := `Examples.TodoReportMain
}

lean_exe scopedResource {
  root := `Examples.ScopedResourceDemo
}

lean_exe queueWorkerPool {
  root := `Examples.QueueWorkerPoolMain
}

lean_exe httpServer {
  root := `Examples.HttpServer
}

lean_exe interpreterBench {
  root := `Benchmarks.Interpreter
}

lean_lib Z
lean_lib Examples

-- Helpers and regression cases for the `tests` executable.
-- `Tests.NotationScope` and `Tests.CoercionScope` carry no runtime tests: they
-- assert what must and must not elaborate, so building them is the check.
lean_lib TestsLib where
  roots := #[
    `Tests.Support,
    `Tests.Regressions,
    `Tests.RegressionsProvide,
    `Tests.RegressionsKeyed,
    `Tests.HEIO,
    `Tests.Deferred,
    `Tests.Queue,
    `Tests.Stream,
    `Tests.Primitives,
    `Tests.Scope,
    `Tests.Http,
    `Tests.NotationScope,
    `Tests.CoercionScope,
    `Examples.KeyedLayerMakeDiagnostics
  ]


-- meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
-- require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
