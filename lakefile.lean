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

-- Optional integrations build on the core `Z` library but are not re-exported
-- by `import Z`.
lean_lib ZenithHttp where
  roots := #[`Zenith.Http]

lean_lib ZenithDebug where
  roots := #[`Zenith.Debug]

lean_lib ZenithServices where
  roots := #[`Zenith.Services]

lean_lib ZenithFormalization where
  roots := #[
    `Zenith.Formalization.CoreLaws,
    `Zenith.Formalization.ServiceKeyLaws,
    `Zenith.Formalization.TypeAlgebra,
    `Zenith.Formalization.ServiceRowConnection,
    `Zenith.Formalization.ErrorShape,
    `Zenith.Formalization.VarianceLaws
  ]

-- Helpers and regression cases for the `tests` executable.
-- Some roots carry no runtime tests. They assert what must and must not
-- elaborate, so compiling them is the check.
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
    `Tests.CoreImportBoundary,
    `Tests.Http,
    `Tests.IntersectionTypes,
    `Tests.NotationScope,
    `Tests.CoercionScope,
    `Tests.Variance,
    `Examples.KeyedLayerMakeDiagnostics
  ]


-- meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
-- require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
