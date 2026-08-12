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

lean_exe interpreterBench {
  root := `Benchmarks.Interpreter
}

lean_lib Z
lean_lib Examples

-- Helpers and regression cases for the `tests` executable.
-- `Tests.NotationScope` is deliberately excluded: it is a compile-only guard
-- checked with `lake env lean Tests/NotationScope.lean`.
lean_lib TestsLib where
  roots := #[
    `Tests.Support,
    `Tests.Regressions,
    `Tests.RegressionsProvide,
    `Tests.RegressionsKeyed
  ]


-- meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
-- require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"
