import Lake
open Lake DSL

package z {
  -- add package configuration options here
}

@[defaultTarget]
lean_exe z {
  root := `Main
}

lean_lib Z
lean_lib Examples


-- meta if get_config? env = some "dev" then -- dev is so not everyone has to build it
-- require «doc-gen4» from git "https://github.com/leanprover/doc-gen4" @ "main"

