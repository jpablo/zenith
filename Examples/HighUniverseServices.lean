import Z

/-!
A compile-time example of a high-universe service and the layer that provides
it. The accompanying explanation is in `docs/Problems.md`.
-/

namespace Examples.HighUniverseServices

structure Issue where

-- Standard `IO` still cannot return an effect from `Type 1`.

structure Github : Type 1 where
  getIssues : String -> Z Unit IO.Error (List Issue)

-- The public environment parameter accepts `Github : Type 1`.

def program : Z Github IO.Error (List Issue) :=
  Z.serviceWithM fun github =>
    github.getIssues "lean"

def githubLayer : Layer Unit IO.Error Github :=
  Layer.fromBuild fun _ =>
    pure {
      getIssues := fun _ => Z.succeed ([] : List Issue)
    }

def runProgram : IO (Exit IO.Error (List Issue)) :=
  githubLayer.run () program

end Examples.HighUniverseServices
