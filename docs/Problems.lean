import Z

namespace Problems

structure Issue where

-- Standard `IO` still cannot return an effect from `Type 1`.
#check_failure IO (Z Unit Empty Unit)

structure Github : Type 1 where
  getIssues : String -> Z Unit IO.Error (List Issue)

-- The public environment parameter accepts `Github : Type 1`.
#check Z Github IO.Error Unit

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

#check githubLayer
#check program
#check runProgram

end Problems
