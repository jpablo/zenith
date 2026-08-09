import Z

namespace Problems

structure Issue where
structure Comment where

-- `IO` cannot contain a value from `Type 1`.
#check_failure IO (Z Unit Empty Unit)

-- A service that stores `Z` operations must live in `Type 1`.
structure GithubZ : Type 1 where
  getIssues (organization : String) : Z Unit IO.Error (List Issue)
  postComment (issue : Issue) (comment : Comment) : Z Unit IO.Error Unit

-- The current `Z` environment parameter accepts only values from `Type`.
#check_failure Z GithubZ Empty Unit

-- A service can remain in `Type` when its `Z` operations are definitions.
structure Github : Type where
  endpoint : String

def Github.getIssues
    (_ : String) : Z Github IO.Error (List Issue) :=
  Z.serviceWith fun _ : Github => []

end Problems
