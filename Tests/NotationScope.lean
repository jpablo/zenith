import Z

/-!
Regression guard for the `Environment` notations.

`Environment T` is definitionally `T`, so a global `++` for `Environment.concat`
competes with `HAppend` in every module that imports `Z`. This file only uses
plain standard-library `++`; it fails to elaborate while that notation is
global, and elaborates once it is scoped to `Environment`.

It is not part of the runtime suite: the check is that this module compiles.
-/

namespace NotationScope

/-- Unascribed `++` on lists must resolve to `HAppend`, with no ambiguity. -/
def appended := [1, 2] ++ [3]

example : appended = [1, 2, 3] := rfl

/-- The same, through a function, where no expected type can disambiguate. -/
def appendLists (left right : List Nat) := left ++ right

example : appendLists [1] [2] = [1, 2] := rfl

/-- Unascribed `++` on strings must also keep working. -/
def appendedText := "left" ++ "right"

example : appendedText = "leftright" := rfl

end NotationScope
