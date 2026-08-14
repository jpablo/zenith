import Z

/-!
Regression guard for the `IO` to `Z` coercion.

A raw `IO` action carries no typed errors, only defects: a throw becomes
`Cause.die`, which `catchAll`/`foldM`/`tryCatch` never observe. Lifting one
into a typed `IO.Error` channel therefore promises recovery that cannot
happen, so that coercion has to be rejected and `Z.attempt` used instead.

It is not part of the runtime suite: the check is that this module compiles.
-/

namespace CoercionScope

private def failing : IO Nat := throw (IO.userError "boom")

-- A raw `IO` action must not satisfy a typed `IO.Error` channel.
#check_failure (failing : Z Unit IO.Error Nat)

-- Lifting into the defect-only channel stays available.
example : Z Unit Empty Nat := failing

-- `Z.attempt` is the supported way to expose an `IO.Error` as a typed error.
example : Z Unit IO.Error Nat := Z.attempt failing

end CoercionScope
