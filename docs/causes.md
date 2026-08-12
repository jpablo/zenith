# Structured failure causes

`Cause E` records how a Zenith effect stopped. It can contain one failure or
a tree of failures:

```lean
inductive Cause (E : Type)
  | fail (error : E)
  | die (defect : IO.Error)
  | interrupt
  | sequential (left right : Cause E)
  | parallel (left right : Cause E)
```

`sequential left right` means that `left` happened before `right`. For
example, `Z.ensuring` uses it when the protected action and its finalizer both
fail.

`parallel left right` means that independent actions failed concurrently.
Parallel layer construction uses it when both layer branches fail.

`Cause.map` changes every typed failure and keeps the tree shape. The
`failureOption` and `failureOrCause` operations search the left branch first.
This lets `catchAll` recover the first typed failure in a cause tree. A tree
that contains only defects and interruptions stays unhandled.

Resource scopes and layers keep all cleanup failures. They put failures in
the order in which cleanup actions run.
