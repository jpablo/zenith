# Problems

## `IO` is not universe polymorphic

Lean defines `IO` as:

```lean
IO : Type → Type
```

The result type of `IO` must therefore be in `Type`. A Zenith effect is in
`Type 1`:

```lean
Z Unit Empty Unit : Type 1
```

As a result, `IO` cannot contain a `Z` value:

```lean
import Z

#check_failure IO (Z Unit Empty Unit)
```

This restriction concerns values stored in `IO`. It is separate from the
environment restriction below.

## `Z` environments are restricted to `Type`

The current declarations fix all three parameters at `Type`:

```lean
inductive Z : Type → Type → Type → Type 1 where ...
inductive Layer : Type → Type → Type → Type 1 where ...
```

Consider a service record that stores operations as `Z` values:

```lean
structure Issue where
structure Comment where

structure GithubZ : Type 1 where
  getIssues (organization : String) : Z Unit IO.Error (List Issue)
  postComment (issue : Issue) (comment : Comment) : Z Unit IO.Error Unit
```

`GithubZ` is in `Type 1` because its fields contain `Z` values. It cannot be
used directly as the `R` parameter:

```lean
#check_failure Z GithubZ Empty Unit
```

This error comes from the declaration `R : Type`. Support for this service
requires a universe-polymorphic environment parameter throughout `Z`, `Layer`,
and the related runtime types. It does not require `IO` to contain a `Z` value.

## `Type` services can still use `Z` operations

A service in `Type` can be used as an environment when its data is stored in
the record and its `Z` operations are separate definitions:

```lean
structure Github : Type where
  endpoint : String

def Github.getIssues
    (_ : String) : Z Github IO.Error (List Issue) :=
  Z.serviceWith fun _ : Github => []
```

The exact restriction is that a service record in `Type` cannot store
`Z`-valued operations as fields.

The complete runnable example is in [`Problems.lean`](Problems.lean). Run it
from the project root:

```sh
lake env lean docs/Problems.lean
```
