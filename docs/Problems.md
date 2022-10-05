# Problems

## `IO` is not universe polymorphic

Right now `IO` is defined in Lean as a function `Type -> Type`.

On the other hand, the main data type `Z` and also the `Layer` data types live in `Type 1`, which makes it impossible to    write:

```coq
IO (Z R E A)
```

and in general it forces the type parameters `R E A` to all live in  `Type`.

```coq
inductive Z : Type -> Type -> Type -> Type 1 where ...

inductive Layer : Type -> Type -> Type -> Type 1 where ...
```

Consider the two service definitions below:

```coq
structure Github: Type where
  getIssues (organization : String) : IO (List Issue)
  postComment (issue : Issue) (comment : Comment) : IO Unit

structure GithubZ: Type 1 where
  getIssues (organization : String) : Z Unit IO.Error (List Issue)
  postComment (issue : Issue) (comment : Comment) : Z Unit IO.Error Unit

```

(I've added the type explicitly to highlight the point)

`Github` has `Type`, and so it can be used as part of the environment:

```coq
def z : Z Github E A := ...
```

`GithubZ` on the other hand cannot:

```coq
-- doesn't compile:
-- def z : Z GithubZ E A := ...
```

Because the `R` parameter has `Type` and `GithubZ` has `Type 1`.

As a result, only non-Z services can be used at the moment as part of the environment.











