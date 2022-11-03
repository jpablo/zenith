# Variance

ZIO uses covariance and contravariance a lot to propagate constraints and in general to express certain program invariants.

```Scala
sealed trait ZIO[-R, +E, +A]
```

For example consider the `Sync` case class

```scala
case class Sync[A](trace: Trace, eval: () => A) extends ZIO[Any, Nothing, A]
```

`R = Any` means that this value is ready to be executed as it has no dependencies in the environment.

`E = Nothing` means that this value has no **expected** errors ("failures").

Such a value can be used whenever a `ZIO[R, E, A]` is expected (for arbitrary types `R`, `E`) due to contravariance of `R` and covariance of `E`.

Lean of course has no co/contravariance, which leads to a situation like the following:

Let's say we have two functions

```coq
def foldCauseZ (effect : Z R E A) (errorHandler : Cause E -> Z R E₁ A₁) (next : A -> Z R E₁ A₁)

def fail [ToString E] (userError : E): Z Unit E Empty
```

and we want to implement `sandbox` in terms of them:

```coq
def sandbox [ToString E]: Z R (Cause E) A :=
  self.foldCauseZ (fun e => fail e) pure
```

Unfortunately this doesn't compile:

```
type mismatch
  fail e
has type
  Z Unit (Cause E) Empty : Type 1
but is expected to have type
  Z R (Cause E) A : Type 1
```

There are two options AFAICT:

## Don't fix type parameters

This means having two versions of the same function, for example:

```coq
def succeedNow' (a : A): Z R E A :=
  Z.internal.done (Exit.success a)

def succeedNow (a : A): Z Unit Empty A := 
  Z.succeedNow' a
```

cleary `succeedNow` is preferred, as it provides more information on the resulting value. But the alternative `succeedNow'` will have to be used in contexts where `Unit` and `Empty` don't work.

## Use coercions to simulate variance

Alternatively, one can try to simulate variance by a set of coercions like this:

```coq
infixl:65 " <: " => Coe

def impossible {T : Empty -> Type _} (e) : T e := 
  Empty.rec e

/-! Using `Empty` as bottom and `Unit` as top  -/

instance :     A <: A    := ⟨id⟩
instance : Empty <: A    := ⟨impossible⟩
instance :     A <: Unit := ⟨fun _ => ()⟩
```

This defines `Empty` as a bottom type and `Unit` as a top type.

Armed with this one can simulate variance:

```coq
/-- Simulate contravariant R -/
instance [inst : R₀ <: R₁] : (Z R₁ E A) <: (Z R₀ E A) := ⟨contramap inst.coe⟩
    
/-- Simulate covariant E -/
instance [inst : E₀ <: E] : (Z R E₀ A) <: (Z R E A) := ⟨mapFailure inst.coe⟩

/-- Simulate covariant A -/
instance [inst : A <: B] : (Z R E A) <: (Z R E B) := ⟨map inst.coe⟩
```

After this the previous example `sandbox` compiles fine.

The downside is that extra nodes are introduced in the program structure.  The cost of this depends a bit on the actual implementation of the coercion functions. Right now `contramap`, `mapFailure` and `map` are either primitives or close to primitives.

On the other hand in some cases the conversion needed is `Empty -> A` which is totally fine, as in this case know for sure that this conversion will never be evaluated.

## Least upper bounds

One situation that it's unclear yet how to handle is the following:

```scala
val combinedEnv: ZIO[Int & String, IOException, Unit] = 
  for
    env <- ZIO.environment[Int]
    str <- ZIO.environment[String]
    _ <- Console.printLine((env, str))
  yield ()
```

In this case Scala 3 will infer the `R` type correctly as `Int & String`.

In Zenith right now the whole environment has to be summoned at once:

```coq
def combinedEnv: Z (Nat × String) Empty Unit := do
  let env <- Z.environment (Nat × String)
  consoleLive.printLine (env.get Nat)
  consoleLive.printLine (env.get String)
```

Because if we try to get one type at a time:

```coq
-- Does not compile:
def envExample1: Z (Nat × String) Empty Unit := do
  let nat <- Z.environment Nat
  let str <- Z.environment String
  consoleLive.printLine (env.get Nat)
  consoleLive.printLine (env.get String)
```

The first `Z.environment Nat` fixes the Monad instance to be 

```coq
Z Nat Empty _
```

which is not compatible with 

```coq
 Z (Nat × String) Empty _
```

