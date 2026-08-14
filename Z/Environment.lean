import Z.Coercions

/-- An environment has the runtime representation of its service type. -/
abbrev Environment (R : Type u) := R

namespace Environment

/-!
`Extract Required Available Remaining` is private evidence used to build
ordinary product-environment projections. It consumes each selected product
position once, which prevents a duplicate requirement from reusing one value.
-/
private class Extract
    (Required : Type u)
    (Available : Type v)
    (Remaining : outParam (Type w)) where
  extract : Available -> Required × Remaining

namespace Extract

private instance (priority := high) head : Extract A (A × T) T := ⟨id⟩

private instance skip [tail : Extract A T R] : Extract A (H × T) (H × R) where
  extract environment :=
    match environment with
    | (head, tailValue) =>
        match tail.extract tailValue with
        | (value, remaining) => (value, (head, remaining))

private instance (priority := low) last : Extract A A Unit := ⟨(·, ())⟩

private instance (priority := low) unit : Extract Unit Available Available :=
  ⟨fun available => ((), available)⟩

/-- Select a product requirement by consuming its two parts in sequence. -/
private instance pair
    [left : Extract A Available Remaining]
    [right : Extract B Remaining Final] :
    Extract (A × B) Available Final where
  extract available :=
    let (leftValue, remaining) := left.extract available
    let (rightValue, final) := right.extract remaining
    ((leftValue, rightValue), final)

end Extract

  /--
  `CanProvide Available Required` supplies a required environment from the
  complete environment that is available to an effect.
  -/
  class CanProvide (Available : Type u) (Required : Type v) where
    provide : Available -> Required

  namespace CanProvide

    /-- Derive a provided environment by mapping the current required value. -/
    @[reducible] def map
        (self : CanProvide Available Required)
        (f : Required -> Provided) : CanProvide Available Provided :=
      ⟨fun available => f (self.provide available)⟩

    instance (priority := high)
        [projection : Extract Required Available Remaining] :
        CanProvide Available Required :=
      ⟨fun available => (projection.extract available).1⟩

    instance (priority := low) [conversion : Available <: Required] :
        CanProvide Available Required :=
      ⟨conversion.coe⟩

  end CanProvide

  /--
  `Meet Left Right Result` combines two environment requirements.

  If one side already provides the other, `Result` is that side. Otherwise,
  `Result` is their product.
  -/
  class Meet.{u, v, w}
      (Left : Type u)
      (Right : Type v)
      (Result : outParam (Type w)) : Type (max u v w) where
    left : Result -> Left
    right : Result -> Right

  namespace Meet

    /-- Build a selected meet from projections of one known result type. -/
    @[reducible] def ofCanProvide
        {Left : Type u}
        {Right : Type v}
        {Result : Type w}
        [leftProvider : CanProvide Result Left]
        [rightProvider : CanProvide Result Right] :
        Meet Left Right Result where
      left := leftProvider.provide
      right := rightProvider.provide

    instance (priority := high)
        [provider : CanProvide Right Left] : Meet Left Right Right where
      left := provider.provide
      right := id

    instance
        [provider : CanProvide Left Right] : Meet Left Right Left where
      left := id
      right := provider.provide

    instance (priority := low) : Meet Left Right (Left × Right) where
      left := Prod.fst
      right := Prod.snd

  end Meet

  /-- The empty ordinary environment. -/
  def EmptyEnv: Type := Environment Unit
  
  /-- Construct the empty ordinary environment. -/
  def empty: EmptyEnv := 
    ()

  /-- Add the first service to an empty environment. -/
  def EmptyEnv.add (_ : EmptyEnv) (a: A) : Environment A := a

  /-- Prepend one service to an ordinary product environment. -/
  def add (self: Environment T) (a: A) : Environment (A × T) := 
    ⟨a, self⟩ 

  /-- Prefix `self` with the services in `ea`. -/
  def concat (self: Environment T) (ea: Environment A) : Environment (A × T) := 
    ⟨ea, self⟩ 

  /-- Project one required environment from an available environment. -/
  def get (self: Environment T) (A) [provider: CanProvide T A] : A :=
    provider.provide self

  /-- Construct an ordinary environment with one service. -/
  def of (a: A) : Environment A := 
    empty.add a

  /-- Transform the runtime representation of an environment. -/
  def map (f: A -> B): Environment A -> Environment B := f

  /--
  `Environment T` is definitionally `T`, so this competes with `HAppend` for
  every value. It stays scoped to keep `++` unambiguous for importers.
  -/
  scoped infixr:67 " ++ " => concat

end Environment 

namespace EnvExamples
  open Environment


  example : get 'c'               Char = 'c' := rfl
  example : get ('c', "a", 1) Char = 'c' := rfl
  example : get ('c', "a", 1)  Nat = 1   := rfl
  example : get ('c', "a", 1) Unit = ()  := rfl

  example : get ('c', "a", 1, "b") String = "a" := rfl

  #check_failure get ('c', "a", 1) Int

  -- Order does not matter
  example : get ('c', "a", 1, "b") (String × Nat) = ("a", 1) := rfl
  example : get ('c', "a", 1, "b") (Nat × String) = (1, "a") := rfl

  -- Note that only the first String "a" is picked up.
  example : get ('c', "a", 1, "b") (Char × String) = ('c', "a") := rfl

  -- Two requirements of the same type come from two different positions.
  example : get ('c', "a", 1, "b") (String × String) = ("a", "b") := rfl
  example : get ('c', "a", "b", 1) (String × String) = ("a", "b") := rfl

  -- So each of them needs a position of its own.
  #check_failure get ('c', "a", 1) (String × String)

  -- Make it a Type 1 on purpose to verify that an Environment can hold types on different universes

  structure Point : Type 1 where
    x : Nat
    y : Nat
    deriving Repr, BEq

  def p := Point.mk 1 2


  def e0: EmptyEnv                     := empty
  def e1: Environment String           := e0.add "<secret>"
  def e2: Environment (Point × String) := e1.add p


  example : e1.get String = "<secret>" := rfl
  example : e2.get String = "<secret>" := rfl
  example : e2.get Point  = p          := rfl
  
  #check_failure e2.get Int

end EnvExamples
