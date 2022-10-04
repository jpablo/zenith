import Z.Combinators


inductive Layer : Type -> Type -> Type -> Type 1
  | internal.apply (self: Z RIn E (Environment ROut)) : Layer RIn E ROut
  
  | fold  
    (self: Layer RIn E ROut)
    (failure: Cause E -> Layer RIn E₂ ROut₂)
    (success: Environment ROut -> Layer RIn E₂ ROut₂)
    : Layer RIn E₂ ROut₂

  | fresh (self: Layer RIn E ROut) : Layer RIn E ROut

  | internal.suspend (self: Unit -> Layer RIn E ROut) : Layer RIn E ROut

  | to
    (self: Layer RIn E ROut)
    (that: Layer ROut E ROut₁)
    : Layer RIn E ROut₁

  | zipWithPar
    (self: Layer RIn E ROut)
    (that: Layer RIn E ROut₂)
    (f: Environment ROut -> Environment ROut₂ -> Environment ROut₃)
    : Layer RIn E ROut₃


namespace Layer

  def suspend (layer: Thunk (Layer RIn E ROut)) : Layer RIn E ROut :=
    .internal.suspend fun _ => layer.get

  def fromZEnvironment (effect: Z R E (Environment A)) : Layer R E A :=
    suspend (internal.apply effect)

  def succeed (a: A) : Layer Unit Empty A :=
    fromZEnvironment (Z.succeedNow (Environment.of a))

  def fromZ (effect: Z R E A) : Layer R E A :=
    fromZEnvironment (effect.map Environment.of)


  def foldLayer 
    (self: Layer RIn E ROut)
    (failure: E -> Layer Rin₁ E₁ ROut₂) 
    (success: Environment ROut -> Layer Rin₁ E₁ ROut₂): Layer RIn₁ E₁ Rout₂ := sorry
    -- Layer.fold self failure success
    -- foldCauseLayer(_.failureOrCause.fold(failure, ZLayer.failCause(_)), success)

  def map (self: Layer RIn E ROut) (f: Environment ROut -> Environment ROut₁) : Layer RIn E ROut₁ := 
    sorry

  class FunctionConstructor (In) where
    Out: Type u
    apply: In -> Out


  instance : FunctionConstructor (Unit -> A) where
    Out := Layer Unit Empty A
    apply f := succeed (f ())

  instance {A B: Type}: FunctionConstructor (A -> B) where
    Out := Layer A Empty B
    apply f := fromZEnvironment (Z.serviceWith fun a => Environment.of (f a))


  instance : FunctionConstructor (A -> B -> C) where
    Out := Layer (A × B) Empty C
    apply f := fromZEnvironment (Z.serviceWith fun ab => Environment.of (f ab.1 ab.2))


  def fromFunction (input: In) [constructor: FunctionConstructor In] : constructor.Out :=
    constructor.apply input

end Layer
