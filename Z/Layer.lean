import Z.Combinators


inductive Layer : Type -> Type -> Type -> Type 1

  | internal.apply (self : Z A E (Environment B)) : Layer A E B
  
  | internal.fold  
    (self : Layer A E B)
    (failure :       Cause E -> Layer A E₁ C)
    (success : Environment B -> Layer A E₁ C)
                              : Layer A E₁ C

  | internal.fresh (self : Layer A E B) : Layer A E B

  | internal.suspend (self : Unit -> Layer A E B) : Layer A E B

  | internal.to
    (self : Layer A E B)
    (that : Layer B E C)
          : Layer A E C

  | internal.zipWithPar
    (self : Layer A E B)
    (that : Layer A E C)
    (f : Environment B -> Environment C -> Environment D)
  : Layer A E D


def Layer.contramap (f : A₀ -> A) : Layer A E C -> Layer A₀ E C := 
  internal.to <| internal.apply <| Z.environment A |>.contramap f

/-- Simulate contravariant A  -/
instance [inst: A₀ <: A] : (Layer A E C) <: (Layer A₀ E C) := ⟨.contramap inst.coe⟩


namespace Layer

  def suspend (layer : Thunk (Layer A E B)) : Layer A E B :=
    internal.suspend fun _ => layer.get

  def fromEnvironment (effect : Z R E (Environment A)) : Layer R E A :=
    suspend (internal.apply effect)

  def succeed (a : A) : Layer Unit Empty A :=
    fromEnvironment <| Z.succeedNow <| Environment.of a

  def succeedEnvironment (env : Environment A) : Layer Unit Empty A :=
    fromEnvironment <| Z.succeedNow env

  def fromZ (effect : Z R E A) : Layer R E A :=
    fromEnvironment <| effect.map Environment.of

  def failCause (cause : Cause E) : Layer R E A := 
    internal.apply <| .failCause cause

  def foldLayer
    [A₀ <: A]
    (self    : Layer A E B)
    (failure :             E -> Layer A₀ E₁ C) 
    (success : Environment B -> Layer A₀ E₁ C) 
                              : Layer A₀ E₁ C :=
    internal.fold 
      self 
      (fun cause : Cause E => 
        match cause.failureOrCause with
          | .inl e => failure e
          | .inr r => Layer.failCause r
      ) 
      success

  def mapError (f : E -> E₁) (aec : Layer A E C) : Layer A E₁ C := by
    apply internal.fold aec
    case failure => 
      intro ce
      apply failCause
      exact ce.map f
    case success => 
      intro env
      apply fromEnvironment
      apply Z.succeedNow' env

  /-- Simulate covariant E -/
  instance [inst: E <: E₁] : (Layer A E C) <: (Layer A E₁ C) := ⟨mapError inst.coe⟩


  def flatMap [E <: E₁] (self : Layer A E B) (f : Environment B -> Layer A E₁ C) : Layer A E₁ C := 
    foldLayer self (fun e => failCause <| Cause.fail e) f


  def map (self : Layer A E B) (f : Environment B -> Environment C) : Layer A E C := 
    flatMap self (fun env => succeedEnvironment <| f env)



  class FunctionConstructor (In) where
    Out: Type u
    apply: In -> Out

  instance : FunctionConstructor (Unit -> A) where
    Out := Layer Unit Empty A
    apply f := succeed (f ())

  instance {A B: Type}: FunctionConstructor (A -> B) where
    Out := Layer A Empty B
    apply f := fromEnvironment (Z.serviceWith fun a => Environment.of (f a))


  instance : FunctionConstructor (A -> B -> C) where
    Out := Layer (A × B) Empty C
    apply f := fromEnvironment (Z.serviceWith fun ab => Environment.of (f ab.1 ab.2))


  def fromFunction (input: In) [constructor: FunctionConstructor In] : constructor.Out :=
    constructor.apply input

end Layer
