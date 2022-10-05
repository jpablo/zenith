import Z.Coercions
import Z.Fiber
import Z.InterruptStatus
import Z.Environment
import Z.Metadata

/-- Z R E A  -/
inductive Z : Type -> Type -> Type -> Type 1 where
  | private internal.done (exit : Exit E A) (md := mempty) : Z R E A 

  | private internal.sync (io : IO A) (md := mempty) : Z R E A

  | private internal.async (registerCallback : Observer E A -> IO Unit) (md := mempty) : Z R E A

  | private internal.onSuccess 
           (effect : Z R E A) 
        (next : A -> Z R E A₁) (md := mempty) 
                   : Z R E A₁

  | private internal.onSuccessAndFailure
                     (effect : Z R E  A) 
    (errorHandler : Cause E -> Z R E₁ A₁) 
            (next :       A -> Z R E₁ A₁) (md : Metadata) 
                             : Z R E₁ A₁

  | private internal.fork (effect : Z R E A) (name : String) (md : Metadata) : Z R Empty (Fiber E A)

  | private internal.setInterruptStatus (effect : Z R E A) (interruptStatus : InterruptStatus) (md := mempty) : Z R E A

  | private internal.contramap (f : R₀ -> R) (effect : Z R E A) (md := mempty) : Z R₀ E A

  | private internal.currentEnvironment (md := mempty) : Z R Empty (Environment R)

  | private internal.provideEnvironment (effect : Z R E A) (env : Environment R) (md := mempty) : Z Unit E A



/-- An effect that has no dependencies and fails with an `IO.Error`  -/
def ZTask (A : Type) : Type 1 := Z Unit IO.Error A

/-- An effect that can't fail  -/
def URIO (R : Type) (A : Type) : Type 1 := Z R Empty A

def UIO (A : Type) : Type 1 := Z Unit Empty A



namespace Z

  /-! public API  -/

  def updateMetadata (f : Metadata -> Metadata) (self : Z R E A): Z R E A := match self with
    | internal.done e md                    => internal.done e (f md)
    | internal.sync io md                   => internal.sync io (f md)
    | internal.async cb md                  => internal.async cb (f md)
    | internal.onSuccess e n md             => internal.onSuccess e n (f md)
    | internal.fork e m md                  => internal.fork e m (f md)
    | internal.onSuccessAndFailure e h n md => internal.onSuccessAndFailure e h n (f md)
    | internal.setInterruptStatus e i md    => internal.setInterruptStatus e i (f md)
    | internal.contramap g e md             => internal.contramap g e (f md)
    | internal.currentEnvironment md        => internal.currentEnvironment (f md)
    | internal.provideEnvironment e r md    => internal.provideEnvironment e r (f md)

  def showHead : Z R E A -> String
    | internal.done _ _                 => "done"
    | internal.sync _ _                 => "sync"
    | internal.async _ _                => "async"
    | internal.onSuccess _ _ _          => "onSuccess"
    | internal.fork ..                  => "fork"
    | internal.onSuccessAndFailure ..   => "onSuccessAndFailure"
    | internal.setInterruptStatus _ _ _ => "setInterruptStatus"
    | internal.contramap _ _ _          => "widenEnv"
    | internal.currentEnvironment _     => "currentEnvironment"
    | internal.provideEnvironment _ _ _ => "provideEnvironment"

  def metadata : Z R E A -> Metadata
    | internal.done _ md                    => md
    | internal.sync _ md                    => md
    | internal.async _ md                   => md
    | internal.onSuccess _ _ md             => md
    | internal.fork _ _ md                  => md
    | internal.onSuccessAndFailure _ _ _ md => md
    | internal.setInterruptStatus _ _ md    => md
    | internal.contramap _ _ md             => md
    | internal.currentEnvironment md        => md
    | internal.provideEnvironment _ _ md    => md

  def withLabel (self : Z R E A) (label : String) : Z R E A :=
    self.updateMetadata fun md => {md with label := label }

  def succeedNow' (a : A) (md := Metadata.withLabel "succeedNow"): Z R E A :=
    Z.internal.done (Exit.success a) md

  def succeedNow (a : A): Z Unit Empty A := 
    Z.succeedNow' a

  def map (f : A -> B) (self : Z R E A) : (Z R E B) :=
    internal.onSuccess self (f ∘> succeedNow') |>.withLabel "map"

  def contramap (f : R₀ -> R₁) (effect : Z R₁ E A) (md := mempty) : Z R₀ E A := 
    internal.contramap f effect md

  def done' (exit : Exit E A) (md := mempty): Z R E A := 
    internal.done exit md

  def done (exit : Exit E A) (md := mempty): Z Unit E A := 
    done' exit md

  def mapError (f : E₀ -> E) (self : Z R E₀ A) : Z R E A := by
      apply internal.onSuccessAndFailure self
      case md => 
        exact mempty
      case errorHandler => 
        intro e0
        exact done' <| .failure <| e0.map f
      case next => 
        exact .success ∘> done'

  def succeed' (io : IO A) (md := Metadata.withLabel "succeed"): Z R E A := 
    internal.sync io md

  def succeed (io : IO A) (md := Metadata.withLabel "succeed"): Z Unit Empty A := 
    succeed' io md

  def fork (effect : Z R E A) (name : String) (md := mempty) := 
    internal.fork effect name md

  def async (registerCallback : Observer E A -> IO Unit) (md := mempty) : Z R E A := 
    internal.async registerCallback md

  def flatMap (effect : Z R E A) (next : A -> Z R E A₁) (md := Metadata.withLabel "flatMap") := 
    internal.onSuccess effect next md

  def setInterruptStatus (effect : Z R E A) (interruptStatus : InterruptStatus) (md := mempty) := 
    internal.setInterruptStatus effect interruptStatus md

  def foldCauseZ (effect : Z R E A) (errorHandler : Cause E -> Z R E₁ A₁) (next : A -> Z R E₁ A₁) (md := Metadata.withLabel "foldCauseZ") := 
    internal.onSuccessAndFailure effect errorHandler next md

  def environment (R) (md := Metadata.withLabel "environment") : Z R Empty (Environment R) :=
    internal.currentEnvironment md

  def provideEnvironment (effect : Z R E A) (env : Environment R) (md := mempty) : Z Unit E A := 
    internal.provideEnvironment effect env md


  def withIO (io : IO A) (f : A -> Z R E B) : Z R E B :=
    internal.onSuccess (Z.internal.sync io) f

  /- ------------ Co/Contravariance -------------- -/

  /-- Simulate contravariant R -/
  instance [inst : R₀ <: R₁] : (Z R₁ E A) <: (Z R₀ E A) := ⟨contramap inst.coe⟩
    
  /-- Simulate covariant A -/
  instance [inst : A <: B] : (Z R E A) <: (Z R E B) := ⟨map inst.coe⟩

  /-- Simulate covariant E -/
  instance [inst : E₀ <: E] : (Z R E₀ A) <: (Z R E A) := ⟨mapError inst.coe⟩

  def widenError (self : Z R Empty A): Z R E A := self

  /- ------------ Other coercions -------------- -/
  instance : IO A <: Z R E A := ⟨Z.succeed'⟩

end Z


/-- Plays better with the rest of the library in many cases. -/
def ioThrow : IO.Error -> IO Empty := 
  @throw IO.Error IO _ Empty

