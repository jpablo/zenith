import Z.Util

/- TODO: remove duplicates?  -/

def Environment (R: Type u) := R

/--
`IsComponent A B` means that a permutation of `A` is part of `B`.

Symbolic form: A ∣ B.

For example if `A = A₁ × A₂` then 
```
A ∣ (B₁ × A₁ × B₂ × A₂)
A ∣ (A₁ × B₁ × B₂ × A₂)
A ∣ (A₁ × A₂ × B₁ × B₂)
A ∣ (A₂ × A₁ × B₁ × B₂)
... 
```
all are valid.

`Unit` is assumed to be part of any product.

- `get: B -> A` is the projection. 
-/
class IsComponent (A : Type u) (B: Type v) where
  get: B -> A

 
infixl:65 " ∣ " => IsComponent


namespace IsComponent

  def contramap [component: A ∣ B] (f: A -> C): C ∣ B :=
    ⟨get ∘> f⟩


  /-- 
  This will detect permutations of `A × B` in `H × T`.

  If we reach this case then we know that `A ≠ H` (as this is covered by `rule4`).

  So we need to things:
  - `A` is in `T`
  - `B` is in `H × T`

  -/
  instance rule5 [A ∣ T] [B ∣ (H × T)] : (A × B) ∣ (H × T) where
    get | (h, t) => (get t, get (h, t))

  /-- Same heads and different tails but one tail is a component of the other -/
  instance rule4 [B ∣ T] : (A × B) ∣ (A × T) where 
    get | (a, t) => (a, get t)

  /- Either A is in the head or the tail -/
  instance rule3 [A ∣ T] : A ∣ (H × T) := ⟨fun (_, t) => get t⟩
  instance rule2         : A ∣ (A × T) := ⟨fun (a, _) => a⟩

  /-- A few base cases  -/
  instance rule1 :    L ∣ L := ⟨id⟩
  instance rule0 : Unit ∣ L := ⟨fun _ => ()⟩

end IsComponent

namespace IsComponentExamples
  def accept [R1 ∣ R2] (_: Environment R1) (_: Environment R2) := true

  /- Base cases -/
  example: accept () () = true := rfl
  example: accept () ('c', "a", 1) = true := rfl

  /- equal elements -/
  example: accept 'c' 'c' = true := rfl
  example: accept ("a", 1) ("a", 1) = true := rfl
  example: accept ('c', "a", 1) ('c', "a", 1) = true := rfl

  /- more elements provided than required -/
  example: accept 'c' ('c', "a", 1, true) = true := rfl
  example: accept ('c', "a", 1) ('c', "a", 1, true) = true := rfl

  -- negative:
  #check_failure accept ('c', "a", 1, true) ('c', "a", 1)

  /- Other rules -/
  example: accept ("a", 1) ('c', "a", 1) = true := rfl
  example: accept (1, "a") ('c', "a", 1) = true := rfl
  example: accept "a"      ('c', "a", 1) = true := rfl
  example: accept ("a", 'c', 1) ('c', "a", 1) = true := rfl

  #check_failure accept (1, "a", 'c') (true, 1)
  #check_failure accept ('c', "a", 1) []

end IsComponentExamples


namespace Environment 

  def EmptyEnv: Type := Environment Unit
  
  def empty: EmptyEnv := 
    ()

  def EmptyEnv.add (self: EmptyEnv) (a: A) : Environment A := a

  def add (self: Environment T) (a: A) : Environment (A × T) := 
    ⟨a, self⟩ 

  def concat (self: Environment T) (ea: Environment A) : Environment (A × T) := 
    ⟨ea, self⟩ 

  def get (self: Environment T) (A) [component: A ∣ T] : A :=
    component.get self

  def of (a: A) : Environment A := 
    empty.add a

  def map (f: A -> B): Environment A -> Environment B := f

  infixr:67 " ++ " => concat

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

  -- Make it a Type 1 on purpose to verify that an Environment can hold types on different universes

  structure Point: Type 1 := (x y: Nat) deriving Repr, BEq

  def p := Point.mk 1 2


  def e0: EmptyEnv                     := empty
  def e1: Environment String           := e0.add "<secret>"
  def e2: Environment (Point × String) := e1.add p


  example : e1.get String = "<secret>" := rfl
  example : e2.get String = "<secret>" := rfl
  example : e2.get Point  = p          := rfl
  
  #check_failure e2.get Int

end EnvExamples

