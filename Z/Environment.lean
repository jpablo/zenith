

/- TODO: remove duplicates?  -/


def Environment (R: Type u) := R

infixr:67 " *: " => Prod.mk


def t1 := 1 *: "a" *: true


/-- Helper type class to extract a value of type A from Env  -/

private class GetElement (Env) (A) where
  apply: Env -> A


/-- Recursive case  -/
instance [getter: GetElement (Environment T) B] : GetElement (Environment (A × T)) B where
  apply | (_, t) => getter.apply t

/-- Base case: element is the head  -/
instance : GetElement (Environment (A × T)) A where
  apply | (a, _) => a

/-- Special case: just one element  -/
instance : GetElement (Environment A) A where
  apply a := a


/- -------------  -/

/- TODO: Find a better name -/
class Subset (A : Type u) (B: Type v) where
  get: B -> A


infixl:65 " ⊂ " => Subset

def Subset.contramap {C} [inst: A ⊂ B] (f: A -> C): C ⊂ B :=
  ⟨f ∘ inst.get⟩


/-- different tails but head1 is in tail2 and tail1 is subset of tuple2 -/
instance [H₁ ⊂ T₂] [T₁ ⊂ (H₂ × T₂)] : (H₁ × T₁) ⊂ (H₂ × T₂) where
  get | (h₂, t₂) => (Subset.get t₂, Subset.get (h₂, t₂))

/-- Same heads and tail1 is subset of tail2  -/
instance [T₁ ⊂ T₂] : (H × T₁) ⊂ (H × T₂) where 
  get | (h, t₂) => (h, Subset.get t₂)

/- Different heads but head1 is subset of tail -/
instance [H₁ ⊂ T] : H₁ ⊂ (H × T) where
  get | (_, t) => Subset.get t

/-- A few base cases  -/
instance : H ⊂ (H × T) := ⟨fun (h, _) => h⟩
instance : L ⊂ L       := ⟨id⟩
instance : Unit ⊂ L    := ⟨fun _ => ()⟩


def accept [R1 ⊂ R2] (_: Environment R1) (_: Environment R2) := true

/- rule 0 -/
example: accept () () = true := rfl
example: accept () ('c' *: "a" *: 1) = true := rfl


/- rule1: same head -/

-- equal elements
example: accept 'c' 'c' = true := rfl
example: accept ("a" *: 1) ("a" *: 1) = true := rfl
example: accept ('c' *: "a" *: 1) ('c' *: "a" *: 1) = true := rfl


-- more elements provided than required
example: accept 'c' ('c' *: "a" *: 1 *: true) = true := rfl
example: accept ('c' *: "a" *: 1) ('c' *: "a" *: 1 *: true) = true := rfl

-- negative:
#check_failure accept ('c' *: "a" *: 1 *: true) ('c' *: "a" *: 1)


/- rule2: -/
example: accept ("a" *: 1) ('c' *: "a" *: 1) = true := rfl

example: accept (1 *: "a") ('c' *: "a" *: 1) = true := rfl

example: accept "a" ('c' *: "a" *: 1) = true := rfl

example: accept ("a" *: 'c' *: 1) ('c' *: "a" *: 1) = true := rfl

#check_failure accept (1 *: "a" *: 'c') (true *: 1)

#check_failure accept ('c' *: "a" *: 1) []






/- -------------  -/

namespace Environment 

  def EmptyEnv: Type := Environment Unit
  
  def empty: EmptyEnv := 
    ()

  def EmptyEnv.add (self: EmptyEnv) (a: A) : Environment A := a

  def add (self: Environment T) (a: A) : Environment (A × T) := 
    ⟨a, self⟩ 

  def concat (self: Environment T) (ea: Environment A) : Environment (A × T) := 
    ⟨ea, self⟩ 

  def get (self: Environment T) (A) [getter: GetElement (Environment T) A] : A :=
    getter.apply self

  def of (a: A) : Environment A := 
    empty.add a

  def map (f: A -> B): Environment A -> Environment B := f

  infixr:67 " ++ " => concat

end Environment 


-- #check_failure Environment.get [] Char

-- #eval ('c').get Char
-- #eval Environment.get ('c' *: "a" *: 1) Char
-- #eval Environment.get ('c' *: "a" *: 1 *: "b") String
-- #eval Environment.get ('c' *: "a" *: 1) Nat

-- #check_failure Environment.get ('c' *: "a" *: 1) Unit


namespace EnvExamples

  open Environment

  -- Make it a Type 1 on purpose to verify that an Environment can hold types on different universes

  structure Point: Type 1 := (x y: Nat) deriving Repr, BEq

  def p := Point.mk 1 2



  def e0: EmptyEnv                     := empty
  def e1: Environment String           := e0.add "<secret>"
  def e2: Environment (Point × String) := e1.add p

  -- #check_failure e0.get Unit

  -- #check_failure e1.get Unit
  example : e1.get String = "<secret>" := rfl

  -- #check_failure e2.get Unit
  example : e2.get String = "<secret>" := rfl
  example : e2.get Point  = p          := rfl
  
  -- #check_failure e2.get Int

end EnvExamples

