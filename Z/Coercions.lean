
infixl:65 " <: " => CoeTC

def impossible {T : Empty -> Type _} (e) : T e :=
  Empty.rec T e


/-! Using `Empty` as bottom and `Unit` as top  -/

instance :     A <: A    := ⟨id⟩
instance : Empty <: A    := ⟨impossible⟩
instance :     A <: Unit := ⟨fun _ => ()⟩
