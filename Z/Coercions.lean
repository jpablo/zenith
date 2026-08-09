
/-- A Zenith-specific value conversion used to express variance constraints. -/
class CanConvert (A : Type u) (B : Type v) : Type (max u v) where
  coe : A -> B

infixl:65 " <: " => CanConvert

def impossible {T : Empty -> Type _} (e) : T e :=
  Empty.rec T e


/-! Using `Empty` as bottom and `Unit` as top  -/

instance (priority := low) : A <: A := ⟨id⟩
instance : Empty <: A := ⟨impossible⟩
instance : A <: Unit := ⟨fun _ => ()⟩
