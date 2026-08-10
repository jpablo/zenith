
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

/-- A least-upper-bound candidate for two error-channel types. -/
class ErrorChannel.Join.{u, v, w}
    (Left : Type u)
    (Right : Type v)
    (Result : outParam (Type w)) : Type (max u v w) where
  left : Left -> Result
  right : Right -> Result

namespace ErrorChannel.Join

instance (priority := high)
    [conversion : Left <: Right] : ErrorChannel.Join Left Right Right where
  left := conversion.coe
  right := id

instance
    [conversion : Right <: Left] : ErrorChannel.Join Left Right Left where
  left := id
  right := conversion.coe

instance (priority := low) :
    ErrorChannel.Join Left Right (Sum Left Right) where
  left := Sum.inl
  right := Sum.inr

end ErrorChannel.Join
