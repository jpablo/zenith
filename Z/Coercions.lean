
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

/-- Inject one error type into a normalized error-channel sum. -/
class ErrorChannel.CanInject (Source : Type u) (Target : Type v) where
  inject : Source -> Target

namespace ErrorChannel.CanInject

instance (priority := high) [conversion : Source <: Target] :
    ErrorChannel.CanInject Source Target :=
  ⟨conversion.coe⟩

instance [left : ErrorChannel.CanInject Left Target]
    [right : ErrorChannel.CanInject Right Target] :
    ErrorChannel.CanInject (Sum Left Right) Target :=
  ⟨Sum.elim left.inject right.inject⟩

instance [injection : ErrorChannel.CanInject Source Left] :
    ErrorChannel.CanInject Source (Sum Left Right) :=
  ⟨Sum.inl ∘ injection.inject⟩

instance (priority := low) [injection : ErrorChannel.CanInject Source Right] :
    ErrorChannel.CanInject Source (Sum Left Right) :=
  ⟨Sum.inr ∘ injection.inject⟩

end ErrorChannel.CanInject
