import Z.Combinators

structure Random where
  nextNat (lo hi : Nat) : UIO Nat


namespace Random

  def randomLive : Random where
    nextNat lo hi := Z.succeed (IO.rand lo hi) |>.withLabel "nextNat"

end Random