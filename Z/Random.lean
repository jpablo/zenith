import Z.Combinators

structure Random where
  nextNat (lo hi : Nat) : UIO Nat


namespace Random

  def randomLive : Random where
    nextNat lo hi := Z.succeed (IO.rand lo hi) |>.withLabel "nextNat"

  def nextNatZ (lo hi : Nat) : Z Random Empty Nat :=
    Z.serviceWithZ fun random => random.nextNat lo hi

end Random
