import Z.Combinators

structure Random where
  nextNat (lo hi : Nat) : UIO Nat


namespace Random

  /--
  Generator state for `live`.

  `IO.rand` reads, advances, and writes the process-global `IO.stdGenRef` in
  three separate steps, so concurrent fibers can draw the same value and lose
  a state advance. This reference is only ever updated atomically.
  -/
  private initialize generator : IO.Ref StdGen ←
    IO.mkRef (mkStdGen (← IO.rand 0 UInt64.size))

  private def nextNatLive (lo hi : Nat) : IO Nat :=
    generator.modifyGet fun state => randNat state lo hi

  def live : Random where
    nextNat lo hi := Z.fromIO (nextNatLive lo hi) |>.withLabel "nextNat"

  def nextNatZ (lo hi : Nat) : Z Random Empty Nat :=
    Z.serviceWithZ fun random => random.nextNat lo hi

end Random
