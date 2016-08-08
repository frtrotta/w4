abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  override def isZero = true
  override def predecessor = throw new NoSuchElementException
  override def successor = new Succ(Zero)
  override def + (that: Nat) = that
  override def - (that: Nat) = if (that.isZero) Zero else throw new NoSuchElementException
  override def toString: String = "0"
}

class Succ(n: Nat) extends Nat {
  override def isZero = false
  override def predecessor = n
  override def successor = new Succ(this)
  override def + (that: Nat) = if (that.isZero) this else successor + that.predecessor
  override def - (that: Nat) = if (that.isZero) this else predecessor - that.predecessor
  override def toString = n + "|"
}

Zero.isZero
Zero + Zero
new Succ(Zero)
new Succ(Zero) + Zero
new Succ(Zero) + new Succ(Zero)
new Succ(Zero) - new Succ(Zero)
new Succ(Zero) - new Succ(new Succ(Zero))

