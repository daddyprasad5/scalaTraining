abstract class Nat {
  def isZero: Boolean
  def predecessor: Nat
  def successor = new Succ(this)
  def +(that: Nat): Nat
  def -(that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new NoSuchElementException
  def -(that: Nat)  = {
    if (that isZero) this
    else throw new NoSuchElementException
  }
  def +(that: Nat) = that
}

class Succ(n: Nat) extends Nat {
  def isZero = false
  def predecessor = n
  def -(that: Nat): Nat = if (that.isZero) this else n - that.predecessor
  def +(that: Nat) = new Succ(n+ that)
}

val n1 = Zero.successor
n1.predecessor.isZero
n1.isZero
n1 + n1 isZero
n1 - Zero

