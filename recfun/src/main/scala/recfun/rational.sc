
class Rational(x: Int, y: Int) {
  require(y != 0, "denominator must not be zero")
  def this(x: Int) = this(x,1)
  def numer = x
  def denom = y

  def + (that: Rational) =
    new Rational(
      (numer * that.denom + that.numer * denom),
      denom * that.denom)

  override def toString = {
    def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
    val g = gcd(x,y)
    numer / g + "/" + denom / g
  }

  def unary_- = {
    new Rational(
      -1 * numer,
      denom)
  }

  def - (that: Rational) = {
    this + -that
  }

  def < (that: Rational) = numer * that.denom < that.numer*denom

  def max(that: Rational) = {
    if (this < that) that else this
  }
}
val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)
x - y - z
y + y
x < y
x max y
new Rational(1)

val bigX = new Rational(1000000000, 2)
val bigY = new Rational(2000000000, 3)





