val x = new Rational(1, 3)
val y = new Rational(5, 7)
val z = new Rational(3, 2)

x.add(y)

x.add(y).mul(z)

x - y
x - y - z

y.add(y)

val strange = new Rational(1, 0)
strange.add(strange)

class Rational (x: Int, y: Int) {

  require(y != 0, "denominator must be positive")

  private def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  private val g = gcd(x, y)

  def numer = x / g
  def denom = y / g

  def less(that: Rational) =
    numer * that.denom < that.numer * denom

  def max(that: Rational) =
    if (this.less(that)) that else this

  def add(that: Rational) =
    new Rational(
      numer * that.denom + that.numer * denom,
      denom * that.denom
    )

  def neg() =
    new Rational(-numer, denom)

  def sub(that: Rational) = add(that.neg())

  def mul(that: Rational) =
    new Rational(
      numer * that.numer,
      denom * that.denom
    )

  def -(that: Rational) = sub(that)

  override def toString = numer + "/" + denom
}