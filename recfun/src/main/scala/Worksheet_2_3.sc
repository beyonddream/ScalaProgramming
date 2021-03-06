import math.abs

object exercise {
  val tolerance = 0.0001

  def isCloneEnough(x: Double, y: Double) =
    abs((x - y) / y) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if(isCloneEnough(guess, next))
        next
      else
        iterate(next)
    }
    iterate(firstGuess)
  }

  fixedPoint(x => 1 + x / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrt(x: Double) =
    fixedPoint(averageDamp(y => x / y))(1)

  sqrt(4)

}