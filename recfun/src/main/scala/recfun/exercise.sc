package recfun
import math.abs
/**
  * Created by btw460 on 1/2/17.
  */
object exercise {
  val tolerance = .0001
  def isCloseEnough(x: Double, y: Double): Boolean = {
    abs((x-y)/x)/x < tolerance
  }
  def fixedPoint(f: Double => Double)(firstGuess: Double): Double= {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
  iterate(firstGuess)
  }

  fixedPoint(x=>1 + x/2)(1)
  def averageDamp(f: Double => Double)(x: Double): Double = {
    (x+f(x))/2
  }
  def sqrt(x: Double): Double= {
    fixedPoint(averageDamp(y => x/y))(1)
  }
}





