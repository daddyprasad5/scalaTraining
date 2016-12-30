import scala.annotation.{switch, tailrec}

def factorialRaj(f: Int): Int = {
  @tailrec
  def fac(xs:List[Int], acc: Int): Int = {
    (xs: @switch) match {
      case Nil => acc
      case x :: tail => fac(tail, x*acc)
    }
  }
  if (f == 0) 0 else {
    val xs: List[Int] = ((1 to f)toList).reverse
    fac(xs, 1)
  }
}

factorialRaj(3)
factorialRaj(0)
factorialRaj(-1)
