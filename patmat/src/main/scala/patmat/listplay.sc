def removeAt[T](n: Int, xs: List[T]): List[T] = {
  n match {
    case 0 => xs.tail
    case a => {
      if (a < 0) xs
      else if (a <= xs.length) xs.head :: removeAt(a-1, xs.tail)
      else xs
    }
  }
}

removeAt(1, List('a', 'b', 'c', 'd')) // List(a, c, d)
removeAt(6, List('a', 'b', 'c', 'd')) // List(a, c, d)
removeAt(-1, List('a', 'b', 'c', 'd')) // List(a, c, d)

def flatten(xs: List[Any]): List[Any] = {
  def iter(xs: List[Any], acc: List[Any]) = {
    if (xs.isEmpty) acc
    else xs.head match {
      case x: List[Any] => flatten(x) ::: flatten(xs.tail)
      case x: Any => x :: flatten(xs.tail)
    }
  }
  iter(xs, List())
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))


def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
  val n = xs.length / 2
  if (n == 0) xs
  else {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (xh :: xt, yh :: yt) =>
          if (ord.lt(xh,yh)) xh :: merge(xt, ys)
          else yh :: merge(yt, xs)
      }
    }
    val (fst, scnd) = xs splitAt n
    merge(msort(fst), msort(scnd))
  }
}

val nums = List(-2, 5, -1, 3, 4)
msort(nums)
val fruits = List("kiwi","orange","apple")
msort(fruits)

def squareList1(xs: List[Int]): List[Int] =
  xs match {
    case Nil => xs
    case y :: ys => (y * y) :: squareList1(xs.tail)
  }

def squareList(xs: List[Int]): List[Int] =
  xs map (x => x*x)

val aList = List(1,2,3,4)
squareList1(aList)
squareList(aList)


def pack[T](xs: List[T]): List[List[T]] = xs match {
  case Nil => Nil
  case x :: xs1 => {
    val (in: List[T], out: List[T]) = xs.span(p => p == x)
    in :: pack(out)
  }
}

pack(List("a", "a", "a", "b", "c", "c", "a"))

def encode[T](xs: List[T]): List[(T, Int)] = {
  pack(xs) map (x => (x(0), x.length))
}

encode(List("a", "a", "a", "b", "c", "c", "a"))

def concat[T](xs: List[T], ys: List[T]): List[T] = {
  (xs foldRight ys)(_ :: _)
}

val anList = List(9, 6, 3)

concat(aList, anList)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  (xs foldRight List[U]())(f(_) :: _)

//val mapped = mapFun[Int,Char](anList, (x => List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j')(x)))

def lengthFun[T](xs: List[T]): Int =
  (xs foldRight 0)((x,y) => 1 + y)

val lengthed = lengthFun(anList)

def queens(n: Int): Set[List[Int]] = {
  def placeQueens(k: Int): Set[List[Int]] = {
    if (k == 0) Set(List())
    else for {
      queens <- placeQueens(k-1)
      col <- 0 until n
      if (isSafe(col, queens))
    } yield col :: queens
  }
  placeQueens(n)
}

def isSafe(col: Int, queens: List[Int]): Boolean = {
  val row = queens.length
  val queesWithRow = (row - 1 to 0 by -1) zip queens
  queesWithRow forall {
    case (r, c) => col != c && math.abs(col - c) != math.abs(row - r)
  }
}

def show(queens: List[Int]) = {
  val lines =
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col, "X ").mkString
  //lines.foreach(println(_))
  "\n" + (lines mkString "\n")
}

queens(4) map show

class Poly (terms0: Map[Int, Double]) {
  def this(bindings: (Int, Double)*) = this(bindings.toMap)
  val terms = terms0 withDefaultValue 0.0
  override def toString =
    (for ((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString " + "

  def + (other: Poly): Poly = {
    //new Poly(terms ++ (other.terms map adjust))
    new Poly((other.terms foldLeft terms)(addTerm))
  }
  //def adjust(term: (Int, Double)): (Int, Double) = (term._1) -> (term._2 + terms(term._1))
  def addTerm(oterms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
    val (exp, coeff1) = term
    oterms + (exp -> (coeff1 + terms(exp)))
  }


}

val p1 = new Poly(1 -> 2.0, 3 -> 4.0, 5 -> 6.2)
val p2 = new Poly(0 -> 3.0, 3 -> 7.0)
p1 + p2
p1.terms(7)


