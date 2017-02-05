
case class Pos(row: Int, col: Int)

case class Series(ps: List[Pos]) {
  val head = ps.head
  val tail = ps.tail
  def :: (p: Pos) = p :: ps
}
case class Mat(m: Map[Pos, Int]) {
  def neighbors (p: Pos): List[Pos]  = {
    val mRow = m.map(_._1.row).max
    val mCol = m.map(_._1.col).max
    (for {
      r <- -1 to 1
      c <- -1 to 1
      if math.abs(r) != math.abs(c)
      if r + p.row >= 0
      if c + p.col >= 0
      if (r + p.row) <= mRow
      if (c + p.col) <= mCol
    } yield new Pos(p.row + r, p.col + c)).toList
  }
  def goodNeighbors(p: Pos, test: (Int, Int) => Boolean ) : List[Pos] =
    for {
      n <- neighbors(p)
      if test(m(p),m(n))
    } yield n

  def extend(s: Series, test: (Int, Int) => Boolean): List[Series] = {
    for (
      n <- this.goodNeighbors(s.head, test)
    ) yield new Series(n :: s)
  }

  def getAllSeries(test: (Int, Int) => Boolean): List[Series] = {
    def iter(in: List[Series], acc: List[Series]): List[Series] = {
      if (in.isEmpty) acc
      else {
        val extensions = extend(in.head, test)
        val nextAcc = if (extensions.isEmpty) in.head :: acc else acc
        val nextIn = if (extensions.isEmpty) in.tail else extensions ::: in.tail
        iter(nextIn, nextAcc)
      }
    }
    val posAsSeries = (for (p <- m) yield new Series(List(p._1))).toList
    iter(posAsSeries, List[Series]())
  }

  def getLongestSeries(test: (Int, Int) => Boolean): List[Series] = {

    def f(x: List[Series], y: Series): List[Series] = {
      if (x.isEmpty) List(y)
      else if (x.head.ps.length > y.ps.length) x
      else if (x.head.ps.length < y.ps.length) List(y)
      else y :: x
    }

    getAllSeries(test).foldLeft(List[Series]())(f)
  }

  def seriesVals (s: Series): List[Int] = {
    s.ps.map(mat1.m(_))
  }

}

def mat1 = {
  val m: Map[Pos, Int] = Map(
    (new Pos(0,0) -> 1),
    (new Pos(0,1) -> 2),
    (new Pos(0,2) -> 3),
    (new Pos(1,0) -> 6),
    (new Pos(1,1) -> 5),
    (new Pos(1,2) -> 6),
    (new Pos(2,0) -> 7),
    (new Pos(2,1) -> 8),
    (new Pos(2,2) -> 9)
  )
  new Mat(m)
}

val p = new Pos(1,1)
val s = new Series (
  List(
    new Pos(0,1),
    new Pos(0,0)
  )
)
mat1.m(p) + 1
mat1.neighbors(p)
def test(p: Int, neighbor: Int) = 3+p == neighbor
mat1.goodNeighbors(p,test).map(mat1.m(_))
mat1.extend(s, test)
val allSeries: List[Series] = mat1.getAllSeries(test)

for {
  s: Series <- allSeries
} yield mat1.seriesVals(s)

for (s <- mat1.getLongestSeries(test)) yield mat1.seriesVals(s)













