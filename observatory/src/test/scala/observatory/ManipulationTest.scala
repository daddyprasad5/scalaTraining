package observatory

import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.scalatest.prop.Checkers

@RunWith(classOf[JUnitRunner])
class ManipulationTest extends FunSuite with Checkers {

  test("points should work") {
    val expected = Set(
      (-2,1),
      (-2,0),
      (-2,-1),
      (-1,1),
      (-1,0),
      (-1,-1),
      (0,1),
      (0,0),
      (0,-1),
      (1,1),
      (1,0),
      (1,-1),
      (2,1),
      (2,0),
      (2,-1)

    )

    assert(Manipulation.points(-2, 2, -1, 1).toSet===expected)
  }

  test("gridPoints should work") {
    val temps: Iterable[(Location, Double)] = Iterable((Location(0,0), 200.0))
    val expected:Set[((Int, Int),Int)] = Set (
      ((-2,1), 200),
        ((-2,0), 200),
        ((-2,-1),200),
        ((-1,1),200),
        ((-1,0),200),
        ((-1,-1),200),
        ((0,1),200),
        ((0,0),200),
        ((0,-1),200),
        ((1,1),200),
        ((1,0),200),
        ((1,-1),200),
        ((2,1),200),
        ((2,0),200),
        ((2,-1),200)
    )

    val actual = Manipulation.gridPoints(-2,2,-1,1,temps)
      .map(a => (a._1, a._2.toInt))
      .toSet

    assert(actual === expected)
  }

  test("ave should work") {
    val input = Iterable(
      ("a", 1.0),
      ("b", 2.0),
      ("a", 3.0),
      ("b", 4.0)
    )
    val expected = Set(
      ("a", 2.0),
      ("b", 3.0)
    )

    assert (expected === Manipulation.ave(input).toSet)
  }

  test("average should work") {
    val temps: Iterable[Iterable[(Location, Double)]] = Iterable(Iterable((Location(0,0), 200.0)),Iterable((Location(0,0), 100.0)))
    val pts = Manipulation.points(-89,90,-180,179)

    val expectedGrid = pts.map((_, 150.0)).toMap

    val expectedFunction = (lat: Int, lon: Int) => expectedGrid((lat, lon))

    val actualFunction = Manipulation.average(temps)

    assert( actualFunction(0,0) === expectedFunction(0,0) )
    assert( actualFunction(90,179) === expectedFunction(90,179) )
    assert( actualFunction(-89,-180) === expectedFunction(-89,-180) )

  }

  test("deviation should work") {
    //temperatures: Iterable[(Location, Double)], normals: (Int, Int) => Double): (Int, Int) => Double

    val temps =  Iterable((Location(0,0), 200.0))
    val pts = Manipulation.points(-89,90,-180,179)
    val normalsGrid = pts.map((_, 150.0)).toMap
    val normalsFunction = (lat: Int, lon: Int) => normalsGrid((lat, lon))

    val actualFun = Manipulation.deviation(temps, normalsFunction)

    assert(actualFun(10,10).round === 50)
    assert(actualFun(-80,-150).round === 50)

  }


}