/**
  * Created by btw460 on 3/18/17.
  */

package scalameterTrial

import org.scalameter._

import scala.collection.parallel._

trait Iterator[T] {
  def hasNext: Boolean
  def next(): T
  def foldLeft[S](z: S)(f: (S, T) => S): S = {
    var result = z
    while (hasNext) result = f(result, next())
    result
  }
}

trait Builder[A, Repr] {
  def +=(elem: A): Builder[A, Repr]
  def result: Repr
}

trait Traversable[T] {
  def foreach(f: T => Unit): Unit
  def newBuilder: Builder[T, Traversable[T]]
  def filter(p: T => Boolean): Traversable[T] = {
    val b = newBuilder
    foreach((x) => if (p(x)) b += x)
    b.result
  }
}




object RangeBenchmark
  extends Bench.LocalTime {
  val sizes = Gen.range("size")(300000, 1500000, 300000)

  val ranges = for {
    size <- sizes
  } yield 0 until size

  performance of "Range" in {
    measure method "map" in {
      using(ranges) in {
        r => r.map(_ + 1)
      }
    }
  }
}



object Conversion {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 10,
    Key.exec.maxWarmupRuns -> 20,
    Key.exec.benchRuns -> 20,
    Key.verbose -> true
  )withWarmer(new Warmer.Default)

  val memConfig = config(
    Key.exec.minWarmupRuns -> 0,
    Key.exec.maxWarmupRuns -> 0,
    Key.exec.benchRuns -> 10,
    Key.verbose -> true
  )withWarmer(Warmer.Zero)

  val vector = Vector.fill(1000000)("")
  val list = vector.toList

  def main(args: Array[String]) {
    val listtime = standardConfig measure {
      list.par
    }
    println(s"list conversion time: $listtime ms")

    val vectortime = standardConfig measure {
      vector.par
    }
    println(s"vector conversion time: $vectortime ms")
  }

}


