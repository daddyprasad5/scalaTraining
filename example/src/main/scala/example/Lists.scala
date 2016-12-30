package example

import scala.annotation.tailrec
import scala.annotation.switch


object  Lists {

  /**
   * This method computes the sum of all elements in the list xs. There are
   * multiple techniques that can be used for implementing this method, and
   * you will learn during the class.
   *
   * For this example assignment you can use the following methods in class
   * `List`:
   *
   *  - `xs.isEmpty: Boolean` returns `true` if the list `xs` is empty
   *  - `xs.head: Int` returns the head element of the list `xs`. If the list
   *    is empty an exception is thrown
   *  - `xs.tail: List[Int]` returns the tail of the list `xs`, i.e. the the
   *    list `xs` without its `head` element
   *
   *  ''Hint:'' instead of writing a `for` or `while` loop, think of a recursive
   *  solution.
   *
   * @param xs A list of natural numbers
   * @return The sum of all elements in `xs`
   */
    def sum(xs: List[Int]): Int = {
    @tailrec
    def sumAccumulator(xs: List[Int], accum: Int): Int = {
      (xs: @switch) match {
        case Nil => accum
        case x :: tail => sumAccumulator(tail, accum + x)
      }
    }
    sumAccumulator(xs, 0)
  }

  def sumRaj(xs: List[Int]): Int = {
    @tailrec
    def sumAccumulator(xs: List[Int], accum: Int): Int = {
      if (xs.isEmpty) accum
      else {
        sumAccumulator(xs.tail, accum + xs.head)
      }
    }
    sumAccumulator(xs, 0)
  }
  
  /**
   * This method returns the largest element in a list of integers. If the
   * list `xs` is empty it throws a `java.util.NoSuchElementException`.
   *
   * You can use the same methods of the class `List` as mentioned above.
   *
   * ''Hint:'' Again, think of a recursive solution instead of using looping
   * constructs. You might need to define an auxiliary method.
   *
   * @param xs A list of natural numbers
   * @return The largest element in `xs`
   * @throws java.util.NoSuchElementException if `xs` is an empty list
   */
    def max(xs: List[Int]): Int = {
    @tailrec
      def maxInt(xs: List[Int], max: Int): Int = {
        (xs: @switch) match {
          case Nil => max
          case x :: tail => {
            if (x > max) maxInt(tail, x)
            else maxInt(tail, max)
          }
        }
      }
    if (xs.isEmpty)  throw new java.util.NoSuchElementException("empty list is not allowed")
    maxInt(xs, 0)
  }
  }
